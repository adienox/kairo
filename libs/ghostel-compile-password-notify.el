;;; ghostel-compile-password-notify.el --- Notify on sudo prompts in ghostel-compile -*- lexical-binding: t; -*-

;; `compilation-filter-hook' won't fire here: while `ghostel-compile'
;; is running, the buffer is a live `ghostel-mode' terminal, not
;; comint-derived.  Instead of polling the buffer text (which may not
;; be rendered while the buffer is hidden), this wraps the process
;; filter and inspects raw PTY output as it arrives.  That works the
;; same for visible and background buffers.

(require 'notifications)

(defcustom +config/compile-watch-ntfy-url nil
  "Full ntfy topic URL to POST to, e.g. \"https://ntfy.sh/my-topic\"
or your self-hosted instance.  Set to nil to skip ntfy entirely."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'ghostel)

(defconst +config/--compile-watch-regexp
  "\\[sudo\\] password for \\|[\n\r]Password: ?\\|\\`Password: ?"
  "Regexp matching a sudo/password prompt in raw output.")

(defconst +config/--compile-watch-tail-length 64
  "Chars of previous output kept so a prompt split across chunks still matches.")

(defvar-local +config/--compile-watch-notification-id nil
  "D-Bus id of the pending sudo notification for this buffer.
Buffer-local so each ghostel/compile buffer tracks its own prompt;
nil when no notification is currently showing.")

;; The ntfy "Enter Password" button is an `http' action: tapping it makes
;; the ntfy app POST to a one-off reply topic.  Emacs listens on that
;; topic with a streaming curl while the prompt is pending, so no inbound
;; connection to this machine is needed.  The topic name carries a random
;; token, so only the recipient of the notification can trigger it.

(defconst +config/--compile-watch-ntfy-listen-timeout 600
  "Seconds to keep listening for the ntfy button press before giving up.")

(defvar-local +config/--compile-watch-ntfy-listener nil
  "Streaming curl process waiting for the ntfy action button in this buffer.")

(defun +config/--compile-watch-ntfy-stop-listener (&optional buf)
  "Kill the ntfy reply listener belonging to BUF (default: current buffer)."
  (let ((buf (or buf (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (process-live-p +config/--compile-watch-ntfy-listener)
          (set-process-sentinel +config/--compile-watch-ntfy-listener #'ignore)
          (delete-process +config/--compile-watch-ntfy-listener))
        (setq +config/--compile-watch-ntfy-listener nil)))))

(defun +config/--compile-watch-ntfy-listen (buf reply-url)
  "Listen on REPLY-URL; when the button is pressed, send the password in BUF."
  (+config/--compile-watch-ntfy-stop-listener buf)
  (let* ((pending "")
         (proc (start-process "compile-watch-ntfy-listen" nil
                              "curl" "-s" "-N"
                              "--max-time"
                              (number-to-string
                               +config/--compile-watch-ntfy-listen-timeout)
                              (concat reply-url "/raw"))))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter
     proc
     (lambda (p output)
       ;; /raw emits one line per message; blank lines are keepalives.
       (setq pending (concat pending output))
       (when (string-match-p "[^ \t\r\n]" pending)
         (setq pending "")
         (delete-process p)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq +config/--compile-watch-ntfy-listener nil)
             (+config/sudo-send-password))))))
    (with-current-buffer buf
      (setq +config/--compile-watch-ntfy-listener proc))))

(defun +config/--compile-watch-ntfy (message &optional buf)
  "Async POST MESSAGE to `+config/compile-watch-ntfy-url' via curl.
When BUF is given, attach an \"Enter Password\" action button that sends
the sudo password into BUF when tapped."
  (when +config/compile-watch-ntfy-url
    (let* ((reply-url
            (and buf
                 (format "%s-reply-%s" +config/compile-watch-ntfy-url
                         (substring (secure-hash 'sha256
                                                 (format "%s%s%s" (random t)
                                                         (float-time)
                                                         (emacs-pid)))
                                    0 24))))
           (args (append
                  (list "curl" "-s"
                        "-H" "Title: Compile waiting on sudo"
                        "-H" "Priority: high")
                  (when reply-url
                    (list "-H"
                          (format "Actions: http, Enter Password, %s, method=POST, body=enter-password, clear=true"
                                  reply-url)))
                  (list "-d" message +config/compile-watch-ntfy-url))))
      ;; Start listening before the notification goes out.
      (when reply-url
        (+config/--compile-watch-ntfy-listen buf reply-url))
      (apply #'start-process "compile-watch-ntfy" nil args))))

(defun +config/compile-watch-dismiss-notification (&optional buf)
  "Dismiss the pending sudo notification associated with BUF.
BUF defaults to the current buffer.  Safe to call when no
notification is pending.  When BUF has no pending notification
(e.g. the command was invoked from a minibuffer or another
window), fall back to the most recently created one in any
buffer so a stray call still cleans up."
  (let ((target (or buf (current-buffer)))
        id)
    (with-current-buffer target
      (setq id +config/--compile-watch-notification-id))
    (unless (integerp id)
      (catch 'found
        (dolist (b (buffer-list))
          (let ((candidate (buffer-local-value
                            '+config/--compile-watch-notification-id b)))
            (when (and (buffer-live-p b) (integerp candidate))
              (setq target b
                    id candidate)
              (throw 'found nil))))))
    ;; The ntfy listener is tied to the same prompt; stop it as well.
    (+config/--compile-watch-ntfy-stop-listener target)
    (when (integerp id)
      (ignore-errors (notifications-close-notification id))
      (when (buffer-live-p target)
        (with-current-buffer target
          (setq +config/--compile-watch-notification-id nil))))))

(defun +config/--compile-watch-notify (buf)
  "Notify about the sudo prompt in BUF and remember the notification id."
  (with-current-buffer buf
    ;; Replace rather than stack if a stale notification is still up.
    (+config/compile-watch-dismiss-notification)
    (setq +config/--compile-watch-notification-id
          (notifications-notify
           :title "Compile waiting on sudo"
           :urgency 'critical
           :body (format "Enter your password in %s buffer." (buffer-name buf))
           :actions '("enter-password" "Enter Password")
           :on-action
           (lambda (_notification _action)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (+config/sudo-send-password))))
           ;; Forget the id if the user (or the server) closes it.
           :on-close
           (lambda (id &rest _)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (eql +config/--compile-watch-notification-id id)
                   (setq +config/--compile-watch-notification-id nil))))))))
  (+config/--compile-watch-ntfy
   (format "Enter your password in %s buffer." (buffer-name buf))
   buf))

(defun +config/--compile-watch-make-filter (buf)
  "Return a :before filter function that watches output for BUF."
  (let ((tail ""))
    (lambda (_proc output)
      (condition-case err
          (let ((text (concat tail output)))
            (if (string-match-p +config/--compile-watch-regexp text)
                (progn
                  ;; Drop the tail so the same prompt can't fire twice.
                  (setq tail "")
                  (+config/--compile-watch-notify buf))
              (setq tail (substring text
                                    (max 0 (- (length text)
                                              +config/--compile-watch-tail-length))))))
        (error (message "compile-watch: %S" err))))))

(defun +config/--compile-watch-install (buf proc)
  (unless (process-get proc 'compile-watch-installed)
    (process-put proc 'compile-watch-installed t)
    (add-function :before (process-filter proc)
                  (+config/--compile-watch-make-filter buf))))

;;;###autoload
(defun +config/compile-watch-start (&optional buf attempts)
  "Watch BUF (default: current buffer) for a sudo prompt.
Retries briefly if the buffer's process hasn't started yet."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (attempts (or attempts 20)))
    (when (buffer-live-p buf)
      (let ((proc (get-buffer-process buf)))
        (cond
         (proc (+config/--compile-watch-install buf proc))
         ((> attempts 0)
          (run-with-timer 0.1 nil #'+config/compile-watch-start
                          buf (1- attempts)))
         (t (message "compile-watch: no process found for %s"
                     (buffer-name buf))))))))

(defun +config/--compile-watch-mode-hook ()
  "Start the watcher when `ghostel-compile-toggle-mode' turns on in this buffer."
  (when (bound-and-true-p ghostel-compile-toggle-mode)
    (+config/compile-watch-start (current-buffer))))

(add-hook 'ghostel-compile-toggle-mode-hook #'+config/--compile-watch-mode-hook)

;; When ghostel finishes obtaining a password - whether from an
;; `auth-source' backend or from the interactive `read-passwd' fallback -
;; the prompt is answered, so pull the notification back down.
(defun +config/--compile-watch-dismiss-after-password (&rest _)
  "Dismiss this buffer's sudo notification once a password was supplied."
  (+config/compile-watch-dismiss-notification))

(with-eval-after-load 'ghostel
  ;; Guard so reloading this library doesn't stack duplicate advice.
  (unless (advice-member-p #'+config/--compile-watch-dismiss-after-password
                           'ghostel--prompt-password)
    (advice-add 'ghostel--prompt-password :after
                #'+config/--compile-watch-dismiss-after-password)))

;;;###autoload
(defun +config/sudo-send-password (&rest _)
  "Fetch sudo password from auth-source and send it via ghostel."
  (interactive)
  (let* ((password (auth-source-pick-first-password
                    :host (system-name)
                    :user (user-login-name)
                    :port "sudo")))
    (when password
      (ghostel-send-string password)
      (ghostel-send-key "return")
      (+config/compile-watch-dismiss-notification))))

(provide 'ghostel-compile-password-notify)
;;; ghostel-compile-password-notify.el ends here

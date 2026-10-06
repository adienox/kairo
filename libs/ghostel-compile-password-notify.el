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

(defun +config/--compile-watch-ntfy (message)
  "Async POST MESSAGE to `+config/compile-watch-ntfy-url' via curl."
  (when +config/compile-watch-ntfy-url
    (start-process "compile-watch-ntfy" nil
                   "curl" "-s"
                   "-H" "Title: Compile waiting on sudo"
                   "-H" "Priority: high"
                   "-d" message
                   +config/compile-watch-ntfy-url)))

(defun +config/--compile-watch-notify (buf)
  (notifications-notify
   :title "Compile waiting on sudo"
   :body (format "Enter your password in %s buffer." (buffer-name buf)))
  (+config/--compile-watch-ntfy
   (format "Compile waiting on sudo password (%s)" (buffer-name buf))))

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

(provide 'ghostel-compile-password-notify)
;;; ghostel-compile-password-notify.el ends here

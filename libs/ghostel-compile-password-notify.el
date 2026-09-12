;;; ghostel-compile-password-notify.el --- Notify on sudo prompts in ghostel-compile -*- lexical-binding: t; -*-

;; `compilation-filter-hook' won't fire here: while `ghostel-compile'
;; is running, the buffer is a live `ghostel-mode' terminal (native
;; PTY, VT-parsed), not comint-derived. It only becomes
;; `compilation-mode'-derived after the process exits, by which point
;; there's nothing left to catch. So this uses a timer instead,
;; tracking a manual "last checked" position the same way
;; `compilation-filter-start' would.

(require 'notifications)

(defcustom +config/compile-watch-ntfy-url nil
  "Full ntfy topic URL to POST to, e.g. \"https://ntfy.sh/my-topic\"
or your self-hosted instance. Set to nil to skip ntfy entirely."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'ghostel)

(defun +config/--compile-watch-ntfy (message)
  "Async POST MESSAGE to `+config/compile-watch-ntfy-url' via curl."
  (when +config/compile-watch-ntfy-url
    (start-process "compile-watch-ntfy" nil
                   "curl" "-s"
                   "-H" "Title: Compile waiting on sudo"
                   "-H" "Priority: high"
                   "-d" message
                   +config/compile-watch-ntfy-url)))

(defvar-local +config/--compile-watch-pos nil
  "Buffer position already scanned for a sudo prompt.")

(defvar-local +config/--compile-watch-timer nil)

(defun +config/compile-watch-for-sudo ()
  "Detect a sudo password prompt in ghostel-compile output and notify."
  (let* ((start (min (or +config/--compile-watch-pos (point-min)) (point-max)))
         (output (buffer-substring-no-properties start (point-max))))
    (setq +config/--compile-watch-pos (point-max))
    (when (string-match-p "\\[sudo\\] password for\\|^Password:" output)
      (notifications-notify
       :title "Compile waiting on sudo"
       :body "Enter your password in the compilation buffer.")
      (+config/--compile-watch-ntfy "Compile waiting on sudo password"))))

;;;###autoload
(defun +config/compile-watch-start ()
  "Start watching the current ghostel-compile buffer for a sudo prompt.
Call right after starting `ghostel-compile'."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not a live ghostel buffer"))
  (when +config/--compile-watch-timer
    (cancel-timer +config/--compile-watch-timer))
  (setq +config/--compile-watch-pos (point-min))
  (setq +config/--compile-watch-timer
        (run-with-timer 0 0.3 #'+config/--compile-watch-tick (current-buffer))))

(defun +config/--compile-watch-tick (buf)
  (if (not (buffer-live-p buf))
      (+config/--compile-watch-stop buf)
    (with-current-buffer buf
      ;; ghostel-compile swaps the major mode once the command finishes --
      ;; that's our signal to stop.
      (if (not (derived-mode-p 'ghostel-mode))
          (+config/--compile-watch-stop buf)
        (+config/compile-watch-for-sudo)))))

(defun +config/--compile-watch-stop (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when +config/--compile-watch-timer
        (cancel-timer +config/--compile-watch-timer)
        (setq +config/--compile-watch-timer nil)))))

(defun +config/--compile-watch-mode-hook ()
  "Start the watcher when `ghostel-compile-toggle-mode' turns on in this buffer.
Unlike advising `ghostel-compile' itself, this runs with `current-buffer'
guaranteed to be the new compile buffer -- `ghostel-compile' does not
reliably leave the new buffer selected on return (same issue as
`ghostel'/`ghostel-project')."
  (when (bound-and-true-p ghostel-compile-toggle-mode)
    (+config/compile-watch-start)))

(add-hook 'ghostel-compile-toggle-mode-hook #'+config/--compile-watch-mode-hook)

(provide 'ghostel-compile-password-notify)
;;; ghostel-compile-password-notify.el ends here

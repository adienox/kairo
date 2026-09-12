;;; config-shell.el --- shell setup -*- lexical-binding: t; -*-

(require 'notifications)

(use-package eshell
  :commands eshell
  :ensure nil
  :hook
  (eshell-mode . hide-mode-line-mode)
  :general
  (+config/leader-toggle
    "e" '(eshell :wk "Eshell"))
  :config
  (setq eshell-rc-script    (expand-file-name "eshell/profile" +config/emacs-directory)
        eshell-aliases-file (expand-file-name "eshell/aliases" +config/emacs-directory)
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh")))

(use-package ghostel
  :commands ghostel
  :hook
  (on-first-file . ghostel-compile-global-mode)
  (ghostel-mode . hide-mode-line-mode)
  :general
  (+config/leader-toggle
    "t" '(ghostel-project :wk "Terminal"))

  (general-define-key
   :keymaps 'ghostel-compile-toggle-mode-map
   "C-c C-s" #'+config/sudo-send-password)

  (general-define-key
   :keymaps 'ghostel-mode-map
   "C-\\" #'window-toggle-side-windows)
  :custom
  (+config/compile-watch-ntfy-url "https://ntfy.chipmunk-teeth.ts.net/anomaly-alerts")
  :config
  (require 'ghostel-compile-password-notify)
  (add-to-list 'ghostel-password-prompt-functions #'+config/ghostel-auth-source)

  (add-hook! +config/after-theme-change (ghostel-sync-theme))
  (add-hook! ghostel-compile-toggle-mode (hide-mode-line-mode -1)))


(defun +config/sudo-send-password (&rest _)
  "Fetch sudo password from auth-source and send it via ghostel."
  (interactive)
  (let* ((password (auth-source-pick-first-password
                    :host (system-name)
                    :user (user-login-name)
                    :port "sudo")))
    (when password
      (ghostel-send-string password)
      (ghostel-send-key "return"))))

(defvar +config/default-ghostel-auth-user "nox")

(defun +config/ghostel-auth-source (row)
  (let* ((user (or (and row
                        (string-match
                         "\\[sudo\\] .+ for \\([^:]+\\):\\|\\[sudo: authenticate\\] Password:"
                         row)
                        (match-string 1 row))
                   +config/default-ghostel-auth-user))
         (host (or (file-remote-p default-directory 'host)
                   (system-name))))
    (and user
         (auth-source-pick-first-password :host host :user user))))

(defun +config/compile-watch-for-sudo ()
  "Detect a sudo password prompt in compilation output and notify."
  (let ((output (buffer-substring-no-properties
                 compilation-filter-start (point))))
    (when (string-match-p "\\[sudo\\] password for\\|^Password:" output)
      (notifications-notify
       :title "Compile waiting on sudo"
       :body "Enter your password in the compilation buffer."))))

(use-package evil-ghostel
  :hook
  (ghostel-mode . evil-ghostel-mode)
  (ghostel-compile-toggle-mode . evil-normal-state))

(provide 'config-shell)

;;; config-shell.el ends here

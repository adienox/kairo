;;; config-core.el --- Core Emacs config setup -*- lexical-binding: t; -*-

;; Emacs 31.0.90 pretest
(setq elpaca-core-date '(20260605))

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq use-package-always-defer t)

;; use M-x use-package-report
(setq use-package-compute-statistics t)
(setq use-package-verbose t)

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

(use-package compile-angel
  :demand t
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (compile-angel-exclude-directory (expand-file-name "config/" +config/emacs-directory))
  (compile-angel-on-load-mode 1))

;; Sane defaults
(use-package emacs
  :ensure nil
  :bind*
  (("C-?" . dictionary-lookup-definition))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (pixel-scroll-precision-mode 1)
  (scroll-bar-mode -1)
  ;;(global-hl-line-mode 1)
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.
  (electric-pair-mode 1)       ;; Enable pair parens.
  (winner-mode 1)              ;; Easily undo window configuration changes.
  (global-subword-mode 1)      ;; Treat thisWord as this Word, two seperate words
  (line-number-mode 1)
  (tab-bar-mode -1)
  (column-number-mode 1)
  :custom
  (find-file-suppress-same-file-warnings t)
  ;; TUI support
  (xterm-extra-capabilities
   '(getSelection setSelection modifyOtherKeys reportBackground))
  (dictionary-server "dict.org")        ;; set dictionary server.
  (delete-selection-mode 1)             ;; Replacing selected text with typed text.
  (global-visual-line-mode 1)           ;; Better text wrapping.
  (display-line-numbers-type 'relative) ;; Use relative line numbering.
  (history-length 25)                   ;; Set the length of the command history.
  (ispell-dictionary "en_US")           ;; Default dictionary for spell checking.
  (ring-bell-function 'ignore)          ;; Disable the audible bell.
  (tab-width 4)                         ;; Set the tab width to 4 spaces.
  (use-dialog-box nil)                  ;; Disable dialog boxes.
  (warning-minimum-level :error)        ;; Set the minimum level of warnings.
  (show-paren-context-when-offscreen t) ;; Show context of parens when offscreen.
  (tab-always-indent 'complete)
  (treesit-font-lock-level 4)
  (use-short-answers t)
  (read-answer-short t)
  (imenu-auto-rescan t)
  (native-comp-async-query-on-exit t)
  (redisplay-skip-fontification-on-input t)
  (sentence-end-double-space nil)
  (confirm-nonexistent-file-or-buffer nil)
  (tab-first-completion 'word-or-paren-or-punct)
  :config
  (add-hook! before-save #'delete-trailing-whitespace)
  (add-hook! after-save  #'executable-make-buffer-file-executable-if-script-p)
  (advice-add 'yes-or-no-p :override #'y-or-n-p)
  (set-default-coding-systems 'utf-8)
  (setq-default select-enable-clipboard t
                indent-tabs-mode nil)

  ;; Configure automatic indentation to be triggered exclusively by newline and
  ;; DEL (backspace) characters.
  (setq-default electric-indent-chars '(?\n ?\^?)))

(use-package on
  :ensure (:wait t)
  :demand t
  :config
  (if (daemonp)
      (add-hook! elpaca-after-init #'on-run-first-input-hooks-h))
  ;; fix on.el to work with elpaca
  (unless (daemonp)
    (add-hook! elpaca-after-init #'on-run-init-ui-hooks-h)))

(use-package gcmh
  :hook
  (on-init-ui . gcmh-mode))

(use-package autosave
  :ensure nil
  :hook
  (on-first-file . auto-save-visited-mode)
  :custom
  (auto-save-default t)     ; auto-save every buffer that visits a file
  (auto-save-timeout 20)    ; number of seconds idle time before auto-save
  (auto-save-interval 200)  ; number of keystrokes between auto-saves
  (auto-save-no-message t)
  (auto-save-include-big-deletions t)
  (kill-buffer-delete-auto-save-files t)
  (auto-save-list-file-prefix (locate-user-emacs-file "autosave/"))
  (tramp-auto-save-directory  (locate-user-emacs-file "tramp-autosave"))
  (auto-save-visited-interval 5))

(use-package autorevert
  :ensure nil
  :hook
  (on-first-file . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode))
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(let ((trash-dir (getenv "XDG_DATA_HOME")))
  (unless (and trash-dir (file-directory-p trash-dir))
    (setq trash-dir (expand-file-name "~/.local/share"))) ;; default fallback
  (setq backup-directory-alist `(("." . ,(expand-file-name "Trash/files" trash-dir)))))

(setq make-backup-files t     ; backup of a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control   t     ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
      kept-new-versions 9)    ; newest versions to keep when a new numbered

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Disable the creation of lockfiles (e.g., .#filename).
;; Modern workflows rely on `global-auto-revert-mode' to handle external file
;; changes gracefully, making the restrictive nature of lockfiles unnecessary.
(setq create-lockfiles nil)

(use-package recentf
  :ensure nil
  :hook
  (on-first-input . recentf-mode)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (run-with-timer (* 30 60) (* 30 60) 'recentf-save-list)
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook! kill-emacs :depth -90 #'recentf-cleanup))

(use-package savehist
  :ensure nil
  :hook
  (on-first-input . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                    ;; clipboard
     register-alist               ;; macros
     mark-ring global-mark-ring   ;; marks
     search-ring
     regexp-search-ring
     command-history
     set-variable-value-history
     custom-variable-history
     query-replace-history
     read-expression-history
     minibuffer-history
     read-char-history
     face-name-history
     bookmark-history
     file-name-history))
  :config
  (savehist-mode)
  (defun unpropertize-kill-ring ()
    (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
  (add-hook! kill-emacs #'unpropertize-kill-ring))

(use-package saveplace
  :ensure nil
  :hook (on-first-file . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package age
  :hook (on-first-file . age-file-enable)
  :custom
  (age-default-identity  "~/.ssh/id_ed25519")
  (age-default-recipient "~/.ssh/id_ed25519.pub")
  (auth-sources (list (expand-file-name "authinfo.age" +config/emacs-directory))))

(use-package sops
  :hook
  (on-first-file . global-sops-mode))

;; (use-package openwith
;;   :hook
;;   (on-first-file . openwith-mode)
;;   :custom
;;   (openwith-associations
;;    '(("\\.\\(mkv\\|mp4\\|webm\\|avi\\|mov\\|m4v\\|flv\\|wmv\\|ts\\|m2ts\\)\\'"
;;       "mpv" (file))
;;      ("\\.\\(png\\|jpe?g\\|gif\\|bmp\\|webp\\|tiff?\\|svg\\)\\'"
;;       "imv" (file))))
;;   :config
;;   (define-advice abort-if-file-too-large
;;       (:around (orig size op-type filename &rest args) openwith-bypass)
;;     (unless (and filename
;;                  (assoc-default filename openwith-associations #'string-match))
;;       (apply orig size op-type filename args))))

(provide 'config-core)

;;; config-core.el ends here

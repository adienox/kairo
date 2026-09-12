;; config-ui.el --- Minibuffer Setup -*- lexical-binding: t; -*-

(use-package compat)

(use-package vertico
  :hook
  (on-first-input . vertico-mode)
  :custom
  (vertico-count-format nil)
  (vertico-count 12)
  (vertico-resize t)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j"      . vertico-next)
              ("C-M-j"    . vertico-next-group)
              ("C-k"      . vertico-previous)
              ("C-M-k"    . vertico-previous-group)
              ("M-RET"    . vertico-exit-input)
              ("<escape>" . minibuffer-keyboard-quit)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	          ("RET" . vertico-directory-enter)
	          ("DEL" . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :ensure nil
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defvar +config/vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +config/vertico-transform-functions) null))
    (dolist (fun (ensure-list +config/vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +config/vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +config/vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and
                 (memq sym minor-mode-list)
                 (boundp sym)
                 (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  (add-to-list 'vertico-multiform-categories
               '(file
                 (+config/vertico-transform-functions . +config/vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+config/vertico-transform-functions . +config/vertico-highlight-enabled-mode))))

(use-package marginalia
  :hook (vertico-mode . marginalia-mode)
  :custom
  (truncate-string-ellipsis "…")
  (marginalia--ellipsis "…")
  (marginalia-align 'right)
  (marginalia-align-offset -1))

(use-package nerd-icons-completion
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ([remap bookmark-jump]       . consult-bookmark)
  ([remap evil-show-marks]     . consult-mark)
  ([remap evil-show-registers] . consult-register)
  ([remap goto-line]           . consult-goto-line)
  ([remap imenu]               . consult-imenu)
  ([remap Info-search]         . consult-info)
  ([remap locate]              . consult-locate)
  ([remap load-theme]          . consult-theme)
  ([remap recentf-open-files]  . consult-recent-file)
  ([remap switch-to-buffer]    . consult-buffer)
  ([remap yank-pop]            . consult-yank-pop)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
  :general
  (+config/leader-search
    "g" '(consult-ripgrep :wk "Grep in dir")
    "i" '(consult-imenu   :wk "Imenu")
    "o" '(consult-outline :wk "Outline")
    "f" '(consult-fd      :wk "Fd Consult")
    "r" '(consult-recent-file  :wk "Recent File")
    "c" '(consult-mode-command :wk "Commands for mode"))
  (+config/leader-lsp
    "e" '(consult-flymake :wk "Errors"))
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")

  (add-to-list 'consult-buffer-filter "\\`\\*helpful .*\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*ChatGPT")
  (add-to-list 'consult-buffer-filter "\\`\\*EGLOT")
  (add-to-list 'consult-buffer-filter "\\`\\*apheleia-")

  (defun +config/add-to-consult-buffer-filter (names)
    "Add each name in the list NAMES to `consult-buffer-filter'.
Each element is wrapped into a regexp matching its starred buffer
form exactly, e.g. \"Messages\" matches \"*Messages*\" only."
    (dolist (name names)
      (add-to-list 'consult-buffer-filter
                   (concat "\\`\\*" (regexp-quote name) "\\*\\'"))))

  (+config/add-to-consult-buffer-filter '("Help" "Ibuffer" "Backtrace" "elpaca-log" "Warnings" "Messages" "scratch" "compilation")))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-default-command #'consult-dir-dired)
  :config
  ;; A function that returns a list of directories
  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (split-string (shell-command-to-string "zoxide query -l") "\n" t))

  ;; A consult source that calls this function
  (defvar consult-dir--source-zoxide
    `(:name     "Zoxide dirs"
                :narrow   ?f
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (executable-find "zoxide"))
                :items    ,#'consult-dir--zoxide-dirs)
    "Fasd directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package embark
  :bind
  (("C-;" . embark-act)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package ace-window
  :autoload aw-select
  :commands ace-window
  :custom
  (aw-dispatch-always t)
  :general
  (general-define-key
   :keymaps 'embark-buffer-map
   "v" (+config/embark-split-action switch-to-buffer split-window-right)
   "s" (+config/embark-split-action switch-to-buffer split-window-below)
   "a" (+config/ace-window-action   switch-to-buffer))
  (general-define-key
   :keymaps 'embark-file-map
   "v" (+config/embark-split-action find-file split-window-right)
   "s" (+config/embark-split-action find-file split-window-below)
   "a" (+config/ace-window-action   find-file)))

(defmacro +config/embark-split-action (fn split-type)
  `(defun ,(intern (concat "+config/embark-"
                           (symbol-name fn)
                           "-"
                           (car (last  (split-string
                                        (symbol-name split-type) "-"))))) ()
     (interactive)
     (select-window (funcall #',split-type))
     (call-interactively #',fn)))

(defmacro +config/ace-window-action (fn)
  `(defun ,(intern (concat "+config/ace-window-"
                           (symbol-name fn))) (target)
     (interactive)
     (select-window (aw-select nil))
     (funcall #',fn target)))

(defun +config/ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select nil)
        type 'reuse)
       (list window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(use-package embark-consult)

(provide 'config-minibuffer)

;; config-minibuffer.el ends here

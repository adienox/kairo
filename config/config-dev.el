;;; config-dev.el --- development setup -*- lexical-binding: t; -*-

(require 'config-keybinds)
(+config/leader-menu! "lsp" "l" eglot-mode-map)

(use-package treesit-auto
  :hook (on-first-file . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

(use-package mason
  :hook (prog-mode . mason-ensure)
  :general
  (+config/leader-packages
    "m" '(mason-manager :wk "Mason manager")
    "i" '(mason-install :wk "Mason install")))

(defmacro +config/mason-ensure! (packages &optional mode)
  "Ensure PACKAGES are installed via Mason.
If MODE is provided, defer installation to its hook.
Otherwise install immediately."
  (let ((install-forms
         `(mason-setup
            (dolist (pkg ,packages)
              (if (mason-installed-p pkg)
                  (message "[mason] %s already installed" pkg)
                (condition-case err
                    (progn
                      (mason-install pkg)
                      (message "[mason] Installed %s" pkg))
                  (error
                   (message "[mason] Failed to install %s: %s"
                            pkg (error-message-string err)))))))))
    (if mode
        `(add-transient-hook! ,mode ,install-forms)
      install-forms)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(use-package rainbow-mode
  :hook
  (help-mode . rainbow-mode)
  (prog-mode . rainbow-mode))

(use-package hl-todo
  :hook (prog-mode. hl-todo-mode))

(use-package tramp
  :ensure nil
  :custom
  (remote-file-name-inhibit-cache 50)
  (remote-file-name-inhibit-locks t)
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 1024 1024))) ;; 1MB
;;(tramp-verbose 2))

(use-package tramp-rpc
  :ensure (:host github :repo "ArthurHeymans/emacs-tramp-rpc")
  :custom
  (tramp-rpc-deploy-git-build-policy 'release)
  (tramp-rpc-deploy-local-cache-directory "~/.cache/emacs/tramp-rpc-binaries")
  :config
  (with-eval-after-load 'ghostel
    (add-to-list 'ghostel-tramp-shells '("rpc" login-shell "/bin/bash"))))

(use-package kirigami
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "zo" 'kirigami-open-fold)
    (define-key evil-normal-state-map "zO" 'kirigami-open-fold-rec)
    (define-key evil-normal-state-map "zc" 'kirigami-close-fold)
    (define-key evil-normal-state-map "za" 'kirigami-toggle-fold)
    (define-key evil-normal-state-map "zr" 'kirigami-open-folds)
    (define-key evil-normal-state-map "zm" 'kirigami-close-folds)))

(use-package treesit-fold
  :hook
  (prog-mode . treesit-fold-mode)
  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :box 'unspecified))

(use-package savefold
  :hook
  (on-first-file . savefold-mode)
  :init
  (setq savefold-backends '(outline org treesit-fold hideshow))
  (setq savefold-directory (locate-user-emacs-file "savefold")))

(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-code-face nil
                      :background 'unspecified))

(use-package eglot
  :ensure nil
  :hook
  (eglot-managed-mode . +config/eglot-remove-signature-eldoc)
  (eglot-managed-mode . +config/eglot-remove-code-action)
  (eglot-managed-mode . +config/update-capf-eglot)
  :general
  (+config/leader-lsp
    "r" '(eglot-rename       :wk "Eglot rename")
    "a" '(eglot-code-actions :wk "Code actions"))
  :custom
  (eglot-autoshutdown t)
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  :config
  (+config/mason-ensure! '("rassumfrassum" "codebook"))
  (add-to-list 'trusted-content +config/projects-directory)
  (set-face-attribute 'eglot-inlay-hint-face nil
                      :inherit 'font-lock-comment-face
                      :italic t))

(defun +config/eglot-remove-signature-eldoc ()
  (setq-local eldoc-documentation-functions
              (remove #'eglot-hover-eldoc-function eldoc-documentation-functions)))

(defun +config/eglot-remove-code-action ()
  (setq-local eldoc-documentation-functions
              (remove #'eglot-code-action-suggestion eldoc-documentation-functions)))

(use-package eldoc-box
  :commands eldoc-box-help-at-point
  :bind
  ([remap evil-lookup] . eldoc-box-help-at-point)
  :config
  (+config/set-eldoc-box-colors)
  (set-face-attribute 'eldoc-box-body nil :inherit 'variable-pitch)
  (add-hook! +config/after-theme-change #'+config/set-eldoc-box-colors))

(defun +config/set-eldoc-box-colors ()
  (set-face-attribute 'eldoc-box-border nil
                      :background 'unspecified
                      :inherit 'corfu-border)

  (set-face-attribute 'eldoc-box-markdown-separator nil
                      :foreground (face-foreground 'shadow nil t)))

(use-package snippy
  :ensure (:host github :repo "MiniApollo/snippy" :branch "main" :rev :newest)
  :hook (yas-global-mode . global-snippy-minor-mode)
  :custom
  (snippy-global-languages '("global"))
  :config
  (run-with-idle-timer 30 nil #'snippy-install-or-update-snippets)
  (add-hook 'completion-at-point-functions #'snippy-capf))

(defun +config/cape-eglot-yasnippet ()
  (cape-wrap-super #'eglot-completion-at-point #'yasnippet-capf #'snippy-capf))

(defun +config/update-capf-eglot ()
  "Adds snippets completion to eglot."
  (remove-hook! 'completion-at-point-functions :local #'eglot-completion-at-point)
  (add-hook! 'completion-at-point-functions :local #'+config/cape-eglot-yasnippet))

(use-package flymake
  :ensure nil
  :general
  (+config/leader-lsp
    "f" '(:ignore t :wk "Flymake")
    "f n" '(flymake-goto-next-error :wk "Next error")
    "f p" '(flymake-goto-prev-error :wk "Previous error")))

(use-package sideline-flymake
  :hook
  (flymake-mode . sideline-mode)
  (sideline-mode . +config/sideline-remove-flymake-eldoc)
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake)))

(defun +config/sideline-remove-flymake-eldoc ()
  "Remove 'flymake-eldoc-function' from 'eldoc-documentation-functions'"
  (setq-local eldoc-documentation-functions (remove #'flymake-eldoc-function eldoc-documentation-functions)))

(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  :hook ((eglot-managed-mode yaml-mode) . indent-bars-mode))

(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)
(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label 0)
(c-set-offset (quote cpp-macro) 0 nil)
(defun smart-electric-indent-mode ()
  "Disable 'electric-indent-mode in certain buffers and enable otherwise."
  (cond ((and (eq electric-indent-mode t)
              (member major-mode '(erc-mode text-mode)))
         (electric-indent-mode 0))
        ((eq electric-indent-mode nil) (electric-indent-mode 1))))
(add-hook! post-command #'smart-electric-indent-mode)

(use-package compile
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-max-output-line-length 2048)
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  :config
  (add-hook! +config/after-theme-change (load-file (expand-file-name "themes/dank-ansi-color.el" user-emacs-directory)))
  (require 'ansi-color))

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

(use-package compile-multi
  :commands compile-multi
  :general
  (+config/leader-project
    "c" '(compile-multi :wk "Compile project"))
  :custom
  (compile-multi-default-directory #'+config/project-root-or-nil))

(defun +config/project-root-or-nil ()
  (when-let* ((proj (project-current)))
    (project-root proj)))

(defun +config/compile-multi-add! (trigger &rest actions)
  "Add ACTIONS under TRIGGER to `compile-multi-config'."
  (with-eval-after-load 'compile-multi
    (if-let* ((entry (assq trigger compile-multi-config)))
        (setcdr entry (append (cdr entry) actions))
      (push (cons trigger actions) compile-multi-config))))

(use-package consult-compile-multi
  :after (:all compile-multi consult)
  :config (consult-compile-multi-mode))

(use-package compile-multi-nerd-icons
  :after (:all nerd-icons-completion compile-multi) :demand t)


(use-package compile-multi-embark
  :demand t
  :after (:all embark compile-multi)
  :config (compile-multi-embark-mode))

(use-package direnv
  :hook (on-first-input . direnv-mode)
  :config
  (+config/add-to-consult-buffer-filter '("direnv")))

(use-package docker
  :commands docker
  :general
  (+config/leader-project
    "d" '(docker :wk "Docker"))
  :custom
  (docker-show-message nil)
  (docker-container-columns
   '((:name "Names" :width 20 :template "{{ json .Names }}" :sort nil
            :format nil)
     (:name "Status" :width 20 :template "{{ json .Status }}" :sort nil
            :format nil)
     (:name "Ports" :width 10 :template "{{ json .Ports }}" :sort nil
            :format nil)
     (:name "Image" :width 15 :template "{{ json .Image }}" :sort nil
            :format nil))))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook
  (nix-ts-mode . eglot-ensure)
  :config
  (+config/mason-ensure! '("nixfmt") nix-ts-mode))

(use-package just-ts-mode
  :mode "\\justfile\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(just "https://github.com/casey/tree-sitter-just")))

(+config/compile-multi-add! '(file-exists-p "justfile")
                            #'+config/compile-multi-just-targets)

(defun +config/compile-multi-just-targets ()
  ;; Read targets from justfile.
  (mapcar (lambda (target)
            (cons (concat "just:" target) (concat "just " target)))
          (split-string (car (process-lines "just" "--summary" "--unsorted")))))

(provide 'config-dev)

;;; config-dev.el ends here

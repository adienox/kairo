;;; config-completions.el --- completions setup -*- lexical-binding: t; -*-

(use-package yasnippet
  :hook (on-first-file . yas-global-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" +config/emacs-directory))))

(use-package yasnippet-capf
  :demand t
  :after cape
  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)      ;; word completion from buffer
  (add-hook 'completion-at-point-functions #'cape-file)         ;; file name completion
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package orderless
  :demand t
  :after (:any vertico corfu)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package completion-preview
  :ensure nil
  :hook
  (on-first-buffer . global-completion-preview-mode)
  :general
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-k" nil)
  (general-define-key
   :keymaps 'completion-preview-active-mode-map
   "TAB" 'completion-preview-insert
   "C-j" 'completion-preview-next-candidate
   "C-k" 'completion-preview-prev-candidate)
  :custom
  (completion-preview-minimum-symbol-length 1)
  :config
  (set-face-attribute 'completion-preview-exact nil
                      :underline t)
  (set-face-attribute 'completion-preview-common nil
                      :underline 'unspecified))

(use-package corfu
  :hook
  (on-first-input . global-corfu-mode)
  :custom
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-auto-prefix 2)               ;; Enable auto completion
  (corfu-auto-delay 2)                ;; Enable auto completion
  (corfu-preview-current 'insert)     ;; Enable current candidate preview
  (corfu-on-exact-match nil)          ;; Configure handling of exact matches
  (corfu-scroll-margin 5)             ;; Use scroll margin
  (corfu-quit-at-boundary 'separator) ;; Quit completion unless seperator
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
        ("M-SPC"      . corfu-insert-separator)
        ("C-j"        . corfu-next)
        ("C-k"        . corfu-previous)
        ("S-<return>" . corfu-insert)
        ("RET"        . nil))
  :config
  (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch)
  (+config/set-corfu-colors)
  (add-hook! +config/after-theme-change #'+config/set-corfu-colors)
  (add-hook! evil-insert-state-exit #'corfu-quit))

(defun +config/set-corfu-colors ()
  (when (featurep 'corfu)
    (set-face-attribute 'corfu-default nil :background (face-background 'default nil t))
    (set-face-attribute 'corfu-current nil :background (face-background 'highlight nil t))))

(use-package nerd-icons-corfu
  :demand t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-history
  :ensure nil
  :after (corfu savehist)
  :hook (corfu-mode . corfu-history-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.2 . 0.5)))

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'config-completions)

;;; config-completions.el ends here

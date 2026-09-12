;;; config-helpful.el --- helpful setup -*- lexical-binding: t; -*-

(use-package helpful
  :hook
  (helpful-mode . hide-mode-line-mode)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap view-hello-file]   . helpful-at-point))

(use-package elisp-demos
  :demand t
  :after helpful
  :custom
  (elisp-demos-user-files (list (expand-file-name "demos.org" +config/emacs-directory)))
  :config
  (+config/elisp-demos-set-faces)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(defun +config/elisp-demos-set-faces ()
  (set-face-attribute 'eros-result-overlay-face nil
                      :background 'unspecified
                      :inherit font-lock-comment-face))

(add-hook! +config/after-theme-change #'+config/elisp-demos-set-faces)

(use-package eros
  :hook (on-first-input . eros-mode)
  :bind
  ([remap eval-last-sexp] . eros-eval-last-sexp)
  ([remap eval-defun]     . eros-eval-defun)
  :config
  (set-face-attribute 'eros-result-overlay-face nil
                      :box 'unspecified))

(provide 'config-helpful)

;;; config-helpful.el ends here

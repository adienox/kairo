;;; lang-python.el --- Python Setup -*- lexical-binding: t; -*-

(use-package python-ts-mode
  :ensure nil
  :hook
  (python-ts-mode . eglot-ensure)
  ;; (python-ts-mode . +config/remove-python-eldoc-function)
  :config
  (+config/compile-multi-add! 'python-mode
                              '("python:interpreter" "python3" (buffer-file-name)))

  (+config/compile-multi-add! 'python-mode
                              '("python:uv" "uv" "run" "--script" (buffer-file-name)))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-ts-mode python-mode) . ("rass" "python" "--" "codebook-lsp" "serve"))))

  (+config/mason-ensure! '("ty" "ruff" "codebook") python-ts-mode))

(defun +config/remove-python-eldoc-function ()
  (setq-local eldoc-documentation-functions
              (remove #'python-eldoc-function eldoc-documentation-functions)))

(use-package uv
  :commands uv
  :ensure (:host github :repo "ethan0456/uv.el")
  :general
  (+config/leader-lsp
    :keymaps 'python-ts-mode-map
    "u" '(uv :wk "Python UV")))

(provide 'lang-python)

;; lang-python.el ends here

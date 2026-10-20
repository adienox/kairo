;;; lang-c.el --- C Setup -*- lexical-binding: t; -*-

(use-package c-ts-mode
  :ensure nil
  :hook
  (c++-ts-mode . eglot-ensure)
  :config
  (+config/mason-ensure! '("clang-format" "clangd") c++-ts-mode))

(provide 'lang-c)

;; lang-c.el ends here

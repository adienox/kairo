;;; lang-flutter.el --- Flutter Setup -*- lexical-binding: t; -*-

(require 'project-init)

(use-package dart-mode :mode "\\.dart\\'")

(use-package dart-ts-mode
  :hook (dart-ts-mode . eglot-ensure)
  :ensure (:host github :repo "50ways2sayhard/dart-ts-mode")
  :config
  (add-to-list 'treesit-language-source-alist
               '(dart ("https://github.com/UserNobody14/tree-sitter-dart"))))

(+config/add-project-vc-root-markers "pubspec.yaml")

(add-to-list '+config/project-init-commands
             '("dart" . ("dart create %s" . t)))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))

(provide 'lang-flutter)

;; lang-flutter.el ends here

;;; config-undo.el --- undo setup -*- lexical-binding: t; -*-

(use-package undo-fu
  :demand t
  :after evil
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (setq undo-limit 67108864)          ; 64mb.
  (setq undo-strong-limit 100663296)  ; 96mb.
  (setq undo-outer-limit 1006632960)) ; 960mb.

(use-package undo-fu-session
  :hook (elpaca-after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :commands vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

(provide 'config-undo)

;;; config-undo.el ends here

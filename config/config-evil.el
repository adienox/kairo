;;; config-evil.el --- Evil Setup -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :hook
  (on-first-input . evil-mode)
  :general
  (general-define-key
   :states '(normal visual motion)
   (kbd "C-S-v") #'cua-set-mark
   (kbd "C-\\")  #'window-toggle-side-windows
   (kbd "C-M-u") #'vundo
   "j" #'evil-next-visual-line
   "k" #'evil-previous-visual-line
   "H" #'evil-first-non-blank
   "L" #'evil-end-of-line)

  (general-define-key
   :states '(insert)
   (kbd "C-v") #'evil-paste-after)

  (+config/leader-buffer
    "n" '(evil-buffer-new :wk "New unnamed buffer")
    "l" '(evil-switch-to-windows-last-buffer :wk "Switch to last open buffer"))
  :custom
  (evil-undo-system 'undo-fu)
  ;; C-u behaves like it does in vim
  (evil-want-C-u-scroll t)
  ;; Make :s in visual mode operate only on the actual visual selection
  ;; (character or block), instead of the full lines covered by the selection
  (evil-ex-visual-char-range t)
  ;; Use Vim-style regular expressions in search and substitute commands,
  ;; allowing features like \v (very magic), \zs, and \ze for precise matches
  (evil-ex-search-vim-style-regexp t)
  ;; Enable automatic vertical split to the right
  (evil-vsplit-window-right t)
  ;; Disable echoing Evil state to avoid replacing eldoc
  (evil-echo-state nil)
  ;; Do not move cursor back when exiting insert state
  (evil-move-cursor-back nil)
  ;; Make `v$` exclude the final newline
  (evil-v$-excludes-newline t)
  ;; Allow C-h to delete in insert state
  (evil-want-C-h-delete t)
  ;; Enable C-u to delete back to indentation in insert state
  (evil-want-C-u-delete t)
  ;; Enable fine-grained undo behavior
  (evil-want-fine-undo t)
  ;; Whether Y yanks to the end of the line
  (evil-want-Y-yank-to-eol t))

(use-package evil-collection
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-want-find-usages-bindings t))

(use-package evil-goggles
  :hook (on-first-file . evil-goggles-mode)
  :custom
  (evil-goggles-duration 0.1)
  (evil-goggles-pulse nil) ; too slow
  ;; evil-goggles provides a good indicator of what has been affected.
  ;; delete/change is obvious, so I'd rather disable it for these.
  (evil-goggles-enable-delete nil)
  (evil-goggles-enable-change nil)
  :config
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-surround :hook (evil-mode . global-evil-surround-mode))

(use-package anzu
  :hook (on-first-file . global-anzu-mode)
  :general
  (+config/leader-key
    "/" '(+config/replace-regexp :wk "Find and replace"))
  :custom
  (replace-regexp-lax-whitespace t)
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "teal")
  (set-face-attribute 'anzu-replace-to nil
                      :foreground 'unspecified
                      :slant 'italic
                      :inherit 'evil-ex-substitute-replacement))

(defun +config/replace-regexp (arg)
  (interactive "P")
  (if arg
      (call-interactively #'anzu-query-replace-regexp)
    (call-interactively #'replace-regexp)))

(use-package evil-anzu :after anzu :demand t)

(defun +config/toggle-cursor ()
  "Toggle the cursor visibility in the current buffer."
  (interactive)
  (if (null cursor-type)
      (progn
        (kill-local-variable 'evil-default-cursor)
        (kill-local-variable 'cursor-type))
    (setq-local evil-default-cursor '(nil))
    (setq-local cursor-type nil))
  (evil-refresh-cursor))

(provide 'config-evil)

;; config-evil.el ends here

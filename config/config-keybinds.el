;;; config-keybinds.el --- Keybinds Setup -*- lexical-binding: t; -*-

;; copied from https://alexforsale.github.io/posts/emacs-general/#useful-macro
(defmacro +config/leader-menu! (name infix-key &optional keymap &rest body)
  "Create a definer NAME `+config/leader-NAME' wrapping `+config/leader-key'.
Create prefix map: `+config/leader-NAME-map'. Prefix bindings in BODY with INFIX-KEY.
Optional KEYMAP (e.g. \\='prog-mode-map) is passed as :keymaps to `general-create-definer'."
  (declare (indent 2))
  `(progn
     (general-create-definer ,(intern (concat "+config/leader-" name))
       :wrapping +config/leader-key
       :prefix-map (quote ,(intern (concat "+config/leader-" name "-map")))
       :infix ,infix-key
       ,@(when keymap `(:keymaps ',keymap))
       :wk-full-keys nil
       "" '(:ignore t :which-key ,name))
     (,(intern (concat "+config/leader-" name))
      ,@body)))

(use-package general
  :ensure (:wait t) ;; according to elpaca docs, if a package is needed in the init file, add (:wait t) to ensure
  :demand t
  :config
  (general-evil-setup)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; leader key specified
  (general-create-definer +config/leader-key
    :states  '(insert emacs normal hybrid motion visual operator)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

;;; leader keys
  (+config/leader-menu! "apps"     "a")
  (+config/leader-menu! "buffer"   "b")
  (+config/leader-menu! "quit"     "q")
  (+config/leader-menu! "search"   "s")
  (+config/leader-menu! "packages" "r")
  (+config/leader-menu! "file"     "f")
  (+config/leader-menu! "toggle"   "t")

  (+config/leader-key
    ";" '(eval-expression    :wk "Eval expression")
    "." '(find-file          :wk "find file")
    "^" '(subword-capitalize :wk "Capitalize subword")
    "u" '(universal-argument :wk "Universal argument" ))

  (defun +config/restart-emacs! ()
    (interactive)
    (start-process "restart-emacs" nil "systemctl" "--user" "restart" "emacs.service"))

  (+config/leader-quit
    "k" '(kill-emacs             :wk "Kill emacs")
    "r" '(+config/restart-emacs! :wk "Restart emacs")
    "f" '(delete-frame           :wk "Delete frame"))

  (+config/leader-search
    "m" '(bookmark-jump :wk "Bookmarks"))

  (+config/leader-file
    "b" '(bookmark-set :wk "Bookmark set")
    "s" '(save-buffer  :wk "Save file"))

  (+config/leader-toggle
    "l" '(elpaca-log                :wk "Package log")
    "n" '(display-line-numbers-mode :wk "Line numbers")
    "d" '(toggle-window-dedicated   :wk "Dedicated window"))

  (+config/leader-packages
    "t" '(elpaca-try      :wk "Try package")
    "n" '(elpaca-info     :wk "Named package filter")
    "u" '(elpaca-pull-all :wk "Upgrade packages"))

  (+config/leader-buffer
    "[" '(previous-buffer       :wk "previous buffer")
    "]" '(next-buffer           :wk "next buffer")
    "b" '(switch-to-buffer      :wk "switch to buffer")
    "c" '(clone-indirect-buffer :wk "clone buffer")
    "d" '(kill-buffer           :wk "kill buffer")
    "k" '(kill-current-buffer   :wk "kill current buffer")
    "r" '(revert-buffer-quick   :wk "revert buffer")
    "R" '(rename-buffer         :wk "rename buffer")
    "z" '(bury-buffer           :wk "bury buffer")
    "C" '(clone-indirect-buffer-other-window                   :wk "clone buffer other window")
    "o" '((lambda () (interactive) (switch-to-buffer nil))          :wk "Other buffer")
    "m" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :wk "Switch to messages buffer")
    "w" '((lambda () (interactive) (switch-to-buffer "*Warnings*")) :wk "Switch to warnings buffer")
    "x" '((lambda () (interactive) (switch-to-buffer "*scratch*"))  :wk "switch to scratch buffer")))

(use-package which-key
  :ensure nil
  :hook (on-first-input . which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 5)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.3)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)
  (which-key-dont-use-unicode nil))

(provide 'config-keybinds)

;; config-keybinds.el ends here

;;; config-dired.el --- dired setup -*- lexical-binding: t; -*-

(require 'config-keybinds)
(+config/leader-menu! "dired" "d")

(use-package dirvish
  :commands dirvish
  :custom
  (dirvish-quick-access-entries
   `(("h" "~/"                          "Home")
     ("D" "~/Documents/"                "Documents")
     ("p" ,+config/projects-directory    "Projects")
     ("n" "~/Documents/notes/"          "Notes")
     ("d" "~/Downloads/"                "Downloads")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-side-attributes
   '(vc-state file-size nerd-icons collapse))
  (dirvish-reuse-session 'open)
  (dirvish-mode-line-bar-image-width 0)
  (dirvish-mode-line-height 32)
  (dirvish-preview-dired-sync-omit t)
  :general
  (+config/leader-dired
    "d" '(dirvish      :wk "Dired")
    "s" '(dirvish-side :wk "Side Follow"))
  (general-define-key
   :states  'normal
   :keymaps 'dired-mode-map
   "h" 'dired-up-directory
   "l" 'dired-open-file)
  (general-define-key
   :states  'normal
   :keymaps 'dirvish-mode-map
   "?"   'dirvish-dispatch
   "a"   'dirvish-quick-access
   "TAB" 'dirvish-subtree-toggle
   "q"   'dirvish-quit)
  :config
  (add-hook! dired-mode (visual-line-mode -1))
  (with-eval-after-load 'spacious-padding-mode
    (add-hook! dired-mode
      (setq-local spacious-padding-widths
                  '(:internal-border-width 15 :header-line-width 4 :mode-line-width 6
                                           :custom-button-width 3 :tab-width 4
                                           :right-divider-width 30 :scroll-bar-width 8
                                           :fringe-width 8))))
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode))

(add-hook! dirvish-special-preview-mode #'+config/toggle-cursor)
(add-hook! dirvish-directory-view-mode #'+config/toggle-cursor)

(use-package dirvish-emerge
  :commands dirvish-emerge-mode
  :ensure nil
  :general
  (+config/leader-dired
    :keymaps 'dirvish-mode-map
    "e" '(dirvish-emerge-mode :wk "Emerge mode"))
  :config
  (setq dirvish-emerge-groups
        ;;  Header string  |   Type   |  Criterias
        '(("Recent files"  (predicate . recent-files-2h))
          ("Documents"     (extensions "pdf" "tex" "bib" "epub"))
          ("Text"          (extensions "md" "org" "txt"))
          ("Video"         (extensions "mp4" "mkv" "webm"))
          ("Pictures"      (extensions "jpg" "png" "svg" "gif"))
          ("Audio"         (extensions "mp3" "flac" "wav" "ape" "aac"))
          ("Archives"      (extensions "gz" "rar" "zip")))))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . dired-omit-mode)
  :general
  (+config/leader-dired
    "o" '(dired-omit-mode :wk "Omit mode"))
  :custom
  ;; hide files/directories starting with "." in dired-omit-mode
  (dired-omit-files (rx (seq bol ".")))
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-free-space nil)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies  'always)
  (dired-create-destination-dirs 'ask))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode)
  ;; highlight parent and directory preview as well
  (dirvish-directory-view-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(add-hook! (on-init-ui +config/after-theme-change) (load-file (expand-file-name "themes/dank-diredfl.el" user-emacs-directory)))

(use-package dired-open
  :after dired
  :custom
  (dired-open-extensions '(("gif"  . "imv")
                           ("jpg"  . "imv")
                           ("webp" . "imv")
                           ("png"  . "imv")
                           ("mkv"  . "mpv")
                           ("webm" . "mpv")
                           ("mp4"  . "mpv"))))

(provide 'config-dired)

;; config-dired.el ends here

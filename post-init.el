;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

(setq init-start-time (current-time))

(load custom-file 'noerror 'no-message)

(use-package exec-path-from-shell
  :hook
  (on-first-input . exec-path-from-shell-initialize))

(defvar nox/authinfo-file (file-name-concat minimal-emacs-user-directory "authinfo.gpg")
  "Authinfo file")

(setq auth-sources (list nox/authinfo-file))

(use-package emacs
  :ensure nil
  :bind*
  (("C-?" . dictionary-lookup-definition))
  :init
  (pixel-scroll-precision-mode 1)
  ;;(global-hl-line-mode 1)
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.
  (electric-pair-mode 1)       ;; Enable pair parens.
  (display-battery-mode 1)     ;; Enable displaying battery info in modline.
  (winner-mode 1)              ;; Easily undo window configuration changes.
  (line-number-mode 1)
  (column-number-mode 1)
  :custom
  (dictionary-server "dict.org")        ;; set dictionary server.
  (delete-selection-mode 1)             ;; Replacing selected text with typed text.
  (global-visual-line-mode 1)           ;; Better text wrapping.
  (display-line-numbers-type 'relative) ;; Use relative line numbering.
  (history-length 25)                   ;; Set the length of the command history.
  (ispell-dictionary "en_US")           ;; Default dictionary for spell checking.
  (ring-bell-function 'ignore)          ;; Disable the audible bell.
  (tab-width 4)                         ;; Set the tab width to 4 spaces.
  (use-dialog-box nil)                  ;; Disable dialog boxes.
  (warning-minimum-level :error)        ;; Set the minimum level of warnings.
  (show-paren-context-when-offscreen t) ;; Show context of parens when offscreen.

  ;; TAB key complete, instead of just indenting.
  (tab-always-indent 'complete)
  ;; Use advanced font locking for Treesit mode.
  (treesit-font-lock-level 4)
  ;; Offer to delete any autosave file when killing a buffer.
  (kill-buffer-delete-auto-save-files t)
  :config
  (add-hook! before-save #'delete-trailing-whitespace)
  (add-hook! after-save  #'executable-make-buffer-file-executable-if-script-p)
  (set-default-coding-systems 'utf-8)
  (setq-default select-enable-clipboard t
                indent-tabs-mode nil))

(use-package server
  :ensure nil
  :hook (elpaca-after-init . server-start))

(defun nox/quit-emacs-server ()
  "Save buffers and kill Emacs server."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(use-package evil
  :hook
  (on-first-input . evil-mode)
  :init
  ;; It has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-mode)

  (evil-define-key 'normal 'global
    (kbd "C-S-v") 'cua-set-mark
    (kbd "C-\\")  'window-toggle-side-windows
    (kbd "C-M-u") 'vundo
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "s" 'evil-avy-goto-char-timer)

  (evil-define-key '(normal visual) 'global
    "P" 'consult-yank-from-kill-ring
    "H" 'evil-first-non-blank
    "L" 'evil-end-of-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-want-find-usages-bindings t))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer nox/leader-keys
    :states  '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer nox/prog-keys
    :states  '(normal insert visual emacs)
    :keymaps 'prog-mode-map
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer nox/org-keys
    :states  '(normal insert visual emacs)
    :keymaps 'org-mode-map
    :prefix "SPC"
    :global-prefix "C-SPC")

(nox/leader-keys
  "a"   '(:ignore t :wk "[A]pplications")
  "a c" '(calendar :wk "[C]alendar")
  "a e" '(=elfeed :wk "[E]lfeed")
  "a g" '(gptel :wk "[G]ptel")
  "a m" '(mu4e :wk "[M]ail"))

(nox/leader-keys
  "b"   '(:ignore t :wk "[B]uffer")
  "b b" '(consult-buffer :wk "[B]uffer Switch")
  "b i" '(persp-ibuffer :wk "[I]buffer")
  "b k" '(kill-current-buffer :wk "[K]ill Buffer")
  "b n" '(next-buffer :wk "[N]ext Buffer")
  "b p" '(previous-buffer :wk "[P]revious Buffer")
  "b r" '(revert-buffer :wk "[R]eload Buffer"))

(nox/prog-keys
  "c"   '(:ignore t :wk "[C]ode")
  "c a" '(eglot-code-actions :wk "Code actions")
  "c r" '(eglot-rename :wk "Eglot rename")
  "c e" '(flycheck-list-errors :wk "List errors")
  "c f" '(consult-flymake :wk "Consult [F]lymake")
  "c n" '(flymake-goto-next-error :wk "[N]ext flymake error")
  "c p" '(flymake-goto-prev-error :wk "[P]revious flymake error")

  "c d"   '(:ignore t :wk "[D]irenv")
  "c d a" '(envrc-allow :wk "[A]llow environment")
  "c d r" '(envrc-reload :wk "[R]eload environment"))

(nox/leader-keys
  "d"   '(:ignore t :wk "[D]ired")
  "d ." '(dired-omit-mode :wk "Toggle dot files")
  "d d" '(dirvish :wk "[D]irvish")
  "d h" '(dired-hide-details-mode :wk "[D]ired"))

(nox/leader-keys
  "e"   '(:ignore t :wk "[E]val")
  "e b" '(eval-buffer :wk "[B]uffer Eval")
  "e d" '(eros-eval-defun :wk "[D]efun Eval")
  "e e" '(eval-expression :wk "[E]xpression Eval")
  "e l" '(eros-eval-last-sexp :wk "[E]xpression Before Eval")
  "e r" '(eval-region :wk "[R]egion Eval"))

(nox/leader-keys
  "f"   '(:ignore t :wk "[F]ile")
  "f c" `((lambda () (interactive) (find-file nox/emacs-config-file)) :wk "[C]onfig File")
  "f s" '(save-buffer :wk "[S]ave Buffer")
  "f b" '(bookmark-set :wk "[B]ookmark Set")
  "f d" '(bufferfile-delete :wk "[D]elete File")
  "f r" '(bufferfile-rename :wk "[R]ename File")
  "f u" '(sudo-edit-find-file :wk "S[U]do Find File")
  "f U" '(sudo-edit :wk "S[U]do Edit File"))

(nox/leader-keys
  "g"   '(:ignore t :wk "[G]it")
  "g g" '(magit-status :wk "[G]it status")
  "g c" '(magit-commit-create :wk "[G]it commit")
  "g n" '(diff-hl-next-hunk :wk "[N]ext hunk")
  "g u" '(diff-hl-revert-hunk :wk "[U]ndo hunk")
  "g p" '(diff-hl-previous-hunk :wk "[P]revious hunk")
  "g s" '(diff-hl-stage-dwim :wk "[S]tage hunk")
  "g P" '(magit-push :wk "[P]ush repo")
  "g S" '(consult-gh-search-repos :wk "[S]earch repos")
  "g D" '(consult-gh-repo-delete :wk "[D]elete repo")
  "g C" '(consult-gh-repo-create :wk "[C]reate repo"))

(nox/leader-keys
  "o"   '(:ignore t :wk "[O]rg")
  "o a" '(org-agenda :wk "[A]genda")
  "o c" '(org-capture :wk "[C]apture")
  "o i" `((lambda () (interactive) (find-file nox/inbox-file)) :wk "[I]nbox")
  "o x" '(org-toggle-checkbox :wk "[C]heckbox")
  "o t" '(nox/capture-todo :wk "[T]odo Capture")
  "o j" '(nox/capture-journal :wk "[J]ournal Capture"))

(nox/leader-keys
  "o k"   '(:ignore t :wk "[K]indle")
  "o k ]" '(nox/kindle-next-page :wk "Next page")
  "o k [" '(nox/kindle-prev-page :wk "Previous page")
  "o k f" '(nox/kindle-first-page :wk "[F]irst page")
  "o k n" '(nox/kindle-next-bookmark :wk "[N]ext bookmark")
  "o k p" '(nox/kindle-prev-bookmark :wk "[P]revious bookmark"))

(nox/leader-keys
  "o l"   '(:ignore t :wk "[L]ink")
  "o l a" '(org-transclusion-add-all :wk "[A]dd transclusions")
  "o l r" '(org-transclusion-remove-all :wk "[R]emove transclusions")
  "o l o" '(org-transclusion-open-source :wk "[O]pen transclusion")
  "o l A" '(org-transclusion-make-from-link :wk "[A]dd link transclusion")
  "o l s" '(org-store-link :wk "[S]tore link"))

(nox/leader-keys
  "o b" '(:ignore t :wk "[B]abel")
  "o b t" '(org-babel-tangle :wk "[T]angle")
  "o b e" '(org-edit-src-code :wk "[E]dit src code")
  "o b d" '(org-babel-demarcate-block :wk "[D]emarcate Block"))

(nox/leader-keys
  "o n"   '(:ignore t :wk "[N]otes")
  "o n f" '(nox/denote-search :wk "[F]ind note")
  "o n r" '(denote-explore-random-note :wk "[R]andom note")
  "o n i" '(denote-explore-isolated-files :wk "[I]solated notes")
  "o n R" '(denote-rename-file :wk "[R]ename note")
  "o n m" '(denote-hide-metadata-mode :wk "[M]etadata toggle")
  "o n l" '(denote-link-or-create :wk "[L]ink note")
  "o n b" '(denote-backlinks :wk "[B]acklinks")
  "o n j" '(denote-journal-new-or-existing-entry :wk "[J]ournal"))

(nox/org-keys
 "o n i" '(nox/org-dblock-insert-inbox-notes :wk "[I]nbox notes"))

(nox/leader-keys
  "q"   '(:ignore t :wk "[Q]uit")
  "q f" '(delete-frame :wk "[F]rame delete")
  "q r" '(nox/restore-perspectives :wk "[R]estore perspectives")
  "q K" '(kill-emacs :wk "[K]ill emacs"))

(nox/leader-keys
  "p"   '(:ignore t :wk "[P]roject")
  "SPC" '(projectile-find-file :wk "Find file in project")
  "p r" '(projectile-remove-known-project :wk "[R]emove Project")
  "p c" '(projectile-compile-project :wk "[C]ompile Project")
  "p s" '(+switch-or-make-project :wk "[S]witch Project"))

(defun nox/reload-config()
  "Reload Emacs config"
  (interactive)
  (load-file (expand-file-name "post-init.el" nox/emacs-directory)))

(nox/leader-keys
  "r" '(:ignore t :wk "[R]eload & Packages")
  ;; Mason.el
  "r m" '(mason-manager :wk "Mason manager")
  "r i" '(mason-install :wk "Mason install")
  ;; Package-menu-mode
  "r t" '(elpaca-try :wk "[T]ry package")
  "r n" '(elpaca-info :wk "[N]amed package filter")
  "r u" '(elpaca-pull-all :wk "[U]pgrade packages")

  "r r" '(nox/reload-config :wk "[R]eload config"))

(nox/leader-keys
  "s"   '(:ignore t :wk "[S]earch")
  "s g" '(consult-ripgrep :wk "[G]rep in dir")
  "s i" '(consult-imenu :wk "[I]menu")
  "s f" '(consult-fd :wk "[F]d Consult")
  "s r" '(consult-recent-file :wk "[R]recent File")
  "s m" '(bookmark-jump :wk "[M]arks")
  "s c" '(consult-mode-command :wk "[C]ommands for mode"))

(nox/leader-keys
  "t"   '(:ignore t :wk "[T]oggle")
  "t e" '(eshell :wk "[E]shell")
  "t t" '(toggle-theme :wk "[T]oggle theme")
  "t l" '(elpaca-log :wk "[L]og Elpaca")
  "t h" '(hl-line-mode :wk "[H]ighlight Line")
  "t c" '(olivetti-mode :wk "[C]olumn Fill Mode")
  "t d" '(toggle-window-dedicated :wk "[D]edicated Mode")
  "t v" '(vterm :wk "[V]term")
  "t n" '(display-line-numbers-mode :wk "[N]umbered Lines"))

(nox/leader-keys
  "TAB"   '(:ignore t :wk "Workspaces")
  "TAB TAB" '(+list-workspaces :wk "List Workspaces")
  "TAB [" '(persp-prev :wk "Previous Workspace")
  "TAB ]" '(persp-next :wk "Next Workspace")
  "TAB d" '((lambda () (interactive) (persp-kill (persp-name (persp-curr)))) :wk "Delete workspace")
  "TAB m" '((lambda () (interactive) (pop-to-buffer "*Messages*")) :wk "Messages buffer")
  "TAB w" '((lambda () (interactive) (pop-to-buffer "*Warnings*")) :wk "Warnings buffer")
  "TAB n" '(persp-switch :wk "New Workspace"))

(nox/leader-keys
  "RET" '(consult-bookmark :wk "Jump to Bookmark")
  "'" '(vertico-repeat :wk "Resume last search")
  "," '(consult-buffer :wk "Switch buffer")
  "." '(find-file :wk "Find File")))

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
  (which-key-separator " → " ))

(setq auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
      auto-save-interval 200) ; number of keystrokes between auto-saves

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

(setq auto-save-visited-interval 5) ; Save after 5 seconds if inactivity
(auto-save-visited-mode 1)

(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (on-first-input . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

;; setting the backup dir to trash.
(let ((trash-dir (getenv "XDG_DATA_HOME")))
  (unless (and trash-dir (file-directory-p trash-dir))
    (setq trash-dir (expand-file-name "~/.local/share"))) ;; default fallback
  (setq backup-directory-alist `(("." . ,(expand-file-name "Trash/files" trash-dir)))))

(setq make-backup-files t     ; backup of a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control   t     ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
      kept-new-versions 9)    ; newest versions to keep when a new numbered

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (on-first-input . recentf-mode)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (run-with-timer (* 30 60) (* 30 60) 'recentf-save-list)
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook! kill-emacs :depth -90 #'recentf-cleanup))

(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (on-first-input . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                     ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring
     regexp-search-ring
     command-history
     set-variable-value-history
     custom-variable-history
     query-replace-history
     read-expression-history
     minibuffer-history
     read-char-history
     face-name-history
     bookmark-history
     file-name-history)))

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook! kill-emacs #'unpropertize-kill-ring)

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (on-first-file . save-place-mode)
  :custom
  (save-place-limit 400))

(defun nox/get-secret (path)
  "Retrieve a specific secret using yq from the decrypted SOPS file."
  (string-trim
   (shell-command-to-string
    (format "sops -d %s | yq -r '%s'"
            (shell-quote-argument
             (expand-file-name "~/Documents/projects/ember/secrets/secrets.sops.yaml"))
            path))))

(defun nox/open-todays-agenda ()
  "Open today's Org agenda in a dedicated floating frame."
  (interactive)
  ;; Create a new frame with a name and class
  (let ((frame (make-frame '((name . "agenda")))))
    (select-frame-set-input-focus frame)
    ;; Open agenda for today
    (org-agenda nil "a")
    ;; Delete other windows to show only agenda buffer
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) t)))

(defun nox/move-up-and-center ()
  "Move up and center."
  (interactive)
  (evil-previous-visual-line)
  (recenter))

(defun nox/move-down-and-center ()
  "Move down and center."
  (interactive)
  (evil-next-visual-line)
  (recenter))

(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

(defvar hyprland-directions-alist
  '(("l" . left)
    ("r" . right)
    ("u" . up)
    ("d" . down)))

(defmacro hyprctl (&rest args)
  `(start-process "emacs-hyprland-integration" nil "hyprctl" "dispatch" ,@args))

(use-package windmove
  :ensure nil
  :autoload windmove-find-other-window)

(defun nox/emacs-hyprland-integration (dir)
  "Move focus in Emacs or Hyprland depending on DIR."
  ;; Assign windmove-dir to correct windmove direction format
  (let ((windmove-dir (cdr (assoc dir hyprland-directions-alist)))
        (other-window nil))
    (setq other-window (windmove-find-other-window windmove-dir))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (hyprctl "movefocus" dir)
      (windmove-do-window-select windmove-dir))))

(defvar nox/after-theme-change-hook nil
  "Hook run after a theme is changed.")

(defun nox/run-after-theme-change-hook (&rest _)
  "Run `nox/after-theme-change-hook` after theme change."
  (run-hooks 'nox/after-theme-change-hook))

;; run `nox/after-theme-change-hook' after load-theme
(advice-add 'load-theme :after #'nox/run-after-theme-change-hook)

(use-package mason
  :hook (on-first-input . mason-ensure))

(defun nox/mason-ensure (arg &optional packages)
  "Ensure PACKAGES are installed via Mason.
If ARG is a symbol (mode), add a Doom-style hook for it.
If ARG is a list (packages), install immediately."
  (if (symbolp arg)
      ;; ARG is a mode
      (when packages
        (add-hook! arg
          (dolist (pkg packages)
            (unless (mason-installed-p pkg)
              (ignore-errors
                (mason-install pkg))))))
    ;; ARG is actually the packages list, install now
    (let ((pkgs arg))
      (dolist (pkg pkgs)
        (unless (mason-installed-p pkg)
          (ignore-errors
            (mason-install pkg)))))))

(use-package avy
  :commands
  (evil-avy-goto-char-timer
   nox/avy-jump-org-block
   nox/avy-jump-to-link)
  :custom
  (avy-background t)
  :config
  (set-face-attribute 'avy-background-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit    'shadow))

(defun nox/avy-jump-org-block ()
  "Jump to org block using Avy subsystem."
  (interactive)
  (avy-jump (rx line-start (zero-or-more blank) "#+begin_src")
            :action 'goto-char)
  ;; Jump _into_ the block:
  (forward-line))

(defun nox/avy-jump-to-link ()
  "Jump to links using Avy subsystem."
  (interactive)
  (avy-jump (rx (or "http://" "https://")) :action 'goto-char))

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(with-eval-after-load 'avy
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?' avy-dispatch-alist) 'avy-action-embark))

(defun nox/open-in-reddigg (url &optional new-window)
    "Open the provided url in reddigg"
    (reddigg-view-comments url))

(defun nox/parse-readwise (url &optional new-window)
  "Extract, decode and open the save URL part from a given Readwise URL."
  (if (string-match "https://wise\\.readwise\\.io/save\\?url=\\(.*\\)" url)
      (browse-url (url-unhex-string (match-string 1 url)))
    (error "Invalid URL format")))

(setq browse-url-handlers
      '(("^https?://www\\.reddit\\.com" . nox/open-in-reddigg)
        ("^https?://arstechnica\\.com" . eww)
        ("^https?://wise\\.readwise\\.io/save\\?url=" . nox/parse-readwise)))

(setq browse-url-generic-program "firefox")

(use-package calendar
  :ensure nil
  :defer t
  :commands calendar
  :hook
  (calendar-mode . olivetti-mode)
  (calendar-mode . hide-mode-line-mode)
  (calendar-mode . (lambda () (setq-local olivetti-body-width 75)))
  (calendar-mode . (lambda () (setq-local global-hl-line-mode nil)))
  (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-mode-line-format nil)
  (calendar-mark-holidays-flag t) ;; Show holidays
  ;; disable unwanted calendar holidays
  (holiday-christian-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-solar-holidays nil)
  :config
  (nox/set-calendar-colors)
  (add-hook! nox/after-theme-change #'nox/set-calendar-colors))

(add-to-list
 'display-buffer-alist
 '("\\*Calendar\\*"
   (display-buffer-same-window)
   (body-function
    . (lambda (buf)
        (with-current-buffer buf
          (setq-local mode-line-format nil))))))

(defun nox/set-calendar-colors ()
  (set-face-attribute 'holiday nil
                      :background 'unspecified
                      :foreground (doom-color 'red)
                      :underline  'unspecified)
  (require 'denote-journal)
  (set-face-attribute 'denote-journal-calendar nil
                      :box 'unspecified
                      :foreground (doom-color 'yellow))
  (set-face-attribute 'calendar-today nil
                      :foreground (doom-color 'green)
                      :underline  'unspecified))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory nox/notes-directory)
  (denote-save-buffers t)
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-date-prompt-use-org-read-date t)
  :config
  (add-hook! denote-after-new-note #'denote-hide-metadata)
  (denote-rename-buffer-mode 1))

(defvar denote-workspace-name "notes" "Default denote workspace name.")

(use-package consult-denote
  :hook (on-first-input . consult-denote-mode))

(use-package denote-explore
  :commands (denote-explore-isolated-files denote-explore-random-note)
  :custom
  (denote-explore-isolated-ignore-keywords '("journal" "podcast")))

(use-package denote-org :defer t)

(use-package denote-journal
  :hook
  (calendar-mode . denote-journal-calendar-mode)
  (denote-journal . journal-hide-metadata)
  :custom
  (denote-journal-directory nox/journal-directory)
  (denote-journal-title-format 'day-date-month-year))

(use-package focus :commands focus-mode)

(defun nox/org-ctrl-c-ctrl-c ()
  "If in `nox/inbox-file' and in heading with CREATED, create a denote note.
Copy only the subtree body to the kill ring, set the heading to DONE, and signal that the key has been handled."
  (when (and buffer-file-name
             (string-equal (expand-file-name buffer-file-name)
                           (expand-file-name nox/inbox-file)))
    (save-excursion
      (org-back-to-heading t)
      (let* ((element (org-element-at-point))
             (raw-contents-begin (org-element-property :contents-begin element))
             (contents-end       (org-element-property :contents-end element))
             (heading  (org-get-heading t t t t))
             (title    (denote-title-prompt heading))
             (keywords (denote-keywords-prompt))
             (created  (org-entry-get (point) "CREATED"))
             (date     (or (and created (org-time-string-to-time created))
                           (current-time)))
             contents-begin)

        ;; Compute contents-begin that skips a property drawer if present
        (when raw-contents-begin
          (goto-char raw-contents-begin)
          (if (looking-at-p "^[ \t]*:PROPERTIES:")
              (let ((drawer (org-element-at-point)))
                (setq contents-begin (org-element-property :end drawer)))
            (setq contents-begin raw-contents-begin)))

        ;; Copy only if there is actual non-drawer contents
        (when (and contents-begin contents-end
                   (< contents-begin contents-end))
          (kill-new (buffer-substring contents-begin contents-end)))

        ;; Mark heading as DONE
        (org-todo 'done)

        ;; Create denote note
        (denote title keywords nil nil date)
        t))))

(add-hook! org-ctrl-c-ctrl-c #'nox/org-ctrl-c-ctrl-c)

(advice-add 'denote-journal-new-or-existing-entry :before
            (lambda (&rest _args)
              (persp-switch denote-workspace-name)))

(advice-add 'kill-current-buffer :after #'persp-kill-if-no-denote-buffer)

(defun nox/denote-search ()
  "Uses `denote-open-or-create' as its backend and switches workspace."
  (interactive)
  (condition-case nil
      (progn
        (persp-switch denote-workspace-name)
        (call-interactively 'denote-open-or-create))
    ((quit error user-error)
     (persp-kill-if-no-denote-buffer))))

(defun persp-denote-buffer-p (buf)
  "Return non-nil if BUF is a live Denote buffer."
  (when (buffer-live-p buf)
    (let ((name (buffer-name buf)))
      (and (stringp name)
           (string-prefix-p denote-buffer-name-prefix name)))))

(defun persp-kill-if-no-denote-buffer ()
  "Kill the current perspective if it contains no Denote buffers."
  (interactive)
  (let ((curr-persp (persp-curr)))
    (when (and (string= (persp-name curr-persp) denote-workspace-name)
               (not (seq-some #'persp-denote-buffer-p
                              (persp-buffers curr-persp))))
      (persp-kill (persp-name curr-persp))
      (message "Workspace killed because it has no Denote buffer."))))

(defface note-title-face
  '((t :height 1.6
       :weight bold
       :inherit default))
  "Face used for displaying note titles.")

(defface note-subtitle-face
  '((t :inherit shadow
       :italic t
       :weight regular))
  "Face for note subtitle value.")

(defface journal-title-face
  '((t :height 1.6
       :weight bold
       :inherit default))
  "Face used for displaying journal titles.")

(defun denote--buffer-has-attachments-p ()
  (let ((dir (org-attach-dir)))
    (and dir
         (file-directory-p dir)
         (not (null (directory-files dir nil "^[^.].*"))))))

(defun denote-hide-metadata (&rest _)
  "Render metadata as:

           Title
           AUTHOR · DATE · tag1, tag2 · +att"
  (remove-overlays (point-min) (point-max) 'denote-org-meta t)

  (let (title title-beg title-end date tags author)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\([a-zA-Z_]+\\):\\s-*\\(.*\\)$" nil t)
        (let* ((key (downcase (match-string 1)))
               (val (match-string 2))
               (bol (match-beginning 0))
               (eol (1+ (match-end 0))))
          (pcase key
            ("title"
             (setq title val
                   title-beg (match-beginning 2)
                   title-end (match-end 2))
             ;; hide keyword only
             (let ((ov (make-overlay bol title-beg)))
               (overlay-put ov 'invisible t)
               (overlay-put ov 'denote-org-meta t))
             ;; face title
             (let ((ov (make-overlay title-beg title-end)))
               (overlay-put ov 'face 'note-title-face)
               (overlay-put ov 'denote-org-meta t)))

            ("date"
             (setq date (nox/org-format-date val))
             (let ((ov (make-overlay bol eol)))
               (overlay-put ov 'invisible t)
               (overlay-put ov 'denote-org-meta t)))

            ("author"
             (setq author val)
             (let ((ov (make-overlay bol eol)))
               (overlay-put ov 'invisible t)
               (overlay-put ov 'denote-org-meta t)))

            ("filetags"
             (setq tags (nox/org-format-filetags val))
             (let ((ov (make-overlay bol eol)))
               (overlay-put ov 'invisible t)
               (overlay-put ov 'denote-org-meta t)))

            (_
             (let ((ov (make-overlay bol eol)))
               (overlay-put ov 'invisible t)
               (overlay-put ov 'denote-org-meta t)))))))

    ;; Inject composed second line
    (when (and title-end (or date tags author))
      (let* ((parts
              (delq nil
                    (list
                     (when author
                       (propertize author 'face 'note-subtitle-face))
                     (when date
                       (propertize date 'face 'note-subtitle-face))
                     (when tags
                       (propertize tags 'face 'note-subtitle-face))
                     (when (denote--buffer-has-attachments-p)
                       (propertize "‡" 'face 'note-subtitle-face)))))
             (sep (propertize " · " 'face 'shadow))
             (text (string-join parts sep))
             (ov (make-overlay title-end title-end)))
        (overlay-put ov 'after-string (concat "\n" text))
        (overlay-put ov 'denote-org-meta t)))))

(defun denote-show-metadata ()
  "Remove Org metadata overlays."
  (remove-overlays (point-min) (point-max) 'denote-org-meta t))

(add-hook! denote-after-rename-file #'denote-hide-metadata)

(defun nox/org-format-filetags (s)
  "Convert :tag1:tag2:tag3: → tag1, tag2, tag3"
  (string-join
   (seq-filter (lambda (x) (not (string-empty-p x)))
               (split-string s ":" t))
   ", "))

(defun nox/org-format-date (s)
  "Format Org timestamp S like
         [2025-10-23 Thu 11:23] → Thu, 23 Oct 2025 - 11:23"
  (when (string-match
         "\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([A-Za-z]\\{3\\}\\) \\([0-9:]\\{5\\}\\)\\]"
         s)
    (let* ((year  (match-string 1 s))
           (month (string-to-number (match-string 2 s)))
           (day   (match-string 3 s))
           (dow   (match-string 4 s))
           (time  (match-string 5 s))
           (month-name (calendar-month-name month t)))
      (format "%s, %s %s %s - %s"
              dow day month-name year time))))

(defun journal-hide-metadata ()
  "Hide all Org header metadata and show only the raw title text,
styled with `journal-title-face`."
  (remove-overlays (point-min) (point-max) 'journal-meta t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+\\([a-zA-Z_]+\\):[ \t]*\\(.*\\)$" nil t)
      (let* ((key (downcase (match-string 1)))
             (val (match-string 2))
             (beg (match-beginning 0))
             (end (min (1+ (match-end 0)) (point-max))))
        (cond
         ;; Title: hide keyword, show value with face
         ((string= key "title")
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'journal-meta t)
            (overlay-put ov 'invisible t)
            (overlay-put ov 'display
                         (propertize (concat val "\n")
                                     'face 'journal-title-face))))
         ;; Other metadata: hide completely
         (t
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'journal-meta t)
            (overlay-put ov 'invisible t))))))))

(defun journal-show-metadata ()
  "Remove journal metadata overlays."
  (remove-overlays (point-min) (point-max) 'journal-meta t))

(define-minor-mode denote-hide-metadata-mode
  "Minor mode to toggle Denote or journal metadata display.

If the current buffer is a journal file (filename contains \"__journal\"),
use journal metadata functions. Otherwise, use Denote metadata functions."
  :init-value nil
  :interactive '(org-mode)
  (when-let ((file (buffer-file-name)))
    (let* ((is-journal (string-match-p "__journal"
                                       (file-name-nondirectory file)))
           (show-fn (if is-journal
                        #'journal-show-metadata
                      #'denote-show-metadata))
           (hide-fn (if is-journal
                        #'journal-hide-metadata
                      #'denote-hide-metadata)))
      (if denote-hide-metadata-mode
          (funcall hide-fn)
        (funcall show-fn)))))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook
  ;; To hide dot-files by default
  (dired-mode . dired-omit-mode)
  :custom
  ;; hide files/directories starting with "." in dired-omit-mode
  (dired-omit-files (rx (seq bol ".")))
  ;; Enable "do what I mean" for target directories
  (dired-dwim-target t)

  ;; Close the previous buffer when opening a new `dired' instance
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (setq dired-free-space nil
        dired-deletion-confirmer 'y-or-n-p
        dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-deletes 'top
        dired-recursive-copies  'always
        dired-create-destination-dirs 'ask))

(use-package diredfl
  :hook
  ;;(dired-mode . diredfl-mode)
  ;; highlight parent and directory preview as well
  (dirvish-directory-view-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dirvish
  :commands (dirvish dired)
  :hook
  (dired-mode . (lambda () (visual-line-mode -1)))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("D" "~/Documents/"                "Documents")
     ("n" "~/Documents/notes/"          "Notes")
     ("d" "~/Downloads/"                "Downloads")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (delete-by-moving-to-trash t)
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-side-attributes
   '(vc-state file-size nerd-icons collapse))
  (dirvish-reuse-session 'open)
  (dirvish-mode-line-bar-image-width 0)
  (dirvish-mode-line-height 32)
  :config
  (dirvish-override-dired-mode)

  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-open-file)

  (evil-define-key 'normal dirvish-mode-map
    (kbd "?") 'dirvish-dispatch
    (kbd "a") 'dirvish-quick-access
    (kbd "TAB") 'dirvish-subtree-toggle
    (kbd "q") 'dirvish-quit)

  (dirvish-side-follow-mode))

(use-package dirvish-emerge
  :commands (dirvish-emerge-mode)
  :ensure nil
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

(use-package dired-open
  :after dirvish
  :config
  (setq dired-open-extensions '(("gif" . "imv")
                                ("jpg" . "imv")
                                ("webp" . "imv")
                                ("png" . "imv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package elfeed
  :commands (elfeed =elfeed)
  :hook
  (elfeed-show-mode   . variable-pitch-mode)
  (elfeed-show-mode   . olivetti-mode)
  (elfeed-search-mode . olivetti-mode)
  (elfeed-search-mode . (lambda () (evil-goggles-mode -1)))
  :bind
  ([remap elfeed-search-fetch]         . nox/elfeed-refresh)
  ([remap elfeed-search-update--force] . nox/elfeed-refresh)
  ([remap elfeed-search-fetch]         . nox/elfeed-refresh)
  ([remap elfeed-search-show-entry]    . nox/elfeed-show-or-open-in-browser)
  :custom
  (elfeed-search-filter "@1-weeks-ago +unread")
  (elfeed-db-directory (expand-file-name "elfeed" minimal-emacs-user-directory))
  :config
  (require 'nano-elfeed)
  (require 'elfeed-workspace)

  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "j") 'nano-elfeed-next-entry
    (kbd "r") 'nox/elfeed-refresh
    (kbd "k") 'nano-elfeed-prev-entry
    (kbd "s") 'elfeed-toggle-star)

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  ;; add advice cause needed to hit j twice for some reason
  (advice-add 'elfeed-search-untag-all-unread :after #'nano-elfeed-next-entry)
  (advice-add 'elfeed-search-untag-all-unread :after #'evil-force-normal-state)
  (advice-add 'nox/elfeed-show-or-open-in-browser :after #'nano-elfeed-next-entry)

  ;; auto center the screen
  (advice-add 'nano-elfeed-next-entry :after #'good-scroll-center-cursor)
  (advice-add 'nano-elfeed-prev-entry :after #'good-scroll-center-cursor)
  (advice-add 'elfeed-search-untag-all-unread :after #'good-scroll-center-cursor)

  (add-hook! elfeed-search-mode
    (setq-local cursor-type nil
                evil-normal-state-cursor nil
                evil-insert-state-cursor nil)))

(use-package elfeed-protocol
  :after elfeed
  :custom
  (elfeed-use-curl t)
  (elfeed-curl-extra-arguments '("--insecure"))
  (elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-fever-update-unread-only t)
  (elfeed-protocol-fever-fetch-category-as-tag t)
  :config
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable)
  (defconst nox/freshrss-url (nox/get-secret ".identity.freshrss.url"))

  (setq elfeed-protocol-feeds `((,(concat "fever+" nox/freshrss-url)
                                 :api-url
                                 ,(concat nox/freshrss-url "/api/fever.php")
                                 :password
                                 ,(nox/get-secret ".identity.freshrss.pass")))))

(use-package reddigg :commands (reddigg-view-comments))

;; FIXME: update logic to exempt shr-link which already contains text before it
(require 'cl-lib)

(defvar visually-cleanup-skip-faces
  '(shr-link
    message-header-name
    gnus-header-name
    gnus-button)
  "List of text faces that should prevent newline joining.")

(defun visually-cleanup-lines ()
  "Visually join single newlines and collapse multiple spaces into one.
Skips paragraph breaks and lines with '*' or faces in `visually-cleanup-skip-faces` near the newline."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    ;; Collapse multiple spaces into one (visually)
    (while (re-search-forward " \\{2,\\}" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'display " ")
        (overlay-put ov 'invisible t)
        (overlay-put ov 'read-only t)
        (overlay-put ov 'evaporate t)))

    ;; Helper to check if any unwanted face is present
    (defun face-should-skip (face)
      (cond
       ((null face) nil)
       ((symbolp face) (memq face visually-cleanup-skip-faces))
       ((listp face) (cl-some (lambda (f) (memq f visually-cleanup-skip-faces)) face))
       (t nil)))

    (goto-char (point-min))
    ;; Visually join single newlines
    (while (re-search-forward "\\([^\n]\\)\n\\([^\n]\\)" nil t)
      (let* ((pre-pos (match-beginning 1))
             (post-pos (match-beginning 2))
             (pre-char (char-after pre-pos))
             (post-char (char-after post-pos))
             (pre-face (get-text-property pre-pos 'face))
             (post-face (get-text-property post-pos 'face)))

        (unless (or (eq pre-char ?*)
                    (eq post-char ?*)
                    (face-should-skip pre-face)
                    (face-should-skip post-face))
          (let ((newline-pos (1+ pre-pos)))
            (let ((ov (make-overlay newline-pos (1+ newline-pos))))
              (overlay-put ov 'display " ")
              (overlay-put ov 'invisible t)
              (overlay-put ov 'read-only t)
              (overlay-put ov 'evaporate t)))))

      ;; Move forward to prevent infinite loop
      (goto-char (1+ (match-beginning 0))))))

(defun nox/elfeed-refresh ()
  "Refresh elfeed feed along with unread state. Only use inside elfeed search."
  (interactive)
  (mark-whole-buffer)
  (cl-loop for entry in (elfeed-search-selected)
           do (elfeed-untag-1 entry 'unread))
  (elfeed-search-update--force)
  (elfeed-protocol-fever-reinit nox/freshrss-url))

(defun nox/elfeed-show-or-open-in-browser ()
  "Open Reddit-tagged entries in external browser, else show in elfeed.
If opened externally, remove the 'unread' tag from the entry."
  (interactive)
  (let ((entry (elfeed-search-selected :ignore-region)))
    (if (memq 'Reddit (elfeed-entry-tags entry))
        (progn
          (browse-url (elfeed-entry-link entry))
          ;; Remove 'unread' tag from entry
          (elfeed-untag entry 'unread)
          ;; Refresh entry in search buffer
          (elfeed-search-update-entry entry))
      (elfeed-search-show-entry entry))))

(defun nox/elfeed-cleanup-and-exit ()
  "Save elfeed DB, kill all elfeed-related buffers, kill the current buffer, and kill the workspace."
  (interactive)
  ;; Save elfeed database
  (elfeed-db-save)

  ;; Kill all elfeed-related buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'elfeed-search-mode 'elfeed-show-mode)
        (kill-buffer buf))))

  ;; Kill the current workspace (Doom-specific)
  (persp-kill +elfeed-workspace-name))

(use-package transient :defer t)

(use-package gptel
  :commands gptel
  :hook
  (gptel-mode . evil-insert-state)
  (gptel-mode . hide-mode-line-mode)
  :bind* (("C-c RET" . gptel-send))
  :custom
  (gptel-model 'gpt-5-mini)
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist
   '((markdown-mode . "**Prompt:** ")
     (org-mode . "* ")
     (text-mode . "Prompt: ")))
  (gptel-response-prefix-alist
   '((markdown-mode . "**Response:** ")
     (org-mode . "")
     (text-mode . "Response: ")))
  (gptel-api-key (nox/get-secret ".api.openai"))
  :config
  (gptel-make-gemini "Gemini"
                     :key (nox/get-secret ".api.gemini")
                     :stream t)
  (add-hook! gptel-post-stream #'gptel-auto-scroll)
  (add-hook! gptel-post-response-functions #'gptel-end-of-response))

(defun nox/gptel-abort ()
  "If the buffer-name matches *ChatGPT, run `gptel-abort'."
  (when (string-match-p "^\\*ChatGPT" (buffer-name))
    (gptel-abort (current-buffer))
    t))

(add-hook! org-ctrl-c-ctrl-c-final #'nox/gptel-abort)

(use-package helpful
  :hook
  (helpful-mode . (lambda ()
                    (set-window-dedicated-p (selected-window) t)))
  (helpful-mode . hide-mode-line-mode)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap view-hello-file]   . helpful-at-point))

(use-package elisp-demos
  :after helpful
  :custom
  (elisp-demos-user-files (list (expand-file-name "demos.org" nox/emacs-directory)))
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package jinx
  :hook
  (on-first-input . global-jinx-mode)
  :bind* (("C-/" . jinx-correct)))

(defun nox/kindle-first-page ()
  "Go to the first page of the book."
  (interactive)
  (url-retrieve "http://kindle:1337/koreader/event/GoToBeginning" #'ignore))

(defun nox/kindle-next-bookmark ()
  "Go to the next bookmark."
  (interactive)
  (url-retrieve "http://kindle:1337/koreader/event/GotoNextBookmarkFromPage" #'ignore))

(defun nox/kindle-prev-bookmark ()
  "Go to the previous bookmark."
  (interactive)
  (url-retrieve "http://kindle:1337/koreader/event/GotoPreviousBookmarkFromPage" #'ignore))

(defun nox/kindle-next-page ()
  "Go to the next page."
  (interactive)
  (url-retrieve "http://kindle:1337/koreader/event/GotoViewRel/1 " #'ignore))

(defun nox/kindle-prev-page ()
  "Go to the prev page."
  (interactive)
  (url-retrieve "http://kindle:1337/koreader/event/GotoViewRel/-1 " #'ignore))

(use-package eros
  :hook
  (on-first-input . eros-mode))

(use-package nano-read :ensure (:host github :repo "rougier/nano-read"))

(defun nox/set-nano-read-colors ()
  (require 'wid-edit)
  (set-face-attribute 'nano-read-prompt-default-face nil
                      :background (doom-color 'orange)
                      :box 'unspecified)
  (set-face-attribute 'nano-read-prompt-warning-face nil
                      :foreground (doom-color 'base0)
                      :background (doom-color 'red)
                      :box 'unspecified)
  (set-face-attribute 'widget-field nil :box 'unspecified)
  (set-face-attribute 'fringe nil :background 'unspecified))

(add-hook! (nox/after-theme-change on-first-input) #'nox/set-nano-read-colors)

(defun nox/yes-or-no-p (prompt)
  "Use `nano-read-yes-or-no` as backend for `yes-or-no-p`."
  (let ((answer (nano-read-yes-or-no "" prompt)))
    (string-equal answer "YES")))

(fset 'yes-or-no-p 'nox/yes-or-no-p)
(fset 'y-or-n-p    'nox/yes-or-no-p)

(defun nox/capture-todo ()
  "Prompt for a TODO/IDEA/WAIT and its context from `nox/todo-contexts',
then write it to `nox/tasks-file'. The suffix of the description
controls the state:
  - \"!\" → IDEA
  - \"wt\" → WAIT
  - otherwise → TODO"
  (interactive)
  (let* ((todo-pair (nano-read-with-list "TODO" nox/todo-contexts))
         (raw-desc (string-trim (car todo-pair)))
         (tag      (cdr todo-pair))
         (bm-name  "recent-capture")
         (tag-str  (propertize (format "[%s]" tag)
                               'face `(:weight bold :foreground ,(doom-color 'orange))))
         ;; detect state from suffix
         (state (cond
                 ((string-suffix-p "!" raw-desc) "IDEA")
                 ((string-suffix-p "wt" raw-desc) "WAIT")
                 (t "TODO")))
         ;; strip the marker from the description
         (desc (pcase state
                 ("IDEA" (string-remove-suffix "!" raw-desc))
                 ("WAIT" (string-remove-suffix "wt" raw-desc))
                 (_      raw-desc)))
         ;; if there was an "@" in the description, prompt for a date:
         (date (when (string-suffix-p "@" desc)
                 (let ((time (org-read-date nil t nil "Schedule date: ")))
                   (format-time-string "<%Y-%m-%d %a>" time))))
         (desc (if date
                 (string-remove-suffix "@" desc)
                 desc))
         ;; final string
         (desc (string-trim desc))
         (heading (format "* %s %s :%s:\n" state desc tag)))

    (with-current-buffer (find-file-noselect nox/tasks-file)
      (goto-char (point-max))
      (insert heading)
      ;; if user supplied a date, insert a scheduled timestamp
      (when date
        (insert
         (format "  SCHEDULED: %s\n" date)))
      (save-buffer)
      (bookmark-set bm-name))

    ;; final feedback
    (if date
        (message "Added %s: %s %s on %s" state desc tag-str date)
      (message "Added %s: %s %s" state desc tag-str))))

(defun nox/capture-journal ()
  "Prompt for a JOURNAL and its date, then write it to the corresponding dailies file."
  (interactive)
  (require 'org-roam-dailies)
  (let* ((journal-pair (nano-read-with-list "JOURNAL" '("TODAY" "YESTERDAY" "TOMORROW")))
         (journal (car journal-pair))
         (date    (cdr journal-pair))
         (date-offset (cond
                       ((string-equal date "YESTERDAY") -1)
                       ((string-equal date "TOMORROW") 1)
                       (t 0)))
         (journal-date (time-add (current-time) (days-to-time date-offset))))
    (progn
      (org-roam-dailies--capture journal-date nil "d")
      (if (not (string-suffix-p "..." journal))
          (progn
            (insert journal)
            (org-capture-finalize))
        (insert (concat (string-remove-suffix "..." journal) "\n"))))
    (message "Journal entry added.")))

(use-package wasabi
  :commands wasabi
  :ensure (:host github :repo "xenodium/wasabi" :branch "main"))

(defun nox/buffer-regex-builder (names)
  "Return a regex that matches *NAMES* buffers."
  (concat
   "\\*\\("
   (mapconcat #'regexp-quote names "\\|")
   "\\)\\*"))

(setq display-buffer-alist
      `((
         ;; Embark
         ,(nox/buffer-regex-builder '("Embark Collect" "Embark Actions"))
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.5)
         (slot . 1)
         (window-parameters . ((mode-line-format . none))))
        ;; Gptel
        ("\\*ChatGPT"
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.5)
         (slot . 1)
         (window-dedicated . t))
        ;; Chatgpt-Shell
        ("\\*chatgpt"
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.5)
         (slot . 1)
         (window-dedicated . t)
         (window-parameters . ((no-delete-other-windows . nil)
                               (select-window . t))))
        ;; Magit
        ((major-mode . magit-status-mode)
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.5)
         (slot . 1)
         (window-dedicated . t)
         (window-parameters . ((mode-line-format . none))))
        ("*COMMIT_EDITMSG"
         (display-buffer-in-direction)
         (direction . leftmost)
         (window-width . 0.5))
        ((major-mode . magit-diff-mode)
         (display-buffer-in-direction)
         (window . root)
         (direction . right)
         (window-width . 0.5))
        ;; Special Windows
        (,(nox/buffer-regex-builder '("reddigg-comments" "vterm" "eshell" "eldoc" "use-package statistics" "compilation"))
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.4)
         (slot . 1)
         (window-dedicated . t)
         (window-parameters . ((mode-line-format . none))))
        ;; Help windows
        ((or (major-mode . Info-mode)
             (major-mode . helpful-mode)
             (major-mode . help-mode))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (inhibit-same-window . nil)
         (side . bottom)
         (slot . 1)
         (window-height . 0.5))
        ("*Calender*"
         (display-buffer-reuse-window display-buffer-in-direction)
         (direction . bottom)
         (window-dedicated . t)
         (window-height . 0.4))
        ;; Org Mode
        ("CAPTURE-"
         (display-buffer-reuse-window display-buffer-in-direction)
         (direction . bottom)
         (inhibit-same-window . nil)
         (window-height . 0.4))
        ((major-mode . org-mode)
         (display-buffer-reuse-window display-buffer-same-window display-buffer-in-direction)
         (direction . right)
         (window-width . 0.5))
        ;; Elpaca
        ((major-mode . elpaca-log-mode)
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.18)
         (slot . 1)
         (window-parameters . ((mode-line-format . none))))))

;; Deleting and renaming of current file
(use-package bufferfile
  :custom
  (bufferfile-use-vc t)
  (bufferfile-delete-switch-to 'previous-buffer)
  :commands (bufferfile-rename bufferfile-delete))

;; Sudo edit the current file
(use-package sudo-edit
  :commands (sudo-edit-find-file sudo-edit))

(defun nox/run-commands-for-buffer-names ()
  "Run specific commands for certain buffer names."
  (let ((buffer-name (buffer-name)))
    (cond
     ((string= buffer-name "*elfeed-entry*")
      ;; cleanup lines and make olivetti-mode work better
      (visually-cleanup-lines))

     ((string= buffer-name "*elpaca-log*")
      (visual-line-mode -1))

     ((string= buffer-name "*reddigg-comments*")
      (jinx-mode -1)
      (org-appear-mode -1)
      (evil-goto-first-line)
      ;; convert all md links to org links
      (nox/md-to-org-links)
      (nox/md-code-blocks-to-org)
      (nox/md-blockquotes-to-org)
      ;; make the window dedicated
      (set-window-dedicated-p (selected-window) t)
      ;; easier quitting of the window
      (evil-local-set-key 'normal "q" 'kill-current-buffer)
      ;; open all folds
      (org-fold-show-all)
      (read-only-mode)))))

;; Add the function to hooks
(add-hook! buffer-list-update #'nox/run-commands-for-buffer-names)

;; (use-package buffer-terminator
;;   :hook (on-first-input . buffer-terminator-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ibuffer
  :ensure nil
  :commands (ibuffer persp-ibuffer)
  :hook
  (ibuffer-mode . (lambda () (display-line-numbers-mode -1)))
  (ibuffer-mode . (lambda () (visual-line-mode -1))))

(use-package perspective
  :hook
  (on-first-input . persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c b"))
  (persp-initial-frame-name "main")
  (persp-sort 'created)
  :config
  (persp-turn-off-modestring))

(defun +list-workspaces ()
  "List all workspaces, numbering them and highlighting the current one."
  (interactive)
  (let* ((all-persp (persp-names))
         (current (persp-name (persp-curr)))
         (msg (mapconcat
               (lambda (p)
                 (let* ((i (1+ (cl-position p all-persp :test #'equal)))
                        (label (format "[%d] %s" i p)))
                   (if (equal p current)
                       (propertize label 'face `(:weight bold :foreground ,(doom-color 'orange)))
                     label)))
               all-persp
               " ")))  ; <-- just space between items
    (message "Workspaces: %s" msg)))

(add-hook! persp-switch #'+list-workspaces)

(defun +workspace-switch-advice (&rest _)
       (if (= (length (persp-names)) 1)
           (message "No other workspace.")))

(advice-add 'persp-next :before #'+workspace-switch-advice)
(advice-add 'persp-prev :before #'+workspace-switch-advice)

(defun +project-workspace-create (&rest _)
  (persp-switch (file-name-nondirectory (directory-file-name (projectile-acquire-root))))
  (projectile-find-file)
  (+list-workspaces))

(setq projectile-switch-project-action '+project-workspace-create)

;; truncate line with …
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
;; wrap line with —
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(set-face-attribute 'variable-pitch nil
                    :family "Inter"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :family "CaskaydiaCove Nerd Font"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'default nil
                    :family "CaskaydiaCove Nerd Font"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'fixed-pitch-serif nil
                    :inherit 'fixed-pitch
                    :family 'unspecified)

(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-14"))

(defun nox/set-fonts ()
  "Set fonts and face attributes."
  ;; setting the emoji font family
  ;; https://emacs.stackexchange.com/a/80186
  (set-fontset-font t 'emoji
                    '("Apple Color Emoji" . "iso10646-1") nil 'prepend)

  ;; italic comments and keywords
  (set-face-attribute 'font-lock-comment-face nil :italic t)

  ;; setting the line spacing
  (setq-default line-spacing 0.16))

(add-hook! on-first-input #'nox/set-fonts)

(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 110))

(use-package ultra-scroll
  :hook (on-first-input . ultra-scroll-mode)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin more than 0 not yet supported
  :config
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'diff-hl-flydiff-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode)
  (add-hook 'ultra-scroll-hide-functions #'good-scroll-mode))

(use-package good-scroll
  :hook (on-first-input . good-scroll-mode)
  :bind
  ([remap evil-scroll-up] . good-scroll-down-half-screen)
  ([remap evil-scroll-line-to-center] . good-scroll-center-cursor)
  ([remap evil-scroll-down] . good-scroll-up-half-screen)
  :config

  (defun good-scroll-center-cursor ()
    "Scroll cursor to center."
    (interactive)
    (let* ((pixel-y (cdr (posn-x-y (posn-at-point))))               ; cursor vertical position
           (half-window (/ (good-scroll--window-usable-height) 2))  ; half of usable window height
           (delta (- pixel-y half-window)))                         ; difference from center
      (good-scroll-move delta)))

  (defun good-scroll-up-half-screen ()
    "Scroll up by half screen."
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

  (defun good-scroll-down-half-screen ()
    "Scroll down by half screen."
    (interactive)
    (good-scroll-move (- (/ (good-scroll--window-usable-height) 2)))))

(use-package spacious-padding
  :hook
  (on-init-ui . spacious-padding-mode))

(use-package evil-goggles
  :hook (on-first-file . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package doom-modeline
  :hook
  (on-init-ui . doom-modeline-mode)
  :init
  (setq doom-modeline-support-imenu t)
  :config
  (setq doom-modeline-major-mode-icon nil)
  (setq find-file-visit-truename t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-height 36))

(use-package hide-mode-line :commands hide-mode-line-mode)

;; (use-package nano-modeline
;;   :hook
;;   (prog-mode            . nano-modeline-prog-mode)
;;   (text-mode            . nano-modeline-text-mode)
;;   (org-mode             . nano-modeline-org-mode)
;;   (pdf-view-mode        . nano-modeline-pdf-mode)
;;   (mu4e-headers-mode    . nano-modeline-mu4e-headers-mode)
;;   (mu4e-view-mode       . nano-modeline-mu4e-message-mode)
;;   (mu4e-compose-mode    . nano-modeline-mu4e-compose-mode)
;;   (elfeed-show-mode     . nano-modeline-elfeed-entry-mode)
;;   (elfeed-search-mode   . nano-modeline-elfeed-search-mode)
;;   (elpher-mode          . nano-modeline-elpher-mode)
;;   (term-mode            . nano-modeline-term-mode)
;;   (eat-mode             . nano-modeline-eat-mode)
;;   (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
;;   (messages-buffer-mode . nano-modeline-message-mode)
;;   (org-capture-mode     . nano-modeline-org-capture-mode)
;;   (org-agenda-mode      . nano-modeline-org-agenda-mode)
;;   :init
;;   (nano-modeline-prog-mode t)
;;   :config
;;   (dolist (spec '((status-**-active nox/buffer-modified-status nil)
;;                   (name-active      nox/buffer-name-active)))
;;     (setf (cdr (assoc (car spec) nano-modeline-faces))
;;           (cdr spec))))
;;
;; (setq-default mode-line-format "")
;;
;; (make-face 'nox/buffer-modified-status)
;; (make-face 'nox/buffer-name-active)
;;
;; (defun nox/set-modeline-faces ()
;;   (set-face-attribute 'nano-modeline-button-active-face nil
;;                       :box `(:line-width 2 :color ,(doom-color 'base3) :style flat-button)
;;                       :foreground (doom-color 'bg)
;;                       :background (doom-color 'orange))
;;
;;   (set-face-attribute 'nano-modeline-button-inactive-face nil
;;                       :box `(:line-width 2 :color ,(doom-color 'base3) :style flat-button)
;;                       :foreground (doom-color 'bg)
;;                       :background (doom-color 'grey))
;;
;;   (set-face-attribute 'nano-modeline-active nil
;;                       :inherit nil
;;                       :box `(:line-width 1 :color ,(doom-color 'base3))
;;                       :foreground (doom-color 'grey)
;;                       :background (doom-color 'base3))
;;
;;   (set-face-attribute 'nano-modeline-status nil
;;                       :foreground (doom-color 'bg)
;;                       :background (doom-color 'dark-cyan))
;;
;;   (set-face-attribute 'nox/buffer-modified-status nil
;;                       :foreground (doom-color 'bg)
;;                       :background (doom-color 'orange))
;;
;;   (set-face-attribute 'nox/buffer-name-active nil
;;                       :inherit 'bold
;;                       :foreground (doom-color 'fg))
;;
;;   (set-face-attribute 'mode-line nil
;;                       :height 1
;;                       :background (doom-color 'bg)
;;                       :underline (doom-color 'base3))
;;
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :height 1
;;                       :background (doom-color 'bg)
;;                       :underline (doom-color 'base3)))
;;
;; (add-hook! (nox/after-theme-change on-init-ui) #'nox/set-modeline-faces)

(let ((theme-file (expand-file-name "~/.cache/theme-status")))
  (setq doom-theme
        (if (and (file-exists-p theme-file)
                 (with-temp-buffer
                   (insert-file-contents theme-file)
                   (string-match-p "light" (buffer-string))))
            'doom-gruvbox-light ;; light theme
          'doom-gruvbox)))      ;; fallback theme or dark theme

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (add-hook! on-init-ui #'nox/theme-setup))

(defun nox/theme-setup()
  (load-theme doom-theme t)
  (doom-themes-org-config))

(use-package org
  :ensure nil
  :defer t
  :init
  ;; Using RETURN to follow links in Org/Evil
  ;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
  ;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)
  :hook
  ;; (org-mode . org-indent-mode)
  (org-mode . prettify-symbols-mode)
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  (org-capture-mode . evil-insert-state)
  (org-mode . org-fold-hide-drawer-all)
  (org-mode . (lambda ()
                (add-hook! before-save :local #'org-update-all-dblocks)))
  :custom
  (org-ellipsis "...")
  (org-confirm-babel-evaluate nil)
  (org-M-RET-may-split-line nil)
  (org-startup-with-latex-preview nil)
  (org-attach-id-dir "attachments/")
  (org-attach-use-inheritance t)
  (org-attach-method 'mv)
  (org-startup-with-link-previews t)
  (org-hide-drawer-startup t)
  (org-image-align 'center)
  (org-image-actual-width nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-support-shift-select t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  :config
  (nox/set-org-faces)
  (require 'inbox-notes-dblock))

(defun nox/set-org-faces ()
  (set-face-attribute 'org-ellipsis nil
                      :foreground 'unspecified
                      :inherit '(org-meta-line))
  (set-face-attribute 'org-special-keyword nil
                      :foreground 'unspecified
                      :inherit '(org-meta-line))
  (set-face-attribute 'org-drawer nil
                      :foreground 'unspecified
                      :inherit '(org-meta-line))
  (set-face-attribute 'org-quote nil
                      :family 'unspecified
                      :slant 'italic
                      :inherit '(variable-pitch org-block)))

(defun nox/org-collapse-done-subtrees ()
  "Fold all Org subtrees marked DONE."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-fold-hide-subtree))
   "/DONE"
   'file))

(use-package org-agenda
  :ensure nil
  :after org
  :commands org-agenda
  :hook
  (org-agenda-mode . olivetti-mode)
  (org-agenda-mode . hide-mode-line-mode)
  :bind
  (:map org-agenda-mode-map
        ("<escape>" . org-agenda-quit))
  :custom
  (org-log-done 'time)
  (org-log-reschedule 'note)
  (org-log-redeadline 'note)
  (org-log-into-drawer t)
  ;; (org-agenda-files (list nox/tasks-file nox/events-file nox/holidays-file))
  (org-agenda-files (list nox/tasks-file))
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-agenda-current-time-string "← now ──────────")
  (org-agenda-start-on-weekday 0)
  (org-agenda-weekend-days '(6))
  :config
  (set-face-attribute 'org-scheduled nil
                      :foreground (doom-color 'fg))

  (setq org-agenda-prefix-format
        '((agenda . "  %i %?-12t")
          (todo   . "  %i %?-12t")
          (tags   . "  %i %?-12t")
          (search . "  %i %?-12t")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IDEA(i@)" "WAIT(w@/!)" "EVNT(e)" "|" "DONE(d!)" "CANC(c@)")))

  (setq org-agenda-category-icon-alist
        '(("tasks"     ("") nil nil :ascent center)
          ("holidays"  ("") nil nil :ascent center)
          ("events"    ("") nil nil :ascent center)))

  (add-hook! org-agenda-mode
    (setq-local cursor-type nil
                evil-normal-state-cursor nil
                evil-insert-state-cursor nil)))

(use-package org-fragtog
  :defer t
  :hook
  (org-mode . (lambda ()
                (add-hook! evil-insert-state-entry :local #'org-fragtog-mode)
                (add-hook! evil-insert-state-exit  :local #'org-latex-preview)
                (add-hook! evil-insert-state-exit  :local (org-fragtog-mode -1)))))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list
   '("◉" "◈" "○" "▷"))
  (org-superstar-cycle-headline-bullets nil)
  (org-superstar-remove-leading-stars t)
  ;; 42 = *
  ;; 43 = +
  ;; 45 = -
  (org-superstar-item-bullet-alist '((42 . 8226) (43 . 10148) (45 . 8226)))
  :config
  (set-face-attribute 'org-superstar-leading nil :height 1.3)
  (set-face-attribute 'org-superstar-header-bullet nil
                      :height 1.2
                      :inherit 'fixed-pitch)
  (set-face-attribute 'org-superstar-item nil :height 1.2))

(setq-default prettify-symbols-alist
              '(("#+begin_src emacs-lisp" . "")
                ("#+begin_src elisp" . "")
                ("#+begin_src nix" . "")
                ("#+begin_src shell" . "")
                (":ATTACH:" . "🔗")
                ("#+attr_org:" . "")
                ;; better start and end
                ("#+begin_src" . "»")
                ("#+end_src" . "«")
                ("#+BEGIN:" . "»")
                ("#+END:" . "«")
                ("#+begin_example" . "»")
                ("#+end_example" . "«")
                ;; quote
                ("#+begin_quote" . "")
                ("#+end_quote" . "")
                ;; babel
                ("#+RESULTS:" . "󰥤")
                (":tangle" . "󰯊")
                (":mkdirp yes" . "")
                ;; elisp
                ("lambda" . "󰘧")
                ("(interactive)" . "")))

  (setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package org-appear
  :defer t
  :hook
  (org-mode . (lambda ()
                (org-appear-mode)
                (add-hook! evil-insert-state-entry :local #'org-appear-manual-start)
                (add-hook! evil-insert-state-exit  :local #'org-appear-manual-stop)))
  :custom
  (org-appear-autolinks t)
  (org-appear-trigger 'manual))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("nix" . "src nix"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ([remap bookmark-jump] . consult-bookmark)
  ([remap evil-show-marks] . consult-mark)
  ([remap evil-show-registers] . consult-register)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap Info-search] . consult-info)
  ([remap locate] . consult-locate)
  ([remap load-theme] . consult-theme)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop)
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)

  :config
  ;; persp with consult
  (with-eval-after-load 'perspective
    (consult-customize consult-source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources 'persp-consult-source))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-default-command #'consult-dir-dired)
  :config
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)

  ;; A function that returns a list of directories
  (defun consult-dir--work-dirs ()
    "Return list of work dirs."
    (append
     (split-string (shell-command-to-string "find ~/.config -maxdepth 1 -type d") "\n" t)))

  ;; A consult source that calls this function
  (defvar consult-dir--source-work
    `(:name     "Work Directories"
                :narrow   ?w
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (executable-find "find"))
                :items    ,#'consult-dir--work-dirs)
    "Work directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-work t))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind*
  (("C-'" . embark-act)
   ("C-;" . embark-dwim))
  (:map embark-general-map
        ("G" . gptel-send)
        ("?" . gptel-quick))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :ensure nil
  :custom
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook! minibuffer-setup #'cursor-intangible-mode))

(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :hook (on-first-input . marginalia-mode))

(use-package nerd-icons-completion
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :hook
  (on-first-input . vertico-mode)
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j"      . vertico-next)
              ("C-M-j"    . vertico-next-group)
              ("C-k"      . vertico-previous)
              ("C-M-k"    . vertico-previous-group)
              ("M-RET"    . vertico-exit-input)
              ("<escape>" . minibuffer-keyboard-quit))
  :config
  ;; Add » before the selected completion.
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
	          ("DEL" . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :ensure nil
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and
                 (memq sym minor-mode-list)
                 (boundp sym)
                 (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))

(use-package sops
  :hook
  (yaml-ts-mode . sops-mode)
  :bind
  (:map yaml-ts-mode-map
        ("C-c C-c" . sops-save-file)
        ("C-c C-k" . sops-cancel)
        ("C-c C-d" . sops-edit-file)))

(use-package corfu
  :hook
  (on-first-input . global-corfu-mode)
  (prog-mode . corfu-mode)
  (shell-mode . corfu-mode)
  (eshell-mode . corfu-mode)
  :custom
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-auto-prefix 2)               ;; Enable auto completion
  (corfu-auto-delay 0.24)             ;; Enable auto completion
  (corfu-preview-current 'insert)     ;; Disable current candidate preview
  (corfu-on-exact-match nil)          ;; Configure handling of exact matches
  (corfu-scroll-margin 5)             ;; Use scroll margin
  (corfu-quit-at-boundary 'separator) ;; Quit completion unless seperator
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator))
  :config
  (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch)
  (nox/set-corfu-colors)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

(defun nox/set-corfu-colors ()
  (when (featurep 'corfu)
    (set-face-attribute 'corfu-current nil :background (doom-color 'bg-alt))))

(add-hook! nox/after-theme-change #'nox/set-corfu-colors)

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-history
  :after (corfu savehist)
  :ensure nil
  :hook
  (corfu-mode . corfu-history-mode)
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

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)      ;; word completion from buffer
  (add-hook 'completion-at-point-functions #'cape-file)         ;; file name completion
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package eglot
  :ensure nil
  :hook
  (prog-mode . eglot-ensure)
  :commands
  (eglot-ensure
   eglot-rename
   eglot-format-buffer)
  :bind
  ([remap eldoc-doc-buffer] . eldoc-box-help-at-point)
  :config
  (add-to-list
   'eglot-server-programs
   '(python-ts-mode . ("rass" "python")))

  ;; ensure rass is installed
  (nox/mason-ensure '("rass"))

  (set-face-attribute 'eglot-inlay-hint-face nil
                      :inherit 'font-lock-comment-face
                      :italic t))

(nox/mason-ensure 'python-ts-mode '("ruff" "ty"))

(use-package markdown-mode :defer t)

(use-package eldoc-box :commands eldoc-box-help-at-point)

(use-package sideline-flymake
  :hook
  (flymake-mode . sideline-mode)
  (sideline-mode . nox/sideline-remove-flymake-eldoc)
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake)))

(defun nox/sideline-remove-flymake-eldoc ()
  (setq-local eldoc-documentation-functions (remove #'flymake-eldoc-function eldoc-documentation-functions)))

(use-package yasnippet
  :hook
  (on-first-file . yas-global-mode)
  (eglot--managed-mode . nox/update-capf-eglot))

(use-package snippy
  :ensure (:host github :repo "MiniApollo/snippy" :branch "main" :rev :newest)
  :hook (yas-global-mode . global-snippy-minor-mode)
  :custom
  (snippy-global-languages '("global"))
  :config
  (snippy-install-or-update-snippets)
  (add-hook 'completion-at-point-functions #'snippy-capf))

(use-package yasnippet-capf
  :after cape
  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(defun cape-eglot-yasnippet ()
  (cape-wrap-super #'eglot-completion-at-point #'yasnippet-capf #'snippy-capf))

(defun nox/update-capf-eglot ()
  "Adds snippets completion to eglot."
  (remove-hook! 'completion-at-point-functions :local #'eglot-completion-at-point)
  (add-hook! 'completion-at-point-functions :local #'cape-eglot-yasnippet))

(use-package apheleia
  :commands apheleia-mode
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

(use-package diff-hl
  :hook
  (on-first-file . global-diff-hl-mode)
  (on-first-file . diff-hl-flydiff-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-ask-before-revert-hunk nil))

(defun nox/set-diff-hl-faces ()
  (when (featurep 'diff-hl)
    (set-face-attribute 'diff-hl-change nil
                        :foreground (doom-color 'yellow)
                        :background 'unspecified)

    (set-face-attribute 'diff-hl-insert nil
                        :foreground (doom-color 'green)
                        :background 'unspecified)

    (set-face-attribute 'diff-hl-delete nil
                        :foreground (doom-color 'red)
                        :background 'unspecified)))

(add-hook! (nox/after-theme-change on-first-file) #'nox/set-diff-hl-faces)

(use-package transient)
(use-package magit
  :hook
  (git-commit-mode . (lambda () (mixed-pitch-mode -1)))
  (git-commit-mode . evil-insert-state)
  :commands magit
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package consult-gh
  :commands (consult-gh-repo-create consult-gh-repo-delete)
  :hook
  (consult-gh-repo-post-clone . projectile-discover-projects-in-directory)
  :custom
  (consult-gh-default-clone-directory nox/projects-directory)
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-confirm-name-before-fork nil)
  (consult-gh-confirm-before-clone t)
  (consult-gh-notifications-show-unread-only nil)
  (consult-gh-default-interactive-command #'consult-gh-transient)
  (consult-gh-prioritize-local-folder 'nil)
  (consult-gh-group-dashboard-by :reason)
  ;;;; Optional
  (consult-gh-repo-preview-major-mode nil) ; show readmes in their original format
  (consult-gh-preview-major-mode 'org-mode) ; use 'org-mode for editing comments, commit messages, ...
  :config
  ;; Remember visited orgs and repos across sessions
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
  (consult-gh-enable-default-keybindings))

(use-package ligature
  :hook (on-first-input . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode org-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@"
                            "~=" "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "=>" "!="
                            "!!" ">:" "\\\\" "://" "..<" "</>" "###" "#_(" "<<<" "<+>"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "<--" "<-<" "<<=" "<<-")))

(use-package projectile
  :commands (+switch-or-make-project)
  :hook
  (on-init-ui . projectile-mode)
  :custom
  (projectile-project-search-path (list nox/projects-directory)))

(defun nox/create-git-repo (project-name)
  "Create a new Git repository under `nox/projects-directory` named PROJECT-NAME.
        If the directory already exists, signal a user error."
  (interactive "sProject name: ")
  (let* ((root   (file-name-as-directory
                  (expand-file-name nox/projects-directory)))
         (target (expand-file-name project-name root)))
    (when (file-exists-p target)
      (user-error "Directory %S already exists" target))
    ;; Create the directory tree
    (make-directory target t)
    ;; Run git init
    (let ((default-directory target))
      (unless (= 0 (call-process "git" nil "*git-init*" t "init"))
        (delete-directory target t)
        (error "git init failed; removed %S" target)))
    (message "Initialized empty Git repository in %S" target)))

(defun nox/create-projectile-project (project-name)
  "Create a new Projectile project under `nox/projects-directory`.
      Makes a fresh directory named PROJECT-NAME and an empty `.projectile` file in it."
  (interactive "sProject name: ")
  (let* ((root   (file-name-as-directory
                  (expand-file-name nox/projects-directory)))
         (target (expand-file-name project-name root)))
    (when (file-exists-p target)
      (user-error "Directory %S already exists" target))
    ;; create directory tree
    (make-directory target t)
    ;; create an empty .projectile file
    (let ((proj-file (expand-file-name ".projectile" target)))
      (with-temp-file proj-file
        ;; insert default ignores or whatever you like, e.g.:
        ;; (insert "# Add patterns to include/ignore in this project\n")
        ))
    (message "Created Projectile project in %S" target)))

(defun nox/create-project (project-name)
  "Interactively create a new project called PROJECT-NAME."
  (interactive "sProject name: ")
  (require 'consult)
  (let* ((choices (list
                   (cons "Create a new git repo on Github" :remote)
                   (cons "Create a new git repo locally" :local)
                   (cons "Create only a project dir" :project)))
         (prompt   "What would you like to do? ")
         (answer   (consult--read choices
                                  :prompt prompt
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))
    (pcase answer
      (:remote  (consult-gh-repo-create project-name))
      (:local   (nox/create-git-repo project-name))
      (:project (nox/create-projectile-project project-name)))))

(defun +switch-or-make-project (&optional arg)
  "Switch to a project from known projects, or create a new one with `consult-gh-repo-create`."
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (if (member project projects)
                       ;; Existing project → switch to it
                       (projectile-switch-project-by-name project arg)
                     ;; New project → create new project and switch to it
                     (progn
                       (nox/create-project project)
                       (projectile-discover-projects-in-search-path)
                       (projectile-switch-project-by-name project arg)))))
      (user-error "There are no known projects"))))

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

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :hook (on-first-input . global-treesit-auto-mode)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  :hook ((eglot--managed-mode yaml-mode) . indent-bars-mode))

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
(add-hook 'post-command-hook #'smart-electric-indent-mode)

(use-package eshell
  :commands eshell
  :ensure nil
  :config
  (setq eshell-rc-script    (expand-file-name "eshell/profile" nox/emacs-directory)
        eshell-aliases-file (expand-file-name "eshell/aliases" nox/emacs-directory)
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

  (add-hook! eshell-mode  #'hide-mode-line-mode))

(use-package vterm
  :commands vterm
  :bind
  (:map vterm-mode-map
        ("C-\\" . window-toggle-side-windows))
  :custom
  (vterm-shell "/usr/bin/zsh")
  :hook
  (vterm-mode . hide-mode-line-mode)
  (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  (vterm-mode . (lambda () (hl-line-mode -1))))

(use-package undo-fu
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

(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
                        (- total-time init-time)) 'face 'shadow))))

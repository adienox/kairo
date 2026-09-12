;;; config-git.el --- git setup -*- lexical-binding: t; -*-

(require 'config-keybinds)
(+config/leader-menu! "git" "g")

(use-package transient)
(use-package magit
  :commands magit
  :hook
  (git-commit-mode . (lambda () (mixed-pitch-mode -1)))
  (git-commit-mode . evil-insert-state)
  :general
  (+config/leader-git
    "P" '(magit-push          :wk "Push repo")
    "c" '(magit-commit-create :wk "Git commit")
    "g" '(magit-status        :wk "Magit"))
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons))

;; (use-package magit-difftastic
;;   :ensure (:host github :repo "rschmukler/magit-difftastic")
;;   :after magit
;;   :demand t
;;   :config
;;   (magit-difftastic-mode))

(use-package diff-hl
  :hook
  (prog-mode    . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode   . diff-hl-dired-mode)
  (focus-in     . diff-hl-update)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :general
  (+config/leader-git
    "n" '(diff-hl-next-hunk     :wk "Next hunk")
    "p" '(diff-hl-previous-hunk :wk "Previous hunk")
    "u" '(diff-hl-revert-hunk   :wk "Undo hunk")
    "s" '(diff-hl-stage-dwim    :wk "Stage hunk"))
  :custom
  (diff-hl-update-async t)
  (diff-hl-show-staged-changes nil)
  (diff-hl-flydiff-delay 0.5)
  (diff-hl-ask-before-revert-hunk nil))

(defun +config/set-diff-hl-faces ()
  (when (featurep 'diff-hl)
    (set-face-attribute 'diff-hl-insert nil
                        :foreground (face-foreground 'success nil t))

    (set-face-attribute 'diff-hl-change nil
                        :background 'unspecified
                        :foreground (face-foreground 'font-lock-builtin-face nil t))

    (set-face-attribute 'diff-hl-delete nil
                        :foreground (face-foreground 'error nil t))

    (set-face-attribute 'diff-added nil
                        :background 'unspecified)

    (set-face-attribute 'diff-removed nil
                        :background 'unspecified)))

(add-hook! (+config/after-theme-change on-first-file) #'+config/set-diff-hl-faces)

(defun +config/diff-hl-update-all-buffers ()
  "Run `diff-hl-update' in every buffer where `diff-hl-mode' is enabled."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p diff-hl-mode)
        (diff-hl-update)))))

(use-package consult-gh
  :disabled
  :after consult
  :demand t
  :general
  (+config/leader-git
    "l" '(consult-gh-repo-list :wk "List repo"))
  :custom
  (consult-gh-default-clone-directory +config/projects-directory)
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-confirm-name-before-fork nil)
  (consult-gh-confirm-before-clone t)
  (consult-gh-notifications-show-unread-only nil)
  (consult-gh-default-interactive-command #'consult-gh-transient)
  (consult-gh-prioritize-local-folder nil)
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


;; Install `consult-gh-embark' for embark actions
(use-package consult-gh-embark
  :after (:all consult consult-gh)
  :demand t
  :config
  (consult-gh-embark-mode +1))

(provide 'config-git)

;;; config-git.el ends here

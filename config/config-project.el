;;; config-project.el --- projects setup -*- lexical-binding: t; -*-

(require 'config-keybinds)
(require 'project-init)
(+config/leader-menu! "workspaces" "TAB")
(+config/leader-menu! "project" "p")

(use-package tab-bar
  :ensure nil
  :general
  (+config/leader-workspaces
    "[" '(tab-previous :wk "Previous tab")
    "]" '(tab-next     :wk "Next tab")
    "r" '(tab-rename   :wk "Rename tab")
    "u" '(winner-undo  :wk "winner undo")
    "U" '(winner-redo  :wk "winner redo")
    "d" '(+config/kill-project-or-tab :wk "Kill project/tab")
    "s" '(tab-bar-switch-to-tab       :wk "Switch tab")
    "TAB" '(+config/tabs-list         :wk "List tabs"))
  :custom
  (tab-bar-show nil)
  :config
  (advice-add 'tab-new      :after #'+config/tabs-list)
  (advice-add 'tab-close    :after #'+config/tabs-list)
  (advice-add 'tab-previous :after #'+config/tabs-list)
  (advice-add 'tab-next     :after #'+config/tabs-list))

(defun +config/tab-buffers (tab-name)
  "Return all live buffers whose +config/tab-name equals TAB-NAME."
  (seq-filter
   (lambda (buf)
     (and (buffer-live-p buf)
          (equal (buffer-local-value '+config/tab-name buf) tab-name)))
   (buffer-list)))

;;;###autoload
(defun +config/tabs-list ()
  "Display all tabs as a numbered string, highlighting the current one."
  (interactive)
  (let* ((tabs (tab-bar-tabs))
         (current-tab (tab-bar--current-tab-find tabs))
         (result
          (mapconcat
           (lambda (tab)
             (let* ((i (1+ (cl-position tab tabs)))
                    (name (alist-get 'name tab))
                    (label (format "[%d] %s" i name)))
               (if (eq tab current-tab)
                   (propertize
                    label
                    'face `(:weight bold
                                    :foreground ,(face-foreground 'font-lock-builtin-face nil t)))
                 label)))
           tabs
           " ")))
    (message "%s" result)))

(add-hook! 'tab-bar-tab-post-open-functions (switch-to-buffer "*scratch*"))

;;;###autoload
(defun +config/tab-close-and-kill-buffers ()
  "Kill buffers shown in the current tab, then close the tab."
  (interactive)
  (let ((buffers-to-kill
         (delete-dups
          (mapcar #'window-buffer
                  (window-list (selected-frame) 'no-minibuf)))))
    (dolist (buf buffers-to-kill)
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (tab-close)))

;;;###autoload
(defun +config/kill-project-or-tab ()
  "Kill project buffers if inside a project; otherwise close the current tab."
  (interactive)
  (if (project-current nil default-directory)
      (project-kill-buffers)
    (+config/tab-close-and-kill-buffers)))

(use-package project
  :commands (project-switch-project project-forget-project)
  :autoload (project-prompt-project-dir)
  :general
  (+config/leader-project
    "r" '(project-forget-project :wk "Remove project")
    "C" '(+config/create-project :wk "Create project")
    "s" '(project-switch-project :wk "Switch project"))
  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands
   (list
    '(consult-project-extra-find "Find file" "f")
    '(consult-ripgrep "Grep" "r")
    '(magit-status "Magit" "m")
    '(ghostel-project "Shell" "s")
    '(tab-close "Quit Workspace" "q"))))

(defun +config/add-project-vc-root-markers (markers)
  "Add MARKERS (a string or list of strings) to `project-vc-root-markers'."
  (interactive
   (list (read-string "Marker (or comma-separated markers): ")))
  (let ((items (if (stringp markers) (list markers) markers)))
    (with-eval-after-load 'project
      (setq project-vc-extra-root-markers
            (delete-dups (append project-vc-extra-root-markers items))))))

(use-package project-x
  :ensure (:host github :repo "vmargb/project-x")
  :hook
  (on-first-input . project-x-mode)
  (project-x-mode . project-x-tabs-mode)
  :general
  (+config/leader-project
    "R" '(project-x-window-state-load :wk "Restore project state")
    "S" '(project-x-window-state-save :wk "Save project state"))
  :custom
  (project-x-tab-find-file-integration t)
  (project-x-tab-kill-buffers-on-close)
  (project-x-default-tab-name "home"))

(use-package consult-project-extra
  :commands consult-project-extra-find
  :general
  (+config/leader-key
    "SPC" '(+config/find-file :wk "Find file in project/notes"))
  :custom (consult-project-function #'consult-project-extra-project-fn))

(defun +config/find-file ()
  (interactive)
  (if (equal +config/tab-name "notes")
      (call-interactively 'denote-open-or-create)
    (consult-project-extra-find)))

(provide 'config-project)

;;; config-project.el ends here

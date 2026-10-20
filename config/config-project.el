;;; config-project.el --- projects setup -*- lexical-binding: t; -*-

(require 'config-keybinds)
(+config/leader-menu! "workspaces" "TAB")
(+config/leader-menu! "project" "p")

(with-eval-after-load 'consult
  (require 'project-init))

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

(defun +config/tab-open-scratch (_tab)
  "Open *scratch* when a genuinely new tab is created.
A frame's initial/default tab is created lazily the first time
`tab-bar-tabs' runs for that frame (see `+config/tab-rename-initial');
that one is skipped so a newly created frame inherits the buffer of
the frame it was made from instead of jumping to *scratch*."
  (when (> (length (tab-bar-tabs)) 1)
    (switch-to-buffer "*scratch*")))

(add-hook! 'tab-bar-tab-post-open-functions #'+config/tab-open-scratch)

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
    '(agent-shell "Agent Shell" "a")
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

;; `project-x--tab-set-default-name' only renames the *selected* frame's
;; lone tab, and only when `project-x-tabs-mode' turns on or a frame is
;; made.  In daemon mode the client frame can be created before the mode
;; is enabled (on-first-input runs during daemon startup on the daemon's
;; own frame), so the client frame is missed and its tab falls back to
;; the buffer name, e.g. "*scratch*".  Rename every frame's lone,
;; project-less tab explicitly instead.
(defun +config/tab-rename-initial (&optional frame)
  "Name FRAME's lone, project-less tab with `project-x-default-tab-name'.
Does nothing when the frame has more than one tab, when its tab holds a
project, or when the tab was named explicitly.  FRAME defaults to the
selected frame."
  (when (and (bound-and-true-p project-x-tabs-mode)
             (boundp 'project-x-default-tab-name)
             project-x-rename-initial-tab)
    (let* ((frame (or frame (selected-frame)))
           (name (if (functionp project-x-default-tab-name)
                     (funcall project-x-default-tab-name)
                   project-x-default-tab-name)))
      ;; `tab-bar-tabs' creates FRAME's default tab, which runs
      ;; `tab-bar-tab-post-open-functions'.  This config's hook switches
      ;; to *scratch*; select FRAME first so that switch lands on FRAME,
      ;; not on whichever frame happens to be selected (e.g. the previous
      ;; frame while a new one is being made).
      (with-selected-frame frame
        (let* ((tabs (funcall tab-bar-tabs-function frame))
               (tab (assq 'current-tab tabs)))
          (when (and name
                     (length= tabs 1)
                     tab
                     (not (alist-get 'project-x-root-dir tab))
                     (not (eq (alist-get 'explicit-name tab) t)))
            (setcdr (assq 'name tab) name)
            (setcdr (assq 'explicit-name tab) 'project-x-def)
            (force-mode-line-update t)))))))

(defun +config/tab-rename-initial-all (&optional _frame)
  "Apply `+config/tab-rename-initial' to every live, normal frame.
Child frames (posframe, corfu, etc.) and tooltips are skipped so we
don't create a tab bar entry on them."
  (dolist (frame (frame-list))
    (unless (or (frame-parameter frame 'parent-frame)
                (frame-parameter frame 'tooltip))
      (ignore-errors (+config/tab-rename-initial frame)))))

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
  (project-x-default-tab-name "home")
  :config
  ;; Catch both new frames and frames that already existed when the mode
  ;; came on (the daemon-mode race described above).
  (add-hook 'after-make-frame-functions #'+config/tab-rename-initial-all)
  (add-hook 'project-x-tabs-mode-hook #'+config/tab-rename-initial-all))

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

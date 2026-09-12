;;; project-init.el --- Project initialization helpers -*- lexical-binding: t; -*-

(defcustom +config/project-init-commands
  '(("uv"  . ("uv init ." . nil))
    ("cargo"        . ("cargo init ." . nil))
    ("create-react-app" . ("npx create-react-app %s" . t)))
  "Alist of LABEL . (COMMAND-OR-FUNCTION . CREATES-OWN-DIR).
COMMAND-OR-FUNCTION is either a shell command string (%s filled with
PROJECT-NAME when CREATES-OWN-DIR), or a function of no arguments
that prompts as needed and RETURNS a shell command string to run.
If CREATES-OWN-DIR is non-nil, `default-directory' is
`+config/projects-directory' (the command must create its own
project dir). Otherwise the project dir is pre-created."
  :type '(alist :key-type string :value-type sexp))

;;;###autoload
(defun +config/create-local-project (project-name)
  "Create a new project under `+config/projects-directory`.
Makes a fresh directory named PROJECT-NAME and an empty `.project' file in it."
  (interactive "sProject name: ")
  (let* ((root   (file-name-as-directory
                  (expand-file-name +config/projects-directory)))
         (target (expand-file-name project-name root))
         (proj-file (expand-file-name ".project" target))
         (todo-file (expand-file-name "todos.org" target)))
    (when (file-exists-p target)
      (user-error "Directory %S already exists" target))
    (make-directory target t)
    (with-temp-file proj-file)
    (with-temp-file todo-file)
    (project-remember-project (project-current nil target))
    (message "Created local project in %S" target)
    (project-switch-project target)))

;;;###autoload
(defun +config/create-git-repo (project-name)
  "Create a new Git repository under `+config/projects-directory'.

PROJECT-NAME is the directory name of the new repo.  Signal a user
error if the target directory already exists."
  (interactive "sProject name: ")
  (let* ((root (file-name-as-directory
                (expand-file-name +config/projects-directory)))
         (target (expand-file-name project-name root))
         (todo-file (expand-file-name "todos.org" target)))
    (when (file-exists-p target)
      (user-error "Directory %S already exists" target))

    ;; Create the repository directory.
    (make-directory target t)

    ;; Initialize Git.
    (let ((default-directory target))
      (unless (zerop (call-process "git" nil "*git-init*" t "init"))
        (delete-directory target t)
        (error "git init failed; removed %S" target)))

    ;; Create the todo file after successful init.
    (with-temp-file todo-file)

    (project-remember-project (project-current nil target))
    (message "Initialized empty Git repository in %S" target)
    (project-switch-project target)))

;;;###autoload
(defun +config/create-custom-project (project-name)
  "Create PROJECT-NAME dir and initialize it with a chosen command."
  (interactive "sProject name: ")
  (require 'consult)
  (let* ((choices (append +config/project-init-commands
                          (list (cons "Run arbitrary command..." :arbitrary))))
         (answer (consult--read choices
                                :prompt "Initialize with: "
                                :lookup #'consult--lookup-cdr
                                :category 'project-init-command
                                :sort nil))
         (handler (if (eq answer :arbitrary)
                      (cons (read-shell-command "Command (use %s for project name): ")
                            (y-or-n-p "Does this command create its own project dir? "))
                    answer))
         (command-or-fn (car handler))
         (creates-own-dir (cdr handler))
         (project-dir (expand-file-name project-name +config/projects-directory))
         (command (if (functionp command-or-fn)
                      (funcall command-or-fn)
                    (format command-or-fn project-name))))
    (let ((default-directory
           (if creates-own-dir
               +config/projects-directory
             (progn (make-directory project-dir t) project-dir))))
      (+config/run-project-init-command command project-dir))))

(defun +config/run-project-init-command (command project-dir)
  "Run COMMAND async in `default-directory'; remember and switch to PROJECT-DIR on success."
  (let ((buf (generate-new-buffer "*project-init*")))
    (message "Initializing project in %s..." project-dir)
    (make-process
     :name "project-init"
     :buffer buf
     :command (list shell-file-name shell-command-switch command)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (and (zerop (process-exit-status proc))
                  (file-directory-p project-dir))
             (progn
               (project-remember-project (project-current nil project-dir))
               (kill-buffer buf)
               (project-switch-project project-dir)
               (message "Project initialized in %s" project-dir))
           (progn
             (message "Project init failed for %s, see %s" project-dir (buffer-name buf))
             (display-buffer buf))))))))

(defun +config/project-init--annotate (cand)
  "Annotate CAND (a label from +config/project-init-commands) with its command."
  (when-let* ((entry (assoc cand +config/project-init-commands))
              (handler (cdr entry))
              (command-or-fn (car handler)))
    (let ((text (cond
                 ((and (symbolp command-or-fn) (functionp command-or-fn))
                  (symbol-name command-or-fn))
                 ((functionp command-or-fn) "<anonymous function>")
                 (t command-or-fn))))
      (concat " " (propertize text 'face 'marginalia-documentation)))))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotators
               '(project-init-command +config/project-init--annotate builtin none)))

;;;###autoload
(defun +config/create-project (project-name)
  "Interactively create a new project called PROJECT-NAME."
  (interactive "sProject name: ")
  (require 'consult)
  (let* ((choices (list
                   (cons "Create a new local project with command" :command)
                   (cons "Create a new git repo on Github" :remote)
                   (cons "Create a new git repo locally"   :local)
                   (cons "Create only a project dir"       :project)))
         (prompt   "What would you like to do? ")
         (answer   (consult--read choices
                                  :prompt prompt
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))
    (pcase answer
      (:remote  (consult-gh-repo-create project-name))
      (:local   (+config/create-git-repo project-name))
      (:command (+config/create-custom-project project-name))
      (:project (+config/create-local-project project-name)))))

;;;###autoload
(defun +switch-or-make-project (&optional arg)
  "Switch to a project from known projects, or create a new one."
  (interactive "P")
  (let ((choice (project-prompt-project-dir)))
    (if (file-directory-p choice)
        (project-switch-project choice)
      (+config/create-project choice))))

(provide 'project-init)
;;; project-init.el ends here

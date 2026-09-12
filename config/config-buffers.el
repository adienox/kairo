;;; config-buffers.el --- buffers setup -*- lexical-binding: t; -*-

(defun +config/buffer-regex-builder (names)
  "Return a regex that matches *NAMES* buffers."
  (concat
   "\\*\\("
   (mapconcat #'regexp-quote names "\\|")
   "\\)\\*"))

(setq display-buffer-alist
      `(;; Magit status
        ((major-mode . magit-status-mode)
         display-buffer-in-side-window
         (side . bottom)
         (window-height . 0.5)
         (slot . 1)
         (window-dedicated . t)
         (window-parameters . ((mode-line-format . none))))

        ;; Commit message
        ("*COMMIT_EDITMSG"
         display-buffer-in-direction
         (direction . leftmost)
         (window-width . 0.5))

        ;; Magit diff
        ((major-mode . magit-diff-mode)
         display-buffer-in-direction
         (window . root)
         (direction . right)
         (window-width . 0.5))

        ;; Special windows (eshell, eldoc, etc.)
        (,(+config/buffer-regex-builder '("eshell" "eldoc" "use-package statistics"))
         display-buffer-in-side-window
         (side . bottom)
         (window-height . 0.4)
         (slot . 1)
         (window-dedicated . t)
         (window-parameters . ((mode-line-format . none))))

        ;; ghostel
        ((major-mode . ghostel-mode)
         display-buffer-in-side-window
         (side . bottom)
         (window-height . 0.4)
         (slot . 1)
         (window-dedicated . t)
         (window-parameters . ((mode-line-format . none))))

        ;; Compilation
        ("*compilation*"
         display-buffer-in-side-window
         (side . bottom)
         (window-height . 0.4)
         (slot . 1)
         (window-dedicated . t))

        ;; Help windows
        ((or
          (major-mode . helpful-mode)
          (major-mode . help-mode))
         display-buffer-in-side-window
         (inhibit-same-window . t)
         (window-dedicated . t)
         (side . bottom)
         (window-height . 0.5)
         (window-parameters . ((mode-line-format . none))))

        ;; Calendar
        ("*Calendar*"
         display-buffer-reuse-window display-buffer-in-direction
         (direction . bottom)
         (window-dedicated . t)
         (window-height . 0.4))

        ;; Org Mode
        ("CAPTURE-"
         (display-buffer-reuse-window display-buffer-in-direction)
         (direction . bottom)
         (inhibit-same-window . nil)
         (window-height . 0.4)
         (window-parameters . ((mode-line-format . none))))

        ;; ((major-mode . org-mode)
        ;;  (display-buffer-reuse-window display-buffer-same-window display-buffer-in-direction)
        ;;  (direction . right)
        ;;  (window-width . 0.5))

        ((major-mode . org-mode)
         (display-buffer-reuse-window))

        ;; Elpaca log
        ((major-mode . elpaca-log-mode)
         display-buffer-in-side-window
         (side . right)
         (window-width . 0.18)
         (slot . 1)
         (window-parameters . ((mode-line-format . none))))))

;; Deleting and renaming of current file
(use-package bufferfile
  :commands (bufferfile-rename bufferfile-delete)
  :custom
  (bufferfile-use-vc t)
  (bufferfile-delete-switch-to 'previous-buffer)
  :general
  (+config/leader-file
    "d" '(bufferfile-delete :wk "Delete file")
    "r" '(bufferfile-rename :wk "Rename file")))

;; Sudo edit the current file
(use-package sudo-edit
  :commands (sudo-edit-find-file sudo-edit)
  :general
  (+config/leader-file
    "U" '(sudo-edit-find-file :wk "Sudo find file")
    "u" '(sudo-edit :wk "Sudo edit file")))

(defvar-local +config/tab-name nil
  "Current buffer tab name.")

(defun +config/run-commands-for-buffers ()
  "Run commands for buffers."
  ;; also set tab name
  (setq +config/tab-name
        (alist-get 'name (tab-bar--current-tab)))
  (let ((buffer-name (buffer-name)))
    (cond
     ((string= buffer-name "*elpaca-log*")
      (visual-line-mode -1)))))

(add-hook! buffer-list-update #'+config/run-commands-for-buffers)

(use-package buffer-terminator
  :hook (on-first-file . buffer-terminator-mode)
  :config
  (add-to-list 'buffer-terminator-rules-alist '(call-function . +config/buffer-terminator-tab-predicate)))

(defun +config/visible-buffers-all-tabs ()
  "Return all visible buffers across all tab-bar tabs."
  (delete-dups
   (append
    (mapcar #'window-buffer (window-list))
    (mapcan (lambda (tab)
              (when-let* ((ws (alist-get 'ws tab)))
                (mapcar #'get-buffer (window-state-buffers ws))))
            (tab-bar-tabs)))))

(defun +config/buffer-visible-in-any-tab-p (&optional buffer)
  "Return t if BUFFER (default: current buffer) is visible in any tab-bar tab."
  (not (null (member (or buffer (current-buffer))
                     (+config/visible-buffers-all-tabs)))))

(defun +config/buffer-terminator-tab-predicate ()
  "Return :kill, :keep, or nil."
  (when (+config/buffer-visible-in-any-tab-p) :keep))

(use-package ibuffer
  :ensure nil
  :commands ibuffer
  :hook
  (ibuffer-mode . (lambda () (display-line-numbers-mode -1)))
  (ibuffer-mode . (lambda () (visual-line-mode -1)))
  :general
  (+config/leader-buffer
    "i" '(ibuffer :wk "Ibuffer"))
  :custom
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 30 30 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-display-summery nil))

;; FIXME: return a proper ibuffer filter
(defun +config/ibuffer-tab-groups ()
  (let ((tabs (mapcar (lambda (tab)
                        (alist-get 'name tab))
                      (funcall tab-bar-tabs-function))))
    (mapcar (lambda (tab)
              (list tab `((eval . (string= +config/tab-name ,tab)))))
            tabs)))

(defun +config/ibuffer-use-tab-groups ()
  (interactive)
  (setq ibuffer-filter-groups (+config/ibuffer-tab-groups)))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'config-buffers)

;;; config-buffers.el ends here

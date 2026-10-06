;;; config-writing.el --- writing setup -*- lexical-binding: t; -*-

(require 'config-keybinds)

(+config/leader-menu! "writing" "w")

(use-package org
  :ensure nil
  :init
  ;; Using RETURN to follow links in Org/Evil
  ;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
  ;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link t)
  :hook
  (org-mode . prettify-symbols-mode)
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  (org-capture-mode . evil-insert-state)
  (org-mode . org-fold-hide-drawer-all)
  (org-mode . (lambda ()
                (add-hook! before-save :local #'org-update-all-dblocks)))
  :general
  (+config/leader-writing
    "t" '(:ignore t :wk "Toggle")
    "t c" '(org-toggle-checkbox :wk "Checkbox")
    "t i" '((lambda () (interactive) (+config/toggle-meta-line "^#\\+identifier:.*$")) :wk "Identifier")
    "t t" '((lambda () (interactive) (+config/toggle-meta-line "^#\\+filetags:.*$")) :wk "Filetags"))
  :custom
  (org-ellipsis "...")
  (org-confirm-babel-evaluate nil)
  (org-M-RET-may-split-line nil)
  (org-startup-with-latex-preview nil)
  (org-startup-with-link-previews t)
  (org-hide-drawer-startup t)
  (org-image-align 'center)
  (org-image-actual-width nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-support-shift-select t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  (set-face-attribute 'org-meta-line nil
                      :italic nil)

  (defadvice! +org/meta-return-checkbox-a (&rest _)
    "Make `org-meta-return' insert a new checkbox item on checkbox lines.
The default command doesn't special-case checkboxes; this advice
does, and steps aside for everything else."
    :before-until #'org-meta-return
    (when (and (not current-prefix-arg)
               (org-at-item-checkbox-p))
      (org-insert-item t)
      t)))

(use-package org-capture
  :ensure nil
  :commands (org-capture)
  :general
  (+config/leader-key
    "x" '(org-capture :wk "Capture"))
  :config
  (setq org-capture-templates
        '(("t" "Task")

          ("tt" "Plain task" entry
           (file +config/tasks-file)
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("tp" "Priority task" entry
           (file +config/tasks-file)
           "* TODO [#%^{Priority|A|B|C}] %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
           :empty-lines 1))))

;; can cause the following issue
;; https://emacs.stackexchange.com/questions/84750/typing-regular-letters-in-a-special-org-buffer-triggers-asking-about-which-tags
;; but needed for completion-preview to work in org mode
(add-hook! org-mode
  (setq-local completion-preview-commands
              '(;; self-insert-command
                org-self-insert-command
                insert-char
                ;; delete-backward-char
                org-delete-backward-char
                backward-delete-char-untabify
                analyze-text-conversion
                completion-preview-complete)))

(use-package org-attach
  :ensure nil
  :after org
  :custom
  (org-attach-id-dir "attachments/")
  (org-attach-use-inheritance t)
  (org-attach-method 'mv))

(defvar-local +config-hidden-meta-line-overlays nil
  "Alist of (REGEXP . OVERLAYS) currently hidden via `+config/hide-meta-line'.")

(defun +config/hide-meta-line (regexp)
  "Hide all lines matching REGEXP (including trailing newline) via overlays."
  (unless (assoc regexp +config-hidden-meta-line-overlays)
    (let (overlays)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let* ((beg (match-beginning 0))
                 (end (min (point-max) (1+ (match-end 0)))) ; swallow trailing \n
                 (ov (make-overlay beg end)))
            (overlay-put ov 'invisible t)
            (overlay-put ov '+config-meta-line t)
            (push ov overlays))))
      (push (cons regexp overlays) +config-hidden-meta-line-overlays))))

(defun +config/show-meta-line (regexp)
  "Reveal lines matching REGEXP previously hidden by `+config/hide-meta-line'."
  (let ((entry (assoc regexp +config-hidden-meta-line-overlays)))
    (when entry
      (mapc #'delete-overlay (cdr entry))
      (setq +config-hidden-meta-line-overlays
            (assoc-delete-all regexp +config-hidden-meta-line-overlays)))))

(defun +config/toggle-meta-line (regexp)
  "Toggle visibility of lines matching REGEXP in the current buffer."
  (interactive "sRegexp to toggle: ")
  (if (assoc regexp +config-hidden-meta-line-overlays)
      (+config/show-meta-line regexp)
    (+config/hide-meta-line regexp)))

(add-hook! org-mode (+config/hide-meta-line "^#\\+identifier:.*$"))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-timestamp nil)
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-progress nil)
  (org-modern-star 'replace)
  (org-modern-checkbox '((88 . "󰄵 ") (45 . "󰡖 ") (32 . "󰄱 ")))
  (org-modern-list '((43 . "◦") (45 . "•") (42 . "•")))
  :config
  (set-face-attribute 'nobreak-space nil
                      :underline nil))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :general
  (+config/leader-writing
    "s" '(denote-open-or-create :wk "Search notes")
    "i" '((lambda () (interactive) (+config/find-file-in-inbox "inbox.org")) :wk "Inbox")
    "x" '((lambda () (interactive) (+config/find-file-in-inbox "tasks.org")) :wk "Tasks"))
  :custom
  (denote-directory +config/notes-directory)
  (denote-save-buffers t)
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-date-prompt-use-org-read-date t)
  (denote-rename-buffer-backlinks-indicator "󰌹 ")
  (denote-rename-buffer-format "%D %b")
  :config
  (denote-rename-buffer-mode)
  (+config/add-project-vc-root-markers ".notes")
  (require 'denote-capf))

(defun +config/find-file-in-inbox (file)
  "Open FILE from the inbox directory in a dedicated right side window."
  (interactive "sFile: ")
  (let* ((full-path (expand-file-name (concat "inbox/" file) +config/notes-directory))
         (buf       (find-file-noselect full-path))
         (win       (split-window (frame-root-window) (- 80) 'right)))
    (set-window-buffer win buf)
    (set-window-dedicated-p win t)
    (select-window win)))

(use-package consult-denote
  :hook (on-first-input . consult-denote-mode))

(use-package denote-org)

(use-package denote-journal
  :hook
  (calendar-mode . denote-journal-calendar-mode)
  :general
  (+config/leader-writing
    "j" '(denote-journal-new-or-existing-entry :wk "Journal"))
  :custom
  (denote-journal-title-format 'day-date-month-year)
  :config
  (set-face-attribute 'denote-journal-calendar nil
                      :inherit 'success
                      :weight 'bold
                      :box 'unspecified))

(add-hook! on-first-buffer #'global-prettify-symbols-mode)

(setq-default prettify-symbols-alist
              '(("#+begin_src emacs-lisp" . "")
                ("#+begin_src elisp" . "")
                ("#+begin_src cpp" . "")
                ("#+begin_src python" . "")

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
                ("#+RESULTS:" . "󰥤")))

(use-package org-appear
  :hook
  (org-mode . (lambda ()
                (org-appear-mode)
                (add-hook! evil-insert-state-entry :local #'org-appear-manual-start)
                (add-hook! evil-insert-state-exit  :local #'org-appear-manual-stop)))
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-trigger 'manual))

(use-package mixed-pitch
  :hook
  (markdown-mode . mixed-pitch-mode)
  (org-mode      . mixed-pitch-mode))

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :general
  (+config/leader-toggle
    "c" '(olivetti-mode :wk "Centered mode"))
  :custom
  (olivetti-body-width 110))

(defun +config/denote-attach-setup ()
  (require 'org-attach)
  (setq-local org-attach-preferred-new-method 'dir)
  (org-update-all-dblocks)
  (add-hook 'completion-at-point-functions #'+config/denote-capf nil t)
  (setq-local org-global-properties
              `(("DIR" . ,(expand-file-name
                           (denote-retrieve-filename-identifier (buffer-file-name))
                           org-attach-id-dir)))))

(add-to-list 'safe-local-eval-forms '(+config/denote-attach-setup))

(provide 'config-writing)

;;; config-writing.el ends here

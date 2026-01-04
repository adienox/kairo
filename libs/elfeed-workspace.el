;;; scripts/elfeed-workspace.el -*- lexical-binding: t; -*-

;;; Code:
(defvar elfeed-workspace-name "feed"
  "The name of the workspace used for Elfeed.")

(defun =elfeed ()
  "Start Elfeed RSS reader."
  (interactive)
  (require 'elfeed)
  (persp-switch elfeed-workspace-name)
  ;; Try to jump to existing elfeed buffers in order of preference:
  ;; 1. Elfeed Search
  ;; 2. Elfeed Show
  ;; If none exist, call `elfeed`.
  (let (search-buffer show-buffer)
    (dolist (buf (buffer-list))
      (pcase (buffer-local-value 'major-mode buf)
        ((and 'elfeed-search-mode (guard (not search-buffer)))
         (setq search-buffer buf))
        ((and 'elfeed-show-mode (guard (not show-buffer)))
         (setq show-buffer buf))))
    (if (and search-buffer (not (eq search-buffer (current-buffer))))
        (switch-to-buffer search-buffer)
      (if (and show-buffer (not (eq show-buffer (current-buffer))))
          (switch-to-buffer show-buffer)
        (elfeed)))))

(defun +elfeed--cleanup-workspace (&rest _)
  "Delete Elfeed workspace when quitting, if it's empty."
  (when (equal (persp-name (persp-curr)) elfeed-workspace-name)
    (persp-kill elfeed-workspace-name)))

(advice-add 'elfeed-search-quit-window :after #'+elfeed--cleanup-workspace)

(provide 'elfeed-workspace)
;;; elfeed-workspace.el ends here

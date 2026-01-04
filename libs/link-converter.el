;;; link-converter.el --- Convert md to org and vice versa -*- lexical-binding: t; -*-

;;; Commentary:
;; from https://www.reddit.com/r/emacs/comments/4154bu/comment/lq5ee4i
;; and https://www.reddit.com/r/emacs/comments/4154bu/comment/lq5ekjp

;;; Code:
(defun nox/md-to-org-links ()
  "Convert all Markdown links in buffer to Org-mode format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
      (replace-match "[[\\2][\\1]]"))))

(defun nox/org-to-md-links ()
  "Convert all Org-mode links in buffer to Markdown format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]" nil t)
      (replace-match "[\\2](\\1)"))))

(defun nox/org-to-md-link ()
  "Convert the Org-mode link at point to Markdown format."
  (interactive)
  (let ((org-regex "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]")
        (case-fold-search t)
        start end link-text)
    (save-excursion
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (goto-char line-start)
        (while (and (< (point) line-end)
                    (not (looking-at "\\[\\[")))
          (forward-char))
        (when (looking-at org-regex)
          (setq start (point)
                end (match-end 0)
                link-text (buffer-substring-no-properties start end)))))
    (if link-text
        (when (string-match org-regex link-text)
          (let ((url (match-string 1 link-text))
                (text (match-string 2 link-text)))
            (delete-region start end)
            (insert (format "[%s](%s)" text url))
            (message "Converted Org-mode link to Markdown")))
      (message "No Org-mode link found at point"))))

(defun nox/md-to-org-link ()
  "Convert the Markdown link at point to Org-mode format."
  (interactive)
  (let ((markdown-regex "\\[\\(.*?\\)\\](\\(.*?\\))")
        (case-fold-search t)
        start end link-text)
    (save-excursion
      (while (and (not (bobp))
                  (not (looking-at "\\[\\|\\]")))
        (backward-char))
      (when (re-search-forward markdown-regex (line-end-position) t)
        (setq start (match-beginning 0)
              end (match-end 0)
              link-text (match-string 0))))
    (if link-text
        (when (string-match markdown-regex link-text)
          (let ((text (match-string 1 link-text))
                (url (match-string 2 link-text)))
            (delete-region start end)
            (insert (format "[[%s][%s]]" url text))
            (message "Converted Markdown link to Org-mode")))
      (message "No Markdown link found at point"))))


(provide 'link-converter)

;;; link-converter.el ends here

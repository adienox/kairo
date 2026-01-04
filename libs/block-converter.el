;;; scripts/block-converter.el -*- lexical-binding: t; -*-

(defun nox/md-code-blocks-to-org ()
  "Convert all Markdown code blocks in the current buffer to Org-mode code blocks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Match blocks like: ```lang\n...\n```
    (while (re-search-forward "^```\\([a-zA-Z0-9+-]*\\)\n\\([\0-\377[:nonascii:]]*?\\)\n```" nil t)
      (let ((lang (match-string 1))
            (body (match-string 2)))
        (replace-match (format "#+begin_src %s\n\n%s\n\n#+end_src" lang body) t t)))))

(defun nox/md-blockquotes-to-org ()
  "Convert Markdown block quotes (with or without space) to Org-mode quote blocks in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Match one or more lines that start with > (with or without space), then a non-quote line
    (while (re-search-forward "^\\(>.*\n\\)+\\(?:\\(?:[^>]\\|\\'\\)\\)" nil t)
      (let* ((match (match-string 0))
             ;; Strip just the leading `>` and optional space
             (quote-body (replace-regexp-in-string "^> ?" "" match)))
        (replace-match (format "#+begin_quote\n\n%s#+end_quote\n\n" quote-body) t t)))))


(provide 'block-converter)
;;; block-converter.el ends here

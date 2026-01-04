(require 'org)
(require 'org-element)
(require 'subr-x)  ;; string-trim

;; Helper: check whether CREATED timestamp belongs to *today*.
(defun nox/org--created-today-p (created)
  "Return non-nil if CREATED timestamp string is for today.
CREATED is typically an Org timestamp, e.g. \"[2026-01-03 Sat 10:23]\"."
  (when created
    (let* ((ts  (org-parse-time-string created)) ; (sec min hour day mon year ...)
           (y   (nth 5 ts))
           (m   (nth 4 ts))
           (d   (nth 3 ts))
           (now (decode-time (current-time))))
      (and y m d
           (= y (nth 5 now))
           (= m (nth 4 now))
           (= d (nth 3 now))))))

;; Helper: remove property/logbook drawers and trim the whole string.
(defun nox/org--clean-content (content)
  "Strip PROPERTIES/LOGBOOK drawers from CONTENT and trim whitespace."
  (when content
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Remove PROPERTIES drawer
      (while (re-search-forward
              "^[ \t]*:PROPERTIES:\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?"
              nil t)
        (replace-match ""))
      (goto-char (point-min))
      ;; Remove LOGBOOK drawer
      (while (re-search-forward
              "^[ \t]*:LOGBOOK:\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?"
              nil t)
        (replace-match ""))
      (string-trim (buffer-string)))))

(defun nox/org-headings-with-created (&optional file)
  "Return list of plists with :title and :content.

Only includes headings in FILE (or current buffer) whose CREATED
property is set to *today*.  In each content:

- PROPERTIES and LOGBOOK drawers are removed
- Leading/trailing whitespace is trimmed

Each element looks like:
  (:title \"Heading title\" :content \"Body text...\")"
  (with-current-buffer (if file
                           (find-file-noselect file)
                         (current-buffer))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (result)
       ;; Visit each heading in the buffer
       (while (re-search-forward org-heading-regexp nil t)
         (let* ((el      (org-element-at-point))
                (created (org-element-property :CREATED el)))
           ;; Only keep headings created today
           (when (nox/org--created-today-p created)
             (let* ((cbegin   (org-element-property :contents-begin el))
                    (cend     (org-element-property :contents-end el))
                    (raw-body (and cbegin
                                   (buffer-substring-no-properties cbegin cend)))
                    (title    (string-trim
                               (or (org-element-property :raw-value el) "")))
                    (content  (nox/org--clean-content raw-body)))
               (push (list :title title :content content) result)))))
       (nreverse result)))))

(defun nox/org--concat-headings (headings)
  "Return string with headings properly formatted.

* Heading
Content

* Another Heading

* Heading Too"
  (let ((content
         (mapconcat
          (lambda (item)
            (let ((title   (plist-get item :title))
                  (content (plist-get item :content)))
              (if (and content (not (string-empty-p content)))
                  (format "* %s\n%s\n\n" title content)
                (format "* %s\n\n" title))))
          headings
          "")))
    (string-trim content)))

(defun org-dblock-write:inbox-notes (params)
  "Insert all headings with a CREATED property from :file (or `nox/inbox-file')."
  (let* ((file     (or (plist-get params :file) nox/inbox-file))
         (headings (nox/org-headings-with-created file))
         (out      (nox/org--concat-headings headings)))
    (insert out)))

(defun nox/org-dblock-insert-inbox-notes ()
  "Insert and update an `inbox-notes' Org dynamic block."
  (interactive nil org-mode)
  (org-create-dblock (list :name "inbox-notes"))
  (org-update-dblock))

(provide 'inbox-notes-dblock)

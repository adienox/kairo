;;; denote-capf.el --- title-match completion-at-point for Denote -*- lexical-binding: t; -*-

;; No explicit trigger prefix. As you type a word, this checks whether
;; it's a substring of any Denote note's title; if so it offers
;; completion, and selecting a candidate replaces the typed text with
;; a real denote: link. If nothing matches, it returns nil and stays
;; out of the way of ordinary prose.

(defvar +config/denote-capf--cache nil
  "Cached (TITLE . FILE) alist, built lazily and invalidated on note changes.")

(defun +config/denote-capf--build-cache (&rest _)
  "Rebuild `+config/denote-capf--cache' from the current Denote corpus."
  (setq +config/denote-capf--cache
        (mapcar
         (lambda (file)
           (cons (or (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
                     (denote-retrieve-filename-title file))
                 file))
         (denote-directory-files))))

(defun +config/denote-capf--title-alist ()
  "Return the cached title/file alist, building it on first use."
  (or +config/denote-capf--cache
      (+config/denote-capf--build-cache)))

;; Keep the cache fresh across the usual ways the corpus changes.
(add-hook 'denote-after-new-note-hook #'+config/denote-capf--build-cache)
(advice-add 'denote-rename-file :after #'+config/denote-capf--build-cache)

(defun +config/denote-capf--annotate (cand)
  "Show the identifier next to CAND in the completion UI."
  (when-let* ((file (get-text-property 0 'denote-capf-file cand))
              (id (denote-retrieve-filename-identifier file)))
    (concat "  " (propertize id 'face 'completions-annotations))))

(defcustom +config/denote-capf-space-after-link t
  "If non-nil, insert a space after the link once a completion is accepted."
  :type 'boolean)

(defun +config/denote-capf--exit (cand status)
  "Replace the inserted title text CAND with a proper denote: link.
Prefers the `denote-capf-file' text property on CAND, but falls back
to a lookup against the title cache: some completion UIs (notably
`completion-preview-mode', via `set-text-properties' in
`completion-preview--update') strip custom text properties from
candidate strings before they ever reach this function."
  (when (memq status '(finished sole))
    (when-let* ((clean (substring-no-properties cand))
                (file (or (get-text-property 0 'denote-capf-file cand)
                          (cdr (assoc clean (+config/denote-capf--title-alist)))))
                (end (point))
                (beg (- end (length clean))))
      (delete-region beg end)
      (insert (denote-format-link
               file clean
               (denote-filetype-heuristics buffer-file-name)
               nil))
      (when +config/denote-capf-space-after-link
        (insert " ")))))

(defcustom +config/denote-capf-min-length 3
  "Minimum length of the word at point before title-matching kicks in.
Keeps the capf from firing (and scanning the title cache) on every
1-2 character word while you're typing ordinary prose."
  :type 'integer)

;;;###autoload
(defun +config/denote-capf ()
  "`completion-at-point-functions' entry: complete to a Denote link
whenever the word at point is a substring of some note's title.
Returns nil (declines to complete) if nothing matches, so it doesn't
interfere with normal typing."
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (start (car bounds))
              (end (cdr bounds))
              (text (buffer-substring-no-properties start end))
              ((>= (length text) +config/denote-capf-min-length))
              (needle (downcase text))
              (matches (seq-filter
                        (lambda (pair) (string-search needle (downcase (car pair))))
                        (+config/denote-capf--title-alist))))
    (list start end
          (completion-table-case-fold
           (mapcar (lambda (pair) (propertize (car pair) 'denote-capf-file (cdr pair)))
                   matches))
          :exclusive 'no
          :annotation-function #'+config/denote-capf--annotate
          :exit-function #'+config/denote-capf--exit)))

(provide 'denote-capf)
;;; denote-capf.el ends here

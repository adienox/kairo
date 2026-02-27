;;; koreader-epub-highlights.el --- Sync KOReader highlights to nov.el -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: nox
;; Version: 0.8.0
;; Package-Requires: ((emacs "27.1") (nov "0.4.0"))
;; Keywords: epub, highlights, koreader, nov

;;; Commentary:

;; This package parses KOReader JSON highlight exports and overlays
;; the highlights onto the current nov.el buffer, grouped by chapter.
;;
;; Usage:
;;
;;   1. Export your highlights from KOReader as JSON.
;;   2. Open your EPUB in nov.el.
;;   3. Call `koreader/load-highlights' and select the JSON file.
;;      Highlights for the current chapter will be applied automatically.
;;   4. Call `koreader/highlight-chapter' to re-apply highlights when
;;      switching chapters, or set up the auto-highlight hook.
;;   5. Call `koreader/clear-highlights' to remove all overlays.
;;   6. Call `koreader/show-note-at-point' to display the KOReader note
;;      for a highlighted passage.
;;   7. Call `koreader/nov-next-highlight' / `koreader/nov-previous-highlight'
;;      to jump between highlights in the current chapter.
;;   8. Call `koreader/reload' to force re-parse the JSON file.
;;   9. Call `koreader/show-highlights-buffer' to see all highlights
;;      grouped by chapter in a dedicated buffer.
;;  10. Call `koreader/browse-highlights' to open the highlights buffer
;;      directly from a JSON file, without opening the EPUB first.
;;  11. Call `koreader/export-to-org' to export all highlights to an Org file.
;;      This command also works from within the highlights buffer.
;;  12. Call `koreader/export-chapter-to-org' to export the current chapter only.
;;  13. Call `koreader/open' to open an EPUB and JSON together in one step.
;;
;; In the highlights buffer:
;;   ?             — show keybinding help
;;   n / p         — next / previous highlight
;;   TAB           — fold / unfold chapter
;;   RET           — jump to passage in the nov buffer
;;               (opens the EPUB automatically if needed)
;;   e             — show note at point (posframe or split window)
;;   w             — copy highlight text at point
;;   s             — search highlight text (press again to advance)
;;   E             — export all highlights to an Org file
;;   g             — refresh buffer from current data
;;   q             — quit
;;
;; Evil normal state bindings (when evil is loaded):
;;   ?             — show keybinding help
;;   C-j / C-k     — next / previous highlight
;;   TAB           — fold / unfold chapter
;;   RET           — jump to passage in the nov buffer
;;   e             — show note at point
;;   y             — copy highlight text at point
;;   /             — search highlight text
;;   n             — advance to next search match
;;   E             — export all highlights to an Org file
;;   gr            — refresh buffer
;;   q             — quit window
;;   z o / z c     — fold / unfold chapter
;;
;; To auto-highlight on chapter changes, add to your config:
;;
;;   (add-hook 'nov-post-html-render-hook #'koreader/highlight-chapter)
;;
;; Or call `koreader/setup-auto-highlight' interactively.
;;
;; Optionally set a default exports directory to skip the file picker:
;;
;;   (setq koreader/exports-directory "~/Documents/koreader-exports/")
;;
;; Optionally install the `posframe' package for floating note display
;; when calling `koreader/show-note-at-point'.  If posframe is not
;; installed the command falls back to the minibuffer or a split window.

;;; Code:

(require 'nov)
(require 'cl-lib)

;;;; Customization

(defgroup koreader-epub-highlights nil
  "Sync KOReader highlights to nov.el."
  :group 'nov
  :prefix "koreader/")

(defface koreader/highlight-face
  '((t (:underline t)))
  "Face used for KOReader highlights in the nov buffer."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-title-face
  '((t (:height 1.4 :weight bold :inherit variable-pitch)))
  "Face for the book title in the highlights buffer."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-chapter-face
  '((t (:height 1.1 :weight bold :inherit variable-pitch)))
  "Face for chapter headings in the highlights buffer."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-rule-face
  '((t (:inherit shadow)))
  "Face for separator lines in the highlights buffer."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-quote-face
  '((t (:slant italic :inherit (variable-pitch font-lock-string-face))))
  "Face for highlight text in the highlights buffer."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-annotated-quote-face
  '((t (:slant italic :inherit (variable-pitch font-lock-type-face))))
  "Face for highlight text that has an associated note."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-note-face
  '((t (:slant italic :inherit variable-pitch)))
  "Face for notes in the highlights buffer."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-page-face
  '((t (:height 0.9 :slant italic :inherit shadow)))
  "Face for page numbers and metadata in the highlights buffer (fixed pitch)."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-chapter-count-face
  '((t (:height 0.9 :slant italic :inherit shadow)))
  "Face for chapter highlight counts in the highlights buffer (fixed pitch)."
  :group 'koreader-epub-highlights)

(defface koreader/buffer-visited-face
  '((t (:slant italic :inherit (variable-pitch shadow))))
  "Face applied to highlight entries that have been jumped to via RET."
  :group 'koreader-epub-highlights)

(defcustom koreader/exports-directory nil
  "Default directory to look for KOReader JSON exports.
When set, `koreader/load-highlights' will default to this directory
in the file picker instead of `default-directory'."
  :type '(choice (const :tag "None" nil)
                 (directory :tag "Directory"))
  :group 'koreader-epub-highlights)

(defcustom koreader/note-posframe-border-width 1
  "Border width in pixels for the KOReader note posframe."
  :type 'integer
  :group 'koreader-epub-highlights)

(defcustom koreader/note-posframe-padding 2
  "Horizontal padding in characters inside the KOReader note posframe."
  :type 'integer
  :group 'koreader-epub-highlights)

(defcustom koreader/note-posframe-max-width 56
  "Maximum width in characters for the KOReader note posframe content area.
The actual frame will be wider by 2 x `koreader/note-posframe-padding'."
  :type 'integer
  :group 'koreader-epub-highlights)

(defcustom koreader/heading-glyph
  (if (char-displayable-p ?❧) "❧" "*")
  "Glyph prepended to chapter headings in the highlights buffer.
Defaults to the fleuron character if displayable, otherwise \"*\"."
  :type 'string
  :group 'koreader-epub-highlights)

(defcustom koreader/quote-open-glyph
  (if (char-displayable-p ?❝) "❝" "\"")
  "Opening glyph for highlight quotations in the highlights buffer."
  :type 'string
  :group 'koreader-epub-highlights)

(defcustom koreader/quote-close-glyph
  (if (char-displayable-p ?❞) "❞" "\"")
  "Closing glyph for highlight quotations in the highlights buffer."
  :type 'string
  :group 'koreader-epub-highlights)

(defcustom koreader/note-glyph
  (if (char-displayable-p ?✎) "✎" "-")
  "Glyph prepended to notes in the highlights buffer."
  :type 'string
  :group 'koreader-epub-highlights)

;;;; Buffer-local state

(defvar-local koreader/-epub-data nil
  "Parsed highlight data for the book open in this buffer.
An alist of the form:
  ((title      . \"Book Title\")
   (filepath   . \"/path/to/export.json\")
   (highlights . ((\"Chapter N\" . (((text . \"...\") (note . \"...\")) ...))
                  ...)))")

(defvar-local koreader/-toc-alist nil
  "Alist of (SRC . TITLE) extracted from the EPUB TOC for this buffer.")

;;;; Guards

(defun koreader/-assert-nov-mode ()
  "Signal an error if the current buffer is not in `nov-mode'."
  (unless (derived-mode-p 'nov-mode)
    (user-error "koreader: this command must be run in a nov.el buffer")))

;;;; Utilities

(defun koreader/-insert (face &rest strings)
  "Insert STRINGS into the current buffer with FACE applied."
  (let ((start (point)))
    (apply #'insert strings)
    (add-face-text-property start (point) face)))

(defun koreader/-count-highlights (epub-data)
  "Return the total number of highlight entries in EPUB-DATA."
  (cl-reduce #'+ (alist-get 'highlights epub-data)
             :key (lambda (c) (length (cdr c)))
             :initial-value 0))

(defun koreader/-plural (n singular &optional suffix)
  "Return SINGULAR or SINGULAR+SUFFIX depending on whether N equals 1.
SUFFIX defaults to \"s\"."
  (if (= n 1) singular (concat singular (or suffix "s"))))

;;;; Highlight overlays (nov buffer)

(defface koreader/highlight-yellow-face
  '((t (:inherit (koreader/highlight-face hi-yellow))))
  "Face for yellow KOReader highlights in the nov buffer."
  :group 'koreader-epub-highlights)

(defface koreader/highlight-gray-face
  `((t (:background ,(face-attribute 'shadow :foreground)
                    :inherit koreader/highlight-face)))
  "Face for gray KOReader highlights in the nov buffer."
  :group 'koreader-epub-highlights)

(defface koreader/highlight-red-face
  '((t (:inherit (koreader/highlight-face hi-salmon))))
  "Face for red KOReader highlights in the nov buffer."
  :group 'koreader-epub-highlights)

(defface koreader/highlight-green-face
  '((t (:inherit (koreader/highlight-face hi-green))))
  "Face for green KOReader highlights in the nov buffer."
  :group 'koreader-epub-highlights)

(defface koreader/highlight-blue-face
  '((t (:inherit (koreader/highlight-face hi-blue))))
  "Face for blue KOReader highlights in the nov buffer."
  :group 'koreader-epub-highlights)

(defun koreader/-color-face (color)
  "Return the highlight face for COLOR string, falling back to the default."
  (pcase (and color (downcase color))
    ("yellow" 'koreader/highlight-yellow-face)
    ("gray"   'koreader/highlight-gray-face)
    ("grey"   'koreader/highlight-gray-face)
    ("red"    'koreader/highlight-red-face)
    ("green"  'koreader/highlight-green-face)
    ("blue"   'koreader/highlight-blue-face)
    (_        'koreader/highlight-face)))

(defun koreader/highlight-string (str)
  "Highlight all occurrences of STR in the current buffer using overlays.
This is a public utility; `koreader/highlight-chapter' handles note and
page attachment internally and does not call this function."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward str nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'face 'koreader/highlight-face)
        (overlay-put ov 'koreader-highlight t)))))

(defun koreader/clear-highlights ()
  "Remove all KOReader highlight overlays from the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'koreader-highlight t))

;;;; Note display

(defconst koreader/-note-buffer "*koreader-note*"
  "Name of the buffer used for note display.")

(defun koreader/-note-content (note page)
  "Format NOTE and PAGE into a display string.
The note body comes first; the page number is appended at the end."
  (string-trim
   (concat (when note note)
           (when (and note page) "\n\n")
           (when page (format "p.%s" page)))))

(defun koreader/-fill-note-content (content max-width)
  "Return CONTENT word-wrapped to MAX-WIDTH, preserving explicit blank lines."
  (with-temp-buffer
    (insert content)
    (let ((fill-column max-width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun koreader/-show-note-posframe (content)
  "Display CONTENT in a posframe near point with proper padding and word-wrap."
  (let* ((pad          koreader/note-posframe-padding)
         (max-w        koreader/note-posframe-max-width)
         (filled       (koreader/-fill-note-content content max-w))
         (pad-str      (make-string pad ?\s))
         (padded-lines (mapcar (lambda (l) (concat pad-str l pad-str))
                               (split-string filled "\n")))
         (inner-w      (cl-reduce #'max padded-lines :key #'length :initial-value 0))
         (inner-h      (+ 1 (length padded-lines)))
         (padded-str   (concat (mapconcat #'identity padded-lines "\n") "\n")))
    (with-current-buffer (get-buffer-create koreader/-note-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert padded-str)
        (setq-local mode-line-format nil)
        (setq-local cursor-type      nil)
        (setq-local truncate-lines   t)
        (setq-local word-wrap        nil)
        (variable-pitch-mode 1)))
    (posframe-show koreader/-note-buffer
                   :position         (point)
                   :width            inner-w
                   :height           inner-h
                   :border-width     koreader/note-posframe-border-width
                   :border-color     (face-foreground 'koreader/buffer-rule-face nil t)
                   :background-color (face-background 'default nil t)
                   :foreground-color (face-foreground 'default nil t)
                   :accept-focus     nil
                   :poshandler       #'posframe-poshandler-point-bottom-left-corner)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [t] (lambda ()
                             (interactive)
                             (posframe-hide koreader/-note-buffer)))
       map))))

(defun koreader/show-note-at-point ()
  "Display the KOReader note and page for the highlighted passage at point.
Works in both the nov buffer (overlay properties) and the highlights buffer
(text properties).  Uses a posframe when available; falls back to the
minibuffer for short content or a split window for longer content."
  (interactive)
  ;; Support both nov overlay properties and highlights-buffer text properties
  (let ((note (or (get-char-property (point) 'koreader-note)
                  (get-text-property  (point) 'koreader-note)))
        (page (or (get-char-property (point) 'koreader-page)
                  (get-text-property  (point) 'koreader-page))))
    (if (not (or note page))
        (message "No KOReader annotation at point.")
      (let ((content (koreader/-note-content note page)))
        (cond
         ((and (fboundp 'posframe-show) (display-graphic-p))
          (koreader/-show-note-posframe content))
         ((< (length content) 80)
          (message "%s" content))
         (t
          (with-current-buffer (get-buffer-create koreader/-note-buffer)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert content)
              (special-mode)
              (goto-char (point-min))))
          (display-buffer koreader/-note-buffer
                          '(display-buffer-below-selected
                            (window-height . 8)))))))))

(defun koreader/nov-next-highlight ()
  "Jump to the next KOReader highlight overlay in the current nov chapter."
  (interactive)
  (let ((pos (next-single-char-property-change (point) 'koreader-highlight)))
    (when (and pos (not (get-char-property pos 'koreader-highlight)))
      (setq pos (next-single-char-property-change pos 'koreader-highlight)))
    (if (and pos (< pos (point-max)))
        (goto-char pos)
      (user-error "koreader: no next highlight"))))

(defun koreader/nov-previous-highlight ()
  "Jump to the previous KOReader highlight overlay in the current nov chapter."
  (interactive)
  (let* ((pos (previous-single-char-property-change (point) 'koreader-highlight))
         (pos (if (and pos (get-char-property pos 'koreader-highlight))
                  (previous-single-char-property-change pos 'koreader-highlight)
                pos)))
    (if (and pos (> pos (point-min)))
        (goto-char pos)
      (user-error "koreader: no previous highlight"))))

;;;; TOC parsing
;;
;; `koreader/-refresh-toc' is the sole public consumer of the two helpers
;; below.  They are kept as named functions for testability but are not
;; part of the package's interactive API.

(defun koreader/-nov-current-doc-path ()
  "Return a normalised relative path for the current nov document.
Uses up to the last two non-empty components so root-level files
(one component) and subdirectory files (two components) both work."
  (let* ((path-full  (cdr (aref nov-documents nov-documents-index)))
         (components (cl-remove "" (file-name-split path-full) :test #'string=))
         (tail       (last components (min 2 (length components)))))
    (string-join tail "/")))

(defun koreader/-parse-toc (type file)
  "Parse EPUB TOC FILE of TYPE and return an alist of (SRC . TITLE).
TYPE must be either \\='nav (EPUB3 nav.xhtml) or \\='ncx (EPUB2 toc.ncx)."
  (unless (memq type '(nav ncx))
    (error "koreader: TOC type must be `nav' or `ncx', got `%s'" type))
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (let ((xml    (libxml-parse-xml-region (point-min) (point-max)))
              (result '()))
          (cl-labels
              ((walk-ncx (node)
                 (when (listp node)
                   (when (eq (car node) 'navPoint)
                     (let* ((nav-label (assoc 'navLabel (cddr node)))
                            (text-node (assoc 'text (cddr nav-label)))
                            (title     (car (xml-node-children text-node)))
                            (content   (assoc 'content (cddr node)))
                            (src       (cdr (assoc 'src (cadr content)))))
                       (when (and src title)
                         (push (cons src title) result))))
                   (mapc #'walk-ncx (xml-node-children node))))
               (walk-nav (node)
                 (when (listp node)
                   (when (eq (car node) 'a)
                     (let* ((attrs (cadr node))
                            (src   (cdr (assoc 'href attrs)))
                            (title (car (xml-node-children node))))
                       (when (and src title)
                         (push (cons src title) result))))
                   (mapc #'walk-nav (xml-node-children node)))))
            (pcase type
              ('ncx (walk-ncx xml))
              ('nav (walk-nav xml))))
          (nreverse result)))
    (error
     (user-error "koreader: failed to parse TOC file `%s': %s" file (cdr err)))))

(defun koreader/-refresh-toc ()
  "Refresh `koreader/-toc-alist' for the current nov buffer."
  (setq koreader/-toc-alist
        (koreader/-parse-toc nov-toc-id (cdr (aref nov-documents 0))))
  (push `("title" . ,(alist-get 'title nov-metadata)) koreader/-toc-alist))

;;;; Chapter detection

(defun koreader/get-chapter ()
  "Return the chapter title for the current nov.el document index."
  (koreader/-assert-nov-mode)
  (let ((current-title (alist-get 'title nov-metadata))
        (stored-title  (cdr (assoc "title" koreader/-toc-alist))))
    (unless (and current-title stored-title
                 (string= current-title stored-title))
      (koreader/-refresh-toc)))
  (cdr (assoc (koreader/-nov-current-doc-path) koreader/-toc-alist)))

;;;; JSON parsing

(defun koreader/-parse-json-file (filepath)
  "Parse KOReader JSON export at FILEPATH.
Returns an alist of the form:
  ((title      . \"Book Title\")
   (filepath   . FILEPATH)
   (highlights . ((\"Chapter N\" . (((text  . \"...\")
                                    (note  . \"...\")
                                    (page  . \"...\")
                                    (color . \"...\")
                                    (time  . 1234567890)) ...)))))
Signals a `user-error' if the file cannot be read or parsed."
  (condition-case err
      (let* ((data (with-temp-buffer
                     (insert-file-contents filepath)
                     (json-parse-buffer :object-type  'alist
                                        :array-type   'list
                                        :null-object   nil
                                        :false-object  nil)))
             (title   (alist-get 'title data))
             (grouped nil))
        (dolist (entry (alist-get 'entries data))
          (let* ((chapter  (alist-get 'chapter entry))
                 (item     `((text  . ,(alist-get 'text  entry))
                             (note  . ,(alist-get 'note  entry))
                             (page  . ,(alist-get 'page  entry))
                             (color . ,(alist-get 'color entry))
                             (time  . ,(alist-get 'time  entry))))
                 (existing (assoc chapter grouped)))
            (if existing
                (push item (cdr existing))
              (push (cons chapter (list item)) grouped))))
        ;; Each chapter list was built with push so entries are in reverse order
        (dolist (chapter grouped)
          (setcdr chapter (nreverse (cdr chapter))))
        `((title      . ,title)
          (filepath   . ,filepath)
          (highlights . ,(nreverse grouped))))
    (error
     (user-error "koreader: failed to parse JSON file `%s': %s" filepath (cdr err)))))

(defun koreader/-json-peek (filepath)
  "Parse FILEPATH with `json-parse-buffer' and return the result, or nil on error.
Used for lightweight auto-detection checks."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents filepath)
        (json-parse-buffer :object-type  'alist
                           :array-type   'list
                           :null-object   nil
                           :false-object  nil))
    (error nil)))

;;;; Main entry points

;;;###autoload
(defun koreader/highlight-chapter ()
  "Apply KOReader highlights for the current chapter to the buffer.
Reports how many highlights were applied.  Does nothing if
`koreader/-epub-data' is nil."
  (interactive)
  (koreader/-assert-nov-mode)
  (if (not koreader/-epub-data)
      (when (called-interactively-p 'interactive)
        (message "koreader: no highlight data loaded.  Call `koreader/load-highlights' first."))
    (koreader/clear-highlights)
    (let* ((chapter         (koreader/get-chapter))
           (highlights      (alist-get 'highlights koreader/-epub-data))
           (chapter-entries (cdr (assoc chapter highlights)))
           (count           0))
      (dolist (entry chapter-entries)
        (when-let ((text (alist-get 'text entry)))
          (let ((note  (alist-get 'note  entry))
                (page  (alist-get 'page  entry))
                (color (alist-get 'color entry)))
            (save-excursion
              (goto-char (point-min))
              (while (search-forward text nil t)
                (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                  (overlay-put ov 'face            (koreader/-color-face color))
                  (overlay-put ov 'koreader-highlight t)
                  (when note (overlay-put ov 'koreader-note note))
                  (when page (overlay-put ov 'koreader-page page))))))
          (cl-incf count)))
      (message "koreader: applied %d %s for \"%s\""
               count
               (koreader/-plural count "highlight")
               (or chapter "unknown chapter")))))

(defun koreader/-find-json-for-current-buffer ()
  "Return a matching KOReader JSON file path for the current nov buffer.
Searches the directory of the current EPUB for JSON files whose `title'
field matches the current book title.  Returns nil if none is found."
  (when-let* ((file      (buffer-file-name))
              (dir       (file-name-directory (expand-file-name file)))
              (matches   (directory-files dir t "\\.json\\'" t))
              (nov-title (alist-get 'title nov-metadata)))
    (cl-find-if
     (lambda (json-file)
       (when-let ((data (koreader/-json-peek json-file)))
         (equal (alist-get 'title data) nov-title)))
     matches)))

;;;###autoload
(defun koreader/load-highlights (filepath)
  "Load KOReader highlights from FILEPATH and apply them to the current chapter.
When called interactively, checks if a matching JSON file exists in the same
directory as the current EPUB and uses it automatically without prompting.
Falls back to `read-file-name' if no matching file is found.
Reloads the JSON only when the cached title differs from the current book.
With a prefix argument, forces a reload regardless."
  (interactive
   (list (let ((auto (and (not current-prefix-arg)
                          (koreader/-find-json-for-current-buffer))))
           (if auto
               (progn
                 (message "koreader: auto-detected %s" (file-name-nondirectory auto))
                 auto)
             (read-file-name "KOReader JSON export: "
                             koreader/exports-directory nil t)))))
  (koreader/-assert-nov-mode)
  (let ((current-title (alist-get 'title nov-metadata))
        (cached-title  (alist-get 'title koreader/-epub-data)))
    (when (or current-prefix-arg
              (not (equal current-title cached-title)))
      (message "koreader: loading highlights from %s..." filepath)
      (setq koreader/-epub-data (koreader/-parse-json-file filepath))))
  (koreader/highlight-chapter))

(defun koreader/reload ()
  "Force re-parse the JSON file previously loaded for this buffer."
  (interactive)
  (koreader/-assert-nov-mode)
  (if-let ((filepath (alist-get 'filepath koreader/-epub-data)))
      (progn
        (message "koreader: reloading from %s..." filepath)
        (setq koreader/-epub-data (koreader/-parse-json-file filepath))
        (koreader/highlight-chapter))
    (user-error "koreader: no file loaded yet.  Call `koreader/load-highlights' first.")))

;;;; Highlights buffer
;;
;; Text properties used in this buffer:
;;   koreader-entry   — non-nil on the entire region of a highlight entry
;;   koreader-text    — the raw highlight string, on the entry region
;;   koreader-note    — the note string, on the entry region
;;   koreader-page    — the page string, on the entry region
;;   koreader-chapter — chapter title string, on the entry region
;;   koreader-fold    — non-nil on the chapter heading region (foldable marker)
;;
;; Overlays are used only for fold visibility: one overlay per chapter body
;; with the `invisible' property, following the org-mode convention.
;;
;; Standalone mode: when opened via `koreader/browse-highlights', the buffer
;; has no associated nov buffer (`koreader/-highlights-nov-buffer' is nil).
;; The EPUB filepath stored in `koreader/-highlights-epub-data' is used to
;; find and open the EPUB on demand when the user presses RET.

(defvar-local koreader/-highlights-nov-buffer nil
  "The nov buffer this highlights buffer was opened from, or nil in standalone mode.")

(defvar-local koreader/-highlights-epub-data nil
  "The epub-data alist used to build this buffer.
Set in both standalone and nov-backed modes; used for refresh and jump-on-demand.")

(defvar-local koreader/-highlights-search-term nil
  "The last search term used in this highlights buffer.")

;;;; Highlights buffer keymap and mode

(defvar koreader/highlights-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?")   #'koreader/highlights-help)
    (define-key map (kbd "n")   #'koreader/highlights-next)
    (define-key map (kbd "p")   #'koreader/highlights-previous)
    (define-key map (kbd "C-j") #'koreader/highlights-next)
    (define-key map (kbd "C-k") #'koreader/highlights-previous)
    (define-key map (kbd "TAB") #'koreader/highlights-toggle-chapter)
    (define-key map (kbd "e")   #'koreader/show-note-at-point)
    (define-key map (kbd "w")   #'koreader/highlights-copy-at-point)
    (define-key map (kbd "s")   #'koreader/highlights-search)
    (define-key map (kbd "E")   #'koreader/export-to-org)
    (define-key map (kbd "g")   #'koreader/highlights-refresh)
    (define-key map (kbd "RET") #'koreader/highlights-jump-to-passage)
    map)
  "Keymap for `koreader/highlights-buffer-mode'.")

(define-derived-mode koreader/highlights-buffer-mode special-mode "KOReader"
  "Major mode for the KOReader highlights buffer.
\\{koreader/highlights-buffer-mode-map}"
  :group 'koreader-epub-highlights
  (setq-local imenu-create-index-function #'koreader/-imenu-index)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (koreader/highlights-refresh))))

;; Evil keybindings — set up when evil is available.
(with-eval-after-load 'evil
  (evil-define-key 'normal koreader/highlights-buffer-mode-map
    (kbd "?")   #'koreader/highlights-help
    (kbd "C-j") #'koreader/highlights-next
    (kbd "C-k") #'koreader/highlights-previous
    (kbd "TAB") #'koreader/highlights-toggle-chapter
    (kbd "RET") #'koreader/highlights-jump-to-passage
    (kbd "e")   #'koreader/show-note-at-point
    (kbd "y")   #'koreader/highlights-copy-at-point
    (kbd "/")   #'koreader/highlights-search
    (kbd "n")   #'koreader/highlights-search-next
    (kbd "E")   #'koreader/export-to-org
    (kbd "gr")  #'koreader/highlights-refresh
    (kbd "q")   #'quit-window
    (kbd "z o") #'koreader/highlights-toggle-chapter
    (kbd "z c") #'koreader/highlights-toggle-chapter))

;;;; Imenu integration

(defun koreader/-imenu-index ()
  "Build an imenu index from chapter headings in the highlights buffer."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (get-text-property (point) 'koreader-fold)
          ;; The koreader-chapter property on the heading holds the title string
          (let ((title (get-text-property (point) 'koreader-chapter)))
            (when (stringp title)
              (push (cons title (point)) index))))
        (let ((next (next-single-property-change (point) 'koreader-fold
                                                 nil (point-max))))
          (goto-char (or next (point-max))))))
    (nreverse index)))

;;;; Help buffer

(defun koreader/highlights-help ()
  "Show a help buffer listing all keybindings for the highlights buffer."
  (interactive)
  (let* ((evil-p (and (featurep 'evil) (bound-and-true-p evil-mode)))
         (bindings
          `(("Navigation" .
             (,(if evil-p "C-j / C-k" "n / p") "next / previous highlight"
              ,@(unless evil-p '("C-j / C-k" "next / previous highlight (also)"))))
            ("Chapter Folding" .
             ("TAB" "toggle fold"
              ,@(when evil-p '("z o / z c" "open / close fold"))))
            ("Actions" .
             ("RET"               "jump to passage in nov buffer"
              "e"                 "show note at point"
              ,(if evil-p "y" "w") "copy highlight text"
              ,(if evil-p "/" "s") "search highlights"
              ,@(when evil-p '("n" "next search match"))
              "E"                 "export all to Org"))
            ("Buffer" .
             (,(if evil-p "gr" "g") "refresh buffer"
              "q"                 "quit"
              "?"                 "show this help")))))
    (with-current-buffer (get-buffer-create "*koreader-help*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "KOReader Highlights Buffer — Keybindings\n")
        (insert (make-string 42 ?─) "\n")
        (pcase-dolist (`(,section . ,pairs) bindings)
          (insert "\n" section "\n")
          (cl-loop for (key desc) on pairs by #'cddr
                   do (insert (format "  %-18s %s\n" key desc))))
        (special-mode)
        (goto-char (point-min))))
    (display-buffer "*koreader-help*"
                    '(display-buffer-below-selected (window-height . 22)))))

;;;; Navigation
;;
;; next-single-property-change returns the BOUNDARY — the first char where
;; the property changes.  nil->t boundary = start of region; go there.
;; t->nil boundary = first char without property; skip past it.

(defun koreader/highlights-next ()
  "Jump to the start of the next highlight entry in the highlights buffer."
  (interactive)
  (let* ((pos (if (get-text-property (point) 'koreader-entry)
                  (next-single-property-change (point) 'koreader-entry nil (point-max))
                (point)))
         (pos (next-single-property-change pos 'koreader-entry nil (point-max))))
    (if (and pos (< pos (point-max)) (get-text-property pos 'koreader-entry))
        (goto-char pos)
      (user-error "No next highlight"))))

(defun koreader/highlights-previous ()
  "Jump to the start of the previous highlight entry in the highlights buffer."
  (interactive)
  (let* ((pos (if (get-text-property (point) 'koreader-entry)
                  (previous-single-property-change (point) 'koreader-entry nil (point-min))
                (point)))
         (pos (when (> pos (point-min)) (1- pos)))
         (pos (when pos
                (if (get-text-property pos 'koreader-entry)
                    (previous-single-property-change pos 'koreader-entry nil (point-min))
                  nil))))
    (if (and pos (get-text-property pos 'koreader-entry))
        (goto-char pos)
      (user-error "No previous highlight"))))

;;;; Folding
;;
;; The fold overlay records its body-start position in koreader-body-start
;; so it can be located from anywhere in the buffer.

(defun koreader/-find-chapter-fold-overlay ()
  "Return the fold visibility overlay for the chapter at or before point."
  (let ((pos (point)))
    (unless (get-text-property pos 'koreader-fold)
      (let ((boundary (previous-single-property-change pos 'koreader-fold nil (point-min))))
        (setq pos (if (and boundary (> boundary (point-min)))
                      (1- boundary)
                    nil))))
    (when (and pos (get-text-property pos 'koreader-fold))
      (let ((body-start (next-single-property-change pos 'koreader-fold nil (point-max))))
        (cl-find-if (lambda (o)
                      (and (overlay-get o 'koreader-fold-body)
                           (eql (overlay-get o 'koreader-body-start) body-start)))
                    (overlays-in body-start (min (1+ body-start) (point-max))))))))

(defun koreader/highlights-toggle-chapter ()
  "Toggle visibility of the chapter section at point."
  (interactive)
  (if-let ((ov (koreader/-find-chapter-fold-overlay)))
      (overlay-put ov 'invisible (not (overlay-get ov 'invisible)))
    (user-error "No foldable chapter found at or before point")))

;;;; Copy

(defun koreader/highlights-copy-at-point ()
  "Copy the highlight text at point to the kill ring."
  (interactive)
  (if-let ((text (get-text-property (point) 'koreader-text)))
      (progn
        (kill-new text)
        (message "Copied: %s" (truncate-string-to-width text 60 nil nil "...")))
    (user-error "No highlight text at point")))

;;;; Search

(defun koreader/-highlights-search-advance (term)
  "Move point to the next entry whose `koreader-text' contains TERM.
Wraps around to the beginning of the buffer if no match is found forward."
  (let* ((start (point))
         (pred  (lambda (val prop)
                  (and prop (string-match-p (regexp-quote val) prop))))
         (match (save-excursion
                  (text-property-search-forward 'koreader-text term pred))))
    (if match
        (goto-char (prop-match-beginning match))
      (let ((wrapped (save-excursion
                       (goto-char (point-min))
                       (text-property-search-forward 'koreader-text term pred))))
        (if (and wrapped (< (prop-match-beginning wrapped) start))
            (progn
              (goto-char (prop-match-beginning wrapped))
              (message "koreader: wrapped around"))
          (user-error "No highlight matching: %s" term))))))

(defun koreader/highlights-search (term)
  "Search highlight text in the highlights buffer.
Advances to the next match on repeated calls with the same term."
  (interactive
   (list (read-string "Search highlights: " koreader/-highlights-search-term)))
  (setq koreader/-highlights-search-term term)
  (koreader/-highlights-search-advance term))

(defun koreader/highlights-search-next ()
  "Advance to the next match for the last search term."
  (interactive)
  (if koreader/-highlights-search-term
      (koreader/-highlights-search-advance koreader/-highlights-search-term)
    (user-error "No previous search term")))

;;;; Refresh

(defun koreader/highlights-refresh ()
  "Rebuild the highlights buffer from current data.
In nov-backed mode, delegates to `koreader/show-highlights-buffer' in the
associated nov buffer.  In standalone mode, re-parses the JSON and rebuilds
the buffer in place, preserving any existing nov buffer link."
  (interactive)
  (cond
   ((and koreader/-highlights-nov-buffer
         (buffer-live-p koreader/-highlights-nov-buffer))
    (with-current-buffer koreader/-highlights-nov-buffer
      (koreader/show-highlights-buffer)))
   (koreader/-highlights-epub-data
    (let* ((filepath (alist-get 'filepath koreader/-highlights-epub-data))
           (fresh    (koreader/-parse-json-file filepath))
           ;; Preserve any nov-buffer link established since last build
           (nov-link koreader/-highlights-nov-buffer))
      (koreader/-build-highlights-buffer (current-buffer) fresh nov-link)))
   (t
    (user-error "koreader: no data source available for refresh"))))

;;;; Jump to passage

(defun koreader/-path-suffix-match-p (full-path short-path)
  "Return t if FULL-PATH ends with the tail components of SHORT-PATH.
Handles EPUBs with root-level files (one component) as well as the common
subdirectory case (two components)."
  (let* ((components  (cl-remove "" (file-name-split full-path) :test #'string=))
         (short-parts (cl-remove "" (file-name-split short-path) :test #'string=))
         (n           (length short-parts))
         (tail        (last components n)))
    (equal tail short-parts)))

(defun koreader/-doc-index-for-chapter (chapter-title nov-buf)
  "Return the nov-documents index for CHAPTER-TITLE in NOV-BUF, or nil."
  (with-current-buffer nov-buf
    (when-let ((pair (rassoc chapter-title koreader/-toc-alist)))
      (let ((path-short (car pair)))
        (cl-loop for i from 0 below (length nov-documents)
                 for doc-path = (cdr (aref nov-documents i))
                 when (koreader/-path-suffix-match-p doc-path path-short)
                 return i)))))

(defun koreader/-ensure-nov-buffer ()
  "Return a live nov buffer, opening the EPUB on demand if necessary.
In nov-backed mode, returns the stored buffer.  In standalone mode, searches
for an already-open nov buffer matching the book title.  If none exists,
locates the EPUB in the same directory as the JSON file (word-by-word fuzzy
match on the title, or prompt) and opens it, loading highlights and linking
the buffers so future calls are instant."
  (cond
   ;; Nov-backed: return the existing live buffer
   ((and koreader/-highlights-nov-buffer
         (buffer-live-p koreader/-highlights-nov-buffer))
    koreader/-highlights-nov-buffer)
   ;; Standalone: find or open the EPUB
   (koreader/-highlights-epub-data
    ;; Capture epub-data NOW before any buffer switch
    (let* ((epub-data  koreader/-highlights-epub-data)
           (json-path  (alist-get 'filepath epub-data))
           (title      (alist-get 'title    epub-data))
           (existing   (cl-find-if
                        (lambda (buf)
                          (with-current-buffer buf
                            (and (derived-mode-p 'nov-mode)
                                 (equal (alist-get 'title nov-metadata) title))))
                        (buffer-list))))
      (or existing
          (let* ((dir        (file-name-directory json-path))
                 (candidates (directory-files dir t "\\.epub\\'" t))
                 ;; Build a list of lowercase words from the title for fuzzy match
                 (words      (split-string (downcase title) "[^a-zA-Z0-9]+" t))
                 (epub-path
                  (or (cl-find-if
                       (lambda (f)
                         (let ((base (downcase (file-name-base f))))
                           (cl-every (lambda (w) (string-match-p (regexp-quote w) base))
                                     words)))
                       candidates)
                      (read-file-name
                       (format "EPUB for \"%s\": " title)
                       dir nil t nil
                       (lambda (f) (or (file-directory-p f)
                                       (string-match-p "\\.epub\\'" f))))))
                 (highlights-buf (current-buffer)))
            (find-file epub-path)
            (unless (derived-mode-p 'nov-mode)
              (user-error "koreader: %s did not open in nov-mode" epub-path))
            ;; epub-data captured above is safe here after the buffer switch
            (setq koreader/-epub-data epub-data)
            (koreader/highlight-chapter)
            (let ((nov-buf (current-buffer)))
              (with-current-buffer highlights-buf
                (setq koreader/-highlights-nov-buffer nov-buf))
              nov-buf)))))
   (t
    (user-error "koreader: no EPUB associated with this highlights buffer"))))

(defun koreader/highlights-jump-to-passage ()
  "Jump to the passage at point in the nov buffer.
In standalone mode, opens the EPUB automatically if not already open.
Marks the entry as visited and falls back to a prefix search if the exact
text is not found (handles encoding differences between KOReader and nov)."
  (interactive)
  (let ((text    (get-text-property (point) 'koreader-text))
        (chapter (get-text-property (point) 'koreader-chapter)))
    (unless text (user-error "No highlight at point"))
    (let* ((entry-pos      (point))
           (highlights-buf (current-buffer))
           (nov-buf        (koreader/-ensure-nov-buffer))
           (index          (koreader/-doc-index-for-chapter chapter nov-buf)))
      (unless index
        (user-error "koreader: could not find chapter \"%s\" in nov documents" chapter))
      ;; Mark entry as visited before switching buffers.
      ;; `previous-single-property-change' returns the boundary position, i.e.
      ;; the first char WITHOUT the property going backwards.  So if point is at
      ;; the very start of an entry that boundary is the preceding gap char, and
      ;; we must use point itself as the start.  We therefore only walk backwards
      ;; when the char *before* entry-pos also has koreader-entry (meaning we are
      ;; somewhere inside the entry, not at its leading edge).
      (with-current-buffer highlights-buf
        (let* ((inhibit-read-only t)
               (entry-start
                (if (and (> entry-pos (point-min))
                         (get-text-property (1- entry-pos) 'koreader-entry))
                    ;; Mid-entry: walk back to the nil->t boundary
                    (or (previous-single-property-change
                         entry-pos 'koreader-entry nil (point-min))
                        (point-min))
                  ;; Already at entry start (or not in an entry at all)
                  entry-pos))
               (entry-end (next-single-property-change
                           entry-pos 'koreader-entry nil (point-max))))
          (when (and entry-start entry-end
                     (get-text-property entry-start 'koreader-entry))
            (add-face-text-property entry-start entry-end
                                    'koreader/buffer-visited-face))))
      (pop-to-buffer nov-buf)
      (unless (= nov-documents-index index)
        (nov-goto-document index))
      (goto-char (point-min))
      (cond
       ((search-forward text nil t)
        (goto-char (match-beginning 0)))
       ((search-forward (substring text 0 (min 40 (length text))) nil t)
        (goto-char (match-beginning 0))
        (message "koreader: approximate match (encoding may differ)"))
       (t
        (user-error "koreader: could not find highlight text in rendered chapter"))))))

;;;; Buffer rendering

(defun koreader/-insert-heading (chapter-title n-highlights n-notes)
  "Insert a chapter heading into the current highlights buffer.
Records positions directly after each insert rather than computing byte offsets,
so multibyte characters in CHAPTER-TITLE are handled correctly."
  (let ((fold-start (point)))
    ;; Title
    (let ((p (point)))
      (insert koreader/heading-glyph " " chapter-title)
      (add-face-text-property p (point) 'koreader/buffer-chapter-face))
    (insert "  ")
    ;; Count
    (let ((p (point)))
      (insert (format "%d %s, %d with notes"
                      n-highlights
                      (koreader/-plural n-highlights "highlight")
                      n-notes))
      (add-face-text-property p (point) 'koreader/buffer-chapter-count-face))
    (insert "\n")
    ;; Rule
    (let ((p (point)))
      (insert (make-string 60 ?-) "\n\n")
      (add-face-text-property p (point) 'koreader/buffer-rule-face))
    ;; Tag the entire heading region with fold and chapter properties
    (put-text-property fold-start (point) 'koreader-chapter chapter-title)
    (put-text-property fold-start (point) 'koreader-fold    t)))

(defun koreader/-build-highlights-buffer (buf epub-data nov-buf)
  "Populate BUF with the highlights review UI from EPUB-DATA.
NOV-BUF is the associated nov buffer, or nil in standalone mode.
Mode activation is deferred until after `with-silent-modifications' closes,
to prevent mode hooks from interfering with buffer construction."
  (let* ((title      (alist-get 'title epub-data))
         (highlights (alist-get 'highlights epub-data))
         (total      (koreader/-count-highlights epub-data)))
    (with-current-buffer buf
      (with-silent-modifications
        (erase-buffer)
        ;; Title + summary line
        (koreader/-insert 'koreader/buffer-title-face title)
        (insert "  ")
        (koreader/-insert 'koreader/buffer-page-face
                          (format "(%d %s)" total (koreader/-plural total "highlight")))
        (insert "\n\n")
        ;; One section per chapter
        (dolist (chapter highlights)
          (let* ((entries      (cdr chapter))
                 (n-highlights (length entries))
                 (n-notes      (cl-count-if (lambda (e) (alist-get 'note e)) entries)))
            (koreader/-insert-heading (car chapter) n-highlights n-notes)
            (let ((body-start (point)))
              (dolist (entry entries)
                (let* ((text        (alist-get 'text  entry))
                       (note        (alist-get 'note  entry))
                       (page        (alist-get 'page  entry))
                       (color       (alist-get 'color entry))
                       (time        (alist-get 'time  entry))
                       (entry-start (point)))
                  (when text
                    (koreader/-insert (if note
                                          'koreader/buffer-annotated-quote-face
                                        'koreader/buffer-quote-face)
                                      koreader/quote-open-glyph " " text
                                      " " koreader/quote-close-glyph)
                    (when page
                      (insert "  ")
                      (koreader/-insert 'koreader/buffer-page-face
                                        (format "p.%s" page)))
                    (insert "\n"))
                  (when note
                    (koreader/-insert 'koreader/buffer-note-face
                                      "  " koreader/note-glyph " " note)
                    (insert "\n"))
                  (when (or color time)
                    (koreader/-insert
                     'koreader/buffer-page-face
                     (concat "  "
                             (when color (format "@ %s" color))
                             (when (and color time) "  ")
                             (when time (format-time-string "%Y-%m-%d"
                                                            (seconds-to-time time)))))
                    (insert "\n"))
                  ;; Tag entry region; blank separator line is excluded
                  (put-text-property entry-start (point) 'koreader-entry   t)
                  (put-text-property entry-start (point) 'koreader-chapter (car chapter))
                  (when text
                    (put-text-property entry-start (point) 'koreader-text text))
                  (when note
                    (put-text-property entry-start (point) 'koreader-note note))
                  (when page
                    (put-text-property entry-start (point) 'koreader-page page))
                  (insert "\n")))
              ;; Single overlay for fold visibility only
              (let ((body-ov (make-overlay body-start (point))))
                (overlay-put body-ov 'koreader-fold-body  t)
                (overlay-put body-ov 'koreader-body-start body-start))))))
      ;; Activate mode AFTER with-silent-modifications to avoid hook interference
      (koreader/highlights-buffer-mode)
      (setq koreader/-highlights-nov-buffer nov-buf)
      (setq koreader/-highlights-epub-data  epub-data)
      (goto-char (point-min)))))

;;;###autoload
(defun koreader/show-highlights-buffer ()
  "Display all highlights for the current book in a dedicated buffer.
Compares the currently loaded epub-data directly against what was used to
build the highlights buffer, and skips rebuilding when they match."
  (interactive)
  (koreader/-assert-nov-mode)
  (unless koreader/-epub-data
    (user-error "koreader: no highlight data loaded.  Call `koreader/load-highlights' first."))
  (let* ((title    (alist-get 'title    koreader/-epub-data))
         (filepath (alist-get 'filepath koreader/-epub-data))
         (nov-buf  (current-buffer))
         (buf-name (format "*koreader: %s*" title))
         (buf      (get-buffer-create buf-name))
         ;; Stale when the highlights buffer was built from different data
         (stale    (not (equal
                         (cons title filepath)
                         (when (buffer-live-p buf)
                           (cons (alist-get 'title
                                            (buffer-local-value
                                             'koreader/-highlights-epub-data buf))
                                 (alist-get 'filepath
                                            (buffer-local-value
                                             'koreader/-highlights-epub-data buf))))))))
    (when stale
      (koreader/-build-highlights-buffer buf koreader/-epub-data nov-buf))
    (pop-to-buffer buf)))

;;;###autoload
(defun koreader/browse-highlights (json-file)
  "Open a highlights buffer directly from JSON-FILE, without an EPUB.
The EPUB will be opened automatically the first time RET is pressed.
This is the standalone entry point for reviewing highlights without
first opening the book in nov.el."
  (interactive
   (list (read-file-name "KOReader JSON export: "
                         koreader/exports-directory nil t)))
  (let* ((epub-data (koreader/-parse-json-file json-file))
         (title     (alist-get 'title epub-data))
         (total     (koreader/-count-highlights epub-data))
         (buf       (get-buffer-create (format "*koreader: %s*" title))))
    (koreader/-build-highlights-buffer buf epub-data nil)
    (pop-to-buffer buf)
    (message "koreader: %d %s — press RET to open passage in EPUB"
             total (koreader/-plural total "highlight"))))

;;;; Org export

(defun koreader/-org-write-entry (entry)
  "Insert an Org representation of highlight ENTRY at point."
  (let ((text  (alist-get 'text  entry))
        (note  (alist-get 'note  entry))
        (color (alist-get 'color entry))
        (time  (alist-get 'time  entry)))
    (when text
      (insert (format "** Highlight%s\n"
                      (if color (format "  :%s:" (upcase color)) "")))
      (when (or time color)
        (insert ":PROPERTIES:\n")
        (when time
          (insert (format ":CREATED: %s\n"
                          (format-time-string "[%Y-%m-%d %a]"
                                              (seconds-to-time time)))))
        (when color
          (insert (format ":COLOR: %s\n" color)))
        (insert ":END:\n"))
      (insert "\n")
      (insert (format "#+begin_quote\n%s\n#+end_quote\n" text))
      (when note (insert (format "\n%s\n" note)))
      (insert "\n"))))

(defun koreader/-epub-data-for-export ()
  "Return epub-data for export commands, working from nov and highlights buffers.
Signals a `user-error' if the current buffer is neither and has no data."
  (cond
   ((derived-mode-p 'nov-mode)
    (or koreader/-epub-data
        (user-error "koreader: no highlight data loaded")))
   ((derived-mode-p 'koreader/highlights-buffer-mode)
    (or koreader/-highlights-epub-data
        (user-error "koreader: no highlight data in this buffer")))
   (t
    (user-error "koreader: export must be run from a nov or highlights buffer"))))

;;;###autoload
(defun koreader/export-to-org ()
  "Export highlights from the current book to an Org file.
Works from both a nov buffer and the highlights buffer.
Each highlight becomes a second-level heading with the text as a
#+begin_quote block, the note as body text, and color/time as properties."
  (interactive)
  (let* ((epub-data  (koreader/-epub-data-for-export))
         (title      (alist-get 'title      epub-data))
         (highlights (alist-get 'highlights epub-data))
         (filepath   (read-file-name "Export to Org file: "
                                     nil nil nil
                                     (concat (replace-regexp-in-string
                                              "[^a-zA-Z0-9]" "-"
                                              (downcase title))
                                             ".org"))))
    (with-temp-file filepath
      (insert (format "#+title: %s\n" title))
      (insert (format "#+date: %s\n\n" (format-time-string "%Y-%m-%d")))
      (dolist (chapter highlights)
        (insert (format "* %s\n\n" (car chapter)))
        (dolist (entry (cdr chapter))
          (koreader/-org-write-entry entry))))
    (message "koreader: exported to %s" filepath)
    (find-file filepath)))

;;;###autoload
(defun koreader/export-chapter-to-org ()
  "Export highlights for the current chapter only to an Org file.
Must be run from a nov buffer."
  (interactive)
  (koreader/-assert-nov-mode)
  (unless koreader/-epub-data
    (user-error "koreader: no highlight data loaded"))
  (let* ((title    (alist-get 'title koreader/-epub-data))
         (chapter  (koreader/get-chapter))
         (entries  (cdr (assoc chapter
                               (alist-get 'highlights koreader/-epub-data))))
         (filepath (read-file-name "Export chapter to Org file: "
                                   nil nil nil
                                   (concat (replace-regexp-in-string
                                            "[^a-zA-Z0-9]" "-"
                                            (downcase (or chapter "chapter")))
                                           ".org"))))
    (unless entries
      (user-error "koreader: no highlights for current chapter \"%s\"" chapter))
    (with-temp-file filepath
      (insert (format "#+title: %s -- %s\n" title chapter))
      (insert (format "#+date: %s\n\n" (format-time-string "%Y-%m-%d")))
      (insert (format "* %s\n\n" chapter))
      (dolist (entry entries)
        (koreader/-org-write-entry entry)))
    (message "koreader: exported chapter to %s" filepath)
    (find-file filepath)))

;;;###autoload
(defun koreader/open (epub-file json-file)
  "Open EPUB-FILE in nov.el and load highlights from JSON-FILE.
If a KOReader JSON export is found next to the EPUB it is offered as
the default for JSON-FILE."
  (interactive
   (let* ((epub (read-file-name "EPUB file: " nil nil t nil
                                (lambda (f) (or (file-directory-p f)
                                                (string-match-p "\\.epub\\'" f)))))
          (auto (let ((dir (file-name-directory (expand-file-name epub))))
                  (cl-find-if
                   (lambda (f)
                     (when-let ((data (koreader/-json-peek f)))
                       (and (alist-get 'title data)
                            (alist-get 'entries data)
                            f)))
                   (directory-files dir t "\\.json\\'" t))))
          (json (or auto
                    (read-file-name "KOReader JSON export: "
                                    koreader/exports-directory nil t))))
     (list epub json)))
  (find-file epub-file)
  (unless (derived-mode-p 'nov-mode)
    (user-error "koreader: %s did not open in nov-mode" epub-file))
  (koreader/load-highlights json-file))

;;;###autoload
(defun koreader/setup-auto-highlight ()
  "Add `koreader/highlight-chapter' to `nov-post-html-render-hook'."
  (interactive)
  (add-hook 'nov-post-html-render-hook #'koreader/highlight-chapter)
  (message "koreader: auto-highlight enabled."))

;;;###autoload
(defun koreader/teardown-auto-highlight ()
  "Remove `koreader/highlight-chapter' from `nov-post-html-render-hook'."
  (interactive)
  (remove-hook 'nov-post-html-render-hook #'koreader/highlight-chapter)
  (message "koreader: auto-highlight disabled."))

(provide 'koreader-epub-highlights)

;;; koreader-epub-highlights.el ends here

;;; pio.el --- Extra functions for working with pio -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (plz "0.7") (marginalia "1.0"))

(require 'plz)
(require 'url-util)
(require 'json)
(require 'marginalia)

(defconst +config/pio-registry-base "https://api.registry.platformio.org/v3")

;;; --- Library search (registry API) ---

(defconst +config/pio-sort-methods
  '("relevance" "popularity" "trending" "added" "updated")
  "Valid `sort' values for the PlatformIO registry `/search' endpoint.")

(defcustom +config/pio-default-sort "relevance"
  "Default sort method used for PlatformIO registry searches.
One of `+config/pio-sort-methods'."
  :type '(choice (const "relevance")
                 (const "popularity")
                 (const "trending")
                 (const "added")
                 (const "updated"))
  :group 'pio)

(defvar +config/pio-library-data nil
  "Alist of (NAME . ITEM) from the last PlatformIO registry search.")

(defun +config/pio--fetch-search (query &optional limit sort)
  "Query the PlatformIO registry for QUERY, returning parsed JSON.
Optional SORT is one of `+config/pio-sort-methods'."
  (let ((url (format "%s/search?query=%s&limit=%d%s"
                     +config/pio-registry-base
                     (url-hexify-string query)
                     (or limit 25)
                     (if sort (format "&sort=%s" sort) ""))))
    (plz 'get url :as (lambda () (json-parse-buffer :object-type 'alist
                                               :array-type 'list)))))

(defun +config/pio-library-candidates (query &optional sort)
  "Completion table of library names for QUERY, tagged for marginalia annotation.
Optional SORT is one of `+config/pio-sort-methods'."
  (let* ((data (+config/pio--fetch-search (concat "type:library " query) nil sort))
         (items (alist-get 'items data)))
    (setq +config/pio-library-data
          (mapcar (lambda (item) (cons (alist-get 'name item) item)) items))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . pio-library))
        (complete-with-action action (mapcar #'car +config/pio-library-data)
                              string pred)))))

(defun +config/pio-library-annotate (name)
  (when-let* ((item (cdr (assoc name +config/pio-library-data))))
    (let* ((owner   (alist-get 'username (alist-get 'owner item)))
           (stars   (or (alist-get 'stars_count item) 0))
           (version (alist-get 'name (alist-get 'version item)))
           (desc    (or (alist-get 'description item) "")))
      (marginalia--fields
       ((format "@%s" owner) :face 'marginalia-key :width 22)
       ((format "v%s" version) :face 'marginalia-version :width 12)
       ((format "★%s" stars) :face 'marginalia-number :width 8)
       (desc :face 'marginalia-documentation :truncate 60)))))

(defun +config/pio--insert-library (item)
  "Insert `owner/name@^version` for ITEM at point (platformio.ini lib_deps)."
  (let ((owner (alist-get 'username (alist-get 'owner item)))
        (name (alist-get 'name item))
        (version (alist-get 'name (alist-get 'version item))))
    (insert (format "%s/%s@^%s" owner name version))
    (message "Inserted %s/%s (v%s, ★%s)"
             owner name version (alist-get 'stars_count item))))

;;;###autoload
(defun +config/pio-search-library (query &optional sort)
  "Search PlatformIO libraries for QUERY and select one via completing-read.
SORT defaults to `+config/pio-default-sort'; with a prefix arg, prompt for it."
  (interactive
   (list (read-string "Search PlatformIO libraries: ")
         (if current-prefix-arg
             (completing-read "Sort by: " +config/pio-sort-methods nil t)
           +config/pio-default-sort)))
  (let* ((table (+config/pio-library-candidates query sort)))
    (if (null +config/pio-library-data)
        (message "No PlatformIO libraries found for %S" query)
      (let* ((choice (completing-read
                      (format "Library (%d results, sorted by %s): "
                              (length +config/pio-library-data) sort)
                      table nil t))
             (item (cdr (assoc choice +config/pio-library-data))))
        (+config/pio--insert-library item)))))

;;; --- Board search (local `pio boards`) ---

(defcustom +config/pio-board-cache-file
  (expand-file-name "pio-boards.eld" user-emacs-directory)
  "File used to persist PlatformIO board data across Emacs sessions."
  :type 'file
  :group 'pio)

(defcustom +config/pio-board-cache-max-age (* 60 60 24 7)
  "Maximum age, in seconds, of the on-disk board cache before it is
considered stale and refetched automatically on next access.
Set to nil to disable expiry entirely (cache only refreshed manually
via `+config/pio-update-board-data')."
  :type '(choice (const :tag "Never expire" nil) integer)
  :group 'pio)

(defvar +config/pio-board-data nil
  "Alist of (BOARD-ID . BOARD-PLIST), the in-memory PlatformIO board cache.
Backed on disk by `+config/pio-board-cache-file'.")

(defun +config/pio--board-data-fetch ()
  "Run `pio boards --json-output' and return the parsed board alist."
  (let* ((json (shell-command-to-string "pio boards --json-output"))
         (boards (json-parse-string json :array-type 'list :object-type 'alist)))
    (mapcar (lambda (b)
              (cons (alist-get 'id b)
                    (list :name (alist-get 'name b)
                          :mcu (alist-get 'mcu b)
                          :platform (alist-get 'platform b)
                          :vendor (alist-get 'vendor b))))
            boards)))

(defun +config/pio--board-data-write (data)
  "Persist DATA, tagged with a fetch timestamp, to the on-disk cache file."
  (make-directory (file-name-directory +config/pio-board-cache-file) t)
  (with-temp-file +config/pio-board-cache-file
    (prin1 (cons (float-time) data) (current-buffer))))

(defun +config/pio--board-data-read ()
  "Read the on-disk cache as (TIMESTAMP . DATA), or nil if unreadable."
  (when (file-readable-p +config/pio-board-cache-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents +config/pio-board-cache-file)
          (read (current-buffer)))
      (error nil))))

(defun +config/pio--board-data-stale-p (timestamp)
  "Return non-nil if TIMESTAMP is older than `+config/pio-board-cache-max-age'."
  (and +config/pio-board-cache-max-age
       (> (- (float-time) timestamp) +config/pio-board-cache-max-age)))

(defun +config/pio-board-data (&optional force)
  "Return PlatformIO board data, keyed by board id.
Checks the in-memory cache first, then the on-disk cache file, and
only shells out to `pio boards --json-output' if neither is
available or the on-disk cache has expired.  With FORCE non-nil,
always refetch from `pio' and update both caches.

To disable automatic refetching on staleness entirely (and only
ever refresh via `+config/pio-update-board-data'), set
`+config/pio-board-cache-max-age' to nil."
  (cond
   ((and (not force) +config/pio-board-data)
    +config/pio-board-data)
   (t
    (let ((cached (unless force (+config/pio--board-data-read))))
      (if (and cached (not (+config/pio--board-data-stale-p (car cached))))
          (setq +config/pio-board-data (cdr cached))
        (progn
          (when (and cached (not force))
            (message "PlatformIO: board cache is stale, refetching… (disable via `+config/pio-board-cache-max-age')"))
          (let ((data (+config/pio--board-data-fetch)))
            (+config/pio--board-data-write data)
            (setq +config/pio-board-data data))))))))

;;;###autoload
(defun +config/pio-update-board-data ()
  "Force-refresh PlatformIO board data from `pio boards --json-output',
updating both the in-memory cache and the on-disk cache file.

Note: this is the only way to refresh the cache if you have set
`+config/pio-board-cache-max-age' to nil to disable automatic
staleness-based refetching."
  (interactive)
  (message "PlatformIO: fetching board data…")
  (let ((data (+config/pio-board-data 'force)))
    (message "PlatformIO: refreshed board data (%d boards) — cached to %s"
             (length data) +config/pio-board-cache-file)))

(defun +config/pio-board-candidates ()
  "Completion table of board ids, tagged for marginalia annotation."
  (let ((boards (+config/pio-board-data)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . pio-board))
        (complete-with-action action boards string pred)))))

(defun +config/pio-board-annotate (board-id)
  (when-let* ((info (alist-get board-id (+config/pio-board-data) nil nil #'equal)))
    (marginalia--fields
     ((plist-get info :name) :face 'marginalia-value :width 55)
     ((plist-get info :mcu) :face 'marginalia-type :width 25)
     ((plist-get info :vendor) :face 'marginalia-documentation :width 40))))

;;;###autoload
(defun +config/pio-search-board ()
  "Select a PlatformIO board via completing-read."
  (interactive)
  (completing-read "Board: " (+config/pio-board-candidates) nil t))

;;; --- Install ---

(defcustom +config/pio-project-root nil
  "Default PlatformIO project root for install commands.
If nil, uses `default-directory' or `projectile-project-root'/`project-root'."
  :type '(choice (const nil) directory)
  :group 'pio)

(defun +config/pio--project-root ()
  "Resolve the PlatformIO project root to run commands from."
  (or +config/pio-project-root
      (and (fboundp 'projectile-project-root) (projectile-project-root))
      (and (fboundp 'project-root)
           (when-let* ((proj (project-current))) (project-root proj)))
      default-directory))

(defun +config/pio--install-sentinel (name)
  "Return a process sentinel for a PIO install of NAME."
  (lambda (process event)
    (let ((buf (process-buffer process)))
      (cond
       ((string-match-p "finished" event)
        (message "PlatformIO: installed %s ✔" name)
        (when buf (kill-buffer buf)))
       ((string-match-p "\\(exited abnormally\\|failed\\)" event)
        (message "PlatformIO: failed to install %s — see %s" name (buffer-name buf)))))))

(defun +config/pio--install-spec (item)
  "Build the `owner/name@version' spec string for ITEM."
  (let ((owner (alist-get 'username (alist-get 'owner item)))
        (name (alist-get 'name item))
        (version (alist-get 'name (alist-get 'version item))))
    (format "%s/%s@^%s" owner name version)))

(defun +config/pio-install-library-spec (spec &optional env)
  "Install SPEC (an `owner/name@version' string) into the current PIO project.
If ENV is given, scope the install to that environment via `-e ENV'."
  (let* ((default-directory (+config/pio--project-root))
         (buf-name (format "*pio-install: %s*" spec))
         (buf (get-buffer-create buf-name))
         (args (append (list "pkg" "install" "--library" spec)
                       (when env (list "-e" env))))
         (proc (apply #'start-process "pio-install" buf "pio" args)))
    (with-current-buffer buf
      (erase-buffer)
      (special-mode))
    (set-process-sentinel proc (+config/pio--install-sentinel spec))
    (message "Installing %s…" spec)
    proc))

;;;###autoload
(defun +config/pio-install-library (query &optional sort)
  "Search PlatformIO libraries for QUERY, select one, and install it
into the current project via `pio pkg install'.
SORT defaults to `+config/pio-default-sort'; with a prefix arg, prompt for it."
  (interactive
   (list (read-string "Search PlatformIO libraries: ")
         (if current-prefix-arg
             (completing-read "Sort by: " +config/pio-sort-methods nil t)
           +config/pio-default-sort)))
  (let ((table (+config/pio-library-candidates query sort)))
    (if (null +config/pio-library-data)
        (message "No PlatformIO libraries found for %S" query)
      (let* ((choice (completing-read
                      (format "Library (%d results, sorted by %s): "
                              (length +config/pio-library-data) sort)
                      table nil t))
             (item (cdr (assoc choice +config/pio-library-data))))
        (+config/pio-install-library-spec (+config/pio--install-spec item))))))
;;; --- Examples (from installed libraries, on disk) ---

(defvar +config/pio-example-data nil
  "Alist of (DISPLAY-NAME . (:path PATH :library LIB :env ENV)) for the
last `+config/pio-find-example' invocation.")

(defun +config/pio--libdeps-root ()
  "Return the `.pio/libdeps' directory for the current project, or nil."
  (let ((dir (expand-file-name ".pio/libdeps" (+config/pio--project-root))))
    (when (file-directory-p dir) dir)))

(defun +config/pio--example-files ()
  "Scan `.pio/libdeps/<env>/<Library>/examples/' for sketch/example files.
Returns a list of plists: (:path PATH :library LIB :env ENV :name NAME)."
  (let ((root (+config/pio--libdeps-root))
        (results nil))
    (unless root
      (user-error "No `.pio/libdeps' found — build the project at least once first"))
    (dolist (env-dir (directory-files root t "^[^.]"))
      (when (file-directory-p env-dir)
        (let ((env (file-name-nondirectory env-dir)))
          (dolist (lib-dir (directory-files env-dir t "^[^.]"))
            (when (file-directory-p lib-dir)
              (let* ((lib (file-name-nondirectory lib-dir))
                     (examples-dir (expand-file-name "examples" lib-dir)))
                (when (file-directory-p examples-dir)
                  ;; each example is usually its own subdirectory containing
                  ;; a .ino/.cpp main sketch file, but flat layouts exist too
                  (dolist (f (directory-files-recursively
                              examples-dir "\\.\\(ino\\|cpp\\)\\'"))
                    (push (list :path f
                                :library lib
                                :env env
                                :name (file-name-nondirectory
                                       (directory-file-name
                                        (file-name-directory f))))
                          results)))))))))
    (nreverse results)))

(defun +config/pio-example-annotate (display-name)
  (when-let* ((info (cdr (assoc display-name +config/pio-example-data))))
    (marginalia--fields
     ((plist-get info :library) :face 'marginalia-key :width 22)
     ((plist-get info :env) :face 'marginalia-type :width 14)
     ((file-relative-name (plist-get info :path)
                          (+config/pio--libdeps-root))
      :face 'marginalia-documentation :truncate 60))))

(defun +config/pio--example-candidates ()
  "Build completion table over installed library examples."
  (let* ((files (+config/pio--example-files)))
    (setq +config/pio-example-data
          (mapcar (lambda (f)
                    (cons (format "%s" (plist-get f :name))
                          f))
                  files))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . pio-example))
        (complete-with-action action (mapcar #'car +config/pio-example-data)
                              string pred)))))

;;;###autoload
(defun +config/pio-find-example ()
  "Browse examples from installed PlatformIO libraries and open one."
  (interactive)
  (let ((table (+config/pio--example-candidates)))
    (if (null +config/pio-example-data)
        (message "No examples found in any installed library")
      (let* ((choice (completing-read
                      (format "Example (%d found): " (length +config/pio-example-data))
                      table nil t))
             (info (cdr (assoc choice +config/pio-example-data))))
        (find-file (plist-get info :path))))))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotators
               '(pio-library +config/pio-library-annotate builtin none))
  (add-to-list 'marginalia-annotators
               '(pio-board +config/pio-board-annotate builtin none))
  (add-to-list 'marginalia-annotators
               '(pio-example +config/pio-example-annotate builtin none)))

;;; pio.el ends here

(provide 'pio)

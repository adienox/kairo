;;; config-ui.el --- UI Setup -*- lexical-binding: t; -*-

(defvar +config/after-theme-change-hook nil
  "Hook run after a theme is changed.")

(defun +config/run-after-theme-change-hook (&rest _)
  "Run `+config/after-theme-change-hook` after theme change."
  (run-hooks '+config/after-theme-change-hook))

;; run `+config/after-theme-change-hook' after load-theme
(advice-add 'load-theme :after #'+config/run-after-theme-change-hook)

(use-package sleek-modeline
  :ensure (:host github :repo "abidanBrito/sleek-modeline")
  :hook
  (on-init-ui . sleek-modeline-mode)
  :custom
  (sleek-modeline-show-modal-state t)
  (sleek-modeline-size 'large)
  :config
  (add-hook! sleek-modeline-mode #'+config/toggle-spacious-padding)

  (set-face-attribute 'sleek-modeline-line-ending-face nil
                      :inherit 'shadow)

  (sleek-modeline-register-segment
   'compile
   :fn '+config/compile-status
   :side 'right
   :priority 5
   :separator t)

  (sleek-modeline-register-segment
   'dedicated
   :fn '+config/window-dedicated
   :side 'left
   :priority 2
   :separator nil))

(add-hook!
  (on-init-ui +config/after-theme-change buffer-list-update after-make-frame-functions)
  #'+config/update-modeline-color)

(defun +config/update-modeline-color ()
  (set-face-attribute 'header-line nil
		              :background (plist-get (face-attribute 'header-line :box) :color))

  (set-face-attribute 'header-line-inactive nil
		              :background (plist-get (face-attribute 'header-line :box) :color)
                      :box `(:line-width 4 :color
                                         ,(plist-get (face-attribute 'header-line :box) :color) :style nil))

  (set-face-attribute 'mode-line-highlight nil
                      :box 'unspecified)

  (set-face-attribute 'mode-line-active nil
	                  :inherit 'mode-line))

(defun +config/compile-status ()
  "Return the current time, or nil to display nothing."
  (when compilation-in-progress
    (propertize "Compiling" 'face '(:inherit warning :slant italic))))

(defun +config/window-dedicated ()
  "Return the current time, or nil to display nothing."
  (if (window-dedicated-p (selected-window))
      (propertize "󰐃 " 'face '(:inherit warning))))

(use-package hide-mode-line
  :commands hide-mode-line-mode
  :general
  (+config/leader-toggle
    "m" '(hide-mode-line-mode :wk "Modeline")))

(use-package spacious-padding
  :hook (on-init-ui . spacious-padding-mode)
  :config
  (add-hook! spacious-padding-mode #'+config/spacious-padding-refresh)
  (add-hook! +config/after-theme-change :append
    (run-with-timer 1 nil #'+config/toggle-spacious-padding)))

;;; Fix for weird behaviour of spacious-padding on tiling window managers
(defun +config/spacious-padding-refresh (&rest _)
  (when spacious-padding-mode
    (run-with-idle-timer
     1 nil
     (lambda ()
       (minibuffer-with-setup-hook #'abort-recursive-edit
         (ignore-errors (read-from-minibuffer "")))))))

;;; Fix for modeline color on theme change
(defun +config/toggle-spacious-padding ()
  "If spacious-padding-mode is enabled, disable it and re-enable it."
  (when spacious-padding-mode
    (spacious-padding-mode 0)
    (spacious-padding-mode 1)))

(defvar +config/themes-dir (expand-file-name "themes" user-emacs-directory))

(add-to-list 'custom-theme-load-path +config/themes-dir)

(load-theme 'dank-emacs t)

(require 'filenotify)

(file-notify-add-watch
 (expand-file-name "dank-emacs-theme.el" +config/themes-dir)
 '(change)
 (lambda (_event)
   (load-theme 'dank-emacs t)
   (run-hooks '+config/after-theme-change-hook)))

(defun +config/set-diff-faces ()
  (set-face-attribute 'diff-added nil
                      :foreground (face-foreground 'success nil t))

  (set-face-attribute 'diff-changed nil
                      :background 'unspecified
                      :foreground (face-foreground 'font-lock-builtin-face nil t))

  (set-face-attribute 'diff-removed nil
                      :foreground (face-foreground 'error nil t)))

(add-hook! (+config/after-theme-change on-first-file) #'+config/set-diff-faces)

;; truncate line with …
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(setq-default truncate-string-ellipsis "")
;; wrap line with —
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(set-face-attribute 'variable-pitch nil
                    :family "Inter"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :family "Maple Mono NF"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'default nil
                    :family "Maple Mono NF"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'fixed-pitch-serif nil
                    :inherit 'fixed-pitch
                    :family 'unspecified)

(add-to-list 'default-frame-alist '(font . "Maple Mono NF-14"))

(defun +config/set-fonts ()
  "Set fonts and face attributes."
  ;; setting the emoji font family
  ;; https://emacs.stackexchange.com/a/80186
  (set-fontset-font t 'emoji
                    (font-spec :family "Apple Color Emoji") nil 'prepend)

  ;; italic comments and keywords
  (set-face-attribute 'font-lock-comment-face nil :italic t)

  ;; setting the line spacing
  (setq-default line-spacing 0.02))

(add-hook! on-init-ui #'+config/set-fonts)

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; for mouse driven scrolling
(use-package ultra-scroll
  :hook (on-first-file . ultra-scroll-mode)
  :init
  (setq scroll-conservatively 20 ; or whatever value you prefer, since v0.4
        scroll-margin 0)         ; important: scroll-margin more than 0 not yet supported
  :config
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'diff-hl-flydiff-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode))

;; for keyboard driven scrolling
(use-package good-scroll
  :hook (on-first-file . good-scroll-mode)
  :bind
  ([remap evil-scroll-up]   . good-scroll-down-half-screen)
  ([remap evil-scroll-down] . good-scroll-up-half-screen)
  ([remap evil-scroll-line-to-center] . good-scroll-center-cursor)
  :config
  (defun good-scroll-center-cursor ()
    "Scroll cursor to center."
    (interactive)
    (let* ((pixel-y (cdr (posn-x-y (posn-at-point))))               ; cursor vertical position
           (half-window (/ (good-scroll--window-usable-height) 2))  ; half of usable window height
           (delta (- pixel-y half-window)))                         ; difference from center
      (good-scroll-move delta)))

  (defun good-scroll-up-half-screen ()
    "Scroll up by half screen."
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

  (defun good-scroll-down-half-screen ()
    "Scroll down by half screen."
    (interactive)
    (good-scroll-move (- (/ (good-scroll--window-usable-height) 2)))))

(use-package svg-tag-mode
  :hook
  (org-mode . svg-tag-mode)
  :config
  (require 'svg-tags)
  (plist-put svg-lib-style-default :height 0.8)
  (add-hook! +config/after-theme-change #'+config/refresh-svg-tag-mode-in-all-buffers))

(defun +config/refresh-svg-tag-mode-in-all-buffers ()
  "In every buffer where `svg-tag-mode' is enabled, turn it off then back on.
Useful for forcing the tags to re-render, e.g. after changing
`svg-tag-tags' or relevant faces."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p svg-tag-mode)
        (svg-tag-mode -1)
        (svg-tag-mode 1)))))

(use-package ligature
  :hook (on-first-buffer . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode
                          '("::" ":::" "?:" ":?" ":?>" "<:" ":>" ":<" "<:<" ">:>"
                            "__" "#{" "#[" "#(" "#?" "#!" "#:" "#=" "#_" "#__" "#_(" "]#" "#######"
                            "<<" "<<<" ">>" ">>>"
                            "{{" "}}" "{|" "|}" "{{--" "{{!--" "--}}" "[|" "|]"
                            "!!" "||" "??" "???" "&&" "&&&"
                            "//" "///" "/*" "/**" "*/"
                            "++" "+++" ";;" ";;;" ".." "..." ".?" "?."
                            "..<" ".=" "<~" "~>" "~~" "<~>" "<~~" "~~>" "-~" "~-" "~@"
                            "<>" "</" "/>" "</>" "<+" "+>" "<+>" "<*" "*>" "<*>"
                            ">=" "<=" "<=<" ">=>" "==" "===" "!=" "!==" "=/=" "=!=" "|="
                            "<=>" "<==>" "<==" "==>" "=>" "<=|" "|=>" "=<=" "=>=" ">=<"
                            ":=" "=:" ":=:" "=:=" "\\\\"
                            "--" "---" "<!--" "<#--" "<!---->"
                            "<->" "<-->" "->" "<-" "-->" "<--" ">->" "<-<" "|->" "<-|"
                            ">--" "--<" "<|||" "|||>" "<||" "||>" "<|" "|>" "<|>" "_|_"
                            "todo))" "fixme))"
                            "[TRACE]" "[DEBUG]" "[INFO]" "[WARN]" "[ERROR]" "[FATAL]"
                            "[TODO]" "[FIXME]" "[NOTE]" "[HACK]" "[MARK]")))

(defvar +config/modeline-less-frame-names '("emacs-float" "emacs-capture" "emacs-agenda")
  "Frame names whose windows should have no modeline.")

(defun +config/modeline-less-frame-p (frame)
  "Return non-nil if FRAME's name is in `+config/modeline-less-frame-names'."
  (member (frame-parameter frame 'name) +config/modeline-less-frame-names))

(defun +config/update-modeline-visibility (&optional frame)
  "Hide or restore the modeline in FRAME's windows based on its name."
  (dolist (win (window-list frame))
    (set-window-parameter
     win 'mode-line-format
     (if (+config/modeline-less-frame-p (window-frame win)) 'none nil))))

(add-hook! (after-make-frame-functions window-configuration-change) #'+config/update-modeline-visibility)

(provide 'config-ui)

;; config-ui.el ends here

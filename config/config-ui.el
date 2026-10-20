;;; config-ui.el --- UI Setup -*- lexical-binding: t; -*-

(defvar +config/after-theme-change-hook nil
  "Hook run after a theme is changed.")

(defun +config/run-after-theme-change-hook (&rest _)
  "Run `+config/after-theme-change-hook` after theme change."
  (run-hooks '+config/after-theme-change-hook))

;; run `+config/after-theme-change-hook' after load-theme
(advice-add 'load-theme :after #'+config/run-after-theme-change-hook)

(defun +config/theme-face-background (face)
  "Return the background the active theme sets for FACE, if any.
Themes are consulted in `custom-enabled-themes' order (highest
precedence first)."
  (require 'subr-x)
  (catch 'found
    (dolist (theme custom-enabled-themes)
      (let ((flat (flatten-tree (alist-get theme (get face 'theme-face)))))
        (while flat
          (when (eq (car flat) :background)
            (throw 'found (cadr flat)))
          (setq flat (cdr flat)))))
    nil))

(defun +config/apply-theme-modeline-colors ()
  "Make the mode-line faces use the active theme's own colours.
`sleek-modeline' derives its mode-line background by darkening the
`default' background, which drifts from the theme after a light/dark
switch.  Keep `sleek-modeline' box height but use the theme's
`mode-line'/`mode-line-inactive' backgrounds."
  (dolist (face '(mode-line mode-line-inactive))
    (let* ((bg (+config/theme-face-background face))
           (box (face-attribute face :box))
           (width (if (fboundp 'sleek-modeline--get-height)
                      (sleek-modeline--get-height)
                    (and (consp box) (plist-get box :line-width)))))
      (when (and bg width)
        (set-face-attribute face nil
                            :background bg
                            :box `(:line-width ,width :color ,bg :style nil)))))
  ;; `mode-line-active' is not themed by matugen; follow `mode-line'.
  (set-face-attribute 'mode-line-active nil
                      :inherit 'mode-line
                      :background 'unspecified
                      :box 'unspecified))

(use-package sleek-modeline
  :ensure (:host github :repo "abidanBrito/sleek-modeline")
  :hook
  (on-init-ui . sleek-modeline-mode)
  :custom
  (sleek-modeline-show-modal-state t)
  (sleek-modeline-size 'large)
  :config
  (add-hook! sleek-modeline-mode #'+config/refresh-spacious-padding-faces)
  ;; Don't let `sleek-modeline's derived colour win over the theme's.
  (advice-add 'sleek-modeline--update-faces :after
              #'+config/apply-theme-modeline-colors)

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
  (on-init-ui +config/after-theme-change buffer-list-update 'after-make-frame-functions)
  #'+config/update-modeline-color)

(defun +config/update-modeline-color (&rest _)
  ;; Accepts and ignores args: `after-make-frame-functions' calls hooks with
  ;; the new frame, while the other hooks that use this pass none.
  (let* ((box (face-attribute 'header-line :box))
         (border-color (plist-get box :color)))
    (set-face-attribute 'header-line nil
                        :background (or border-color 'unspecified))

    (set-face-attribute 'header-line-inactive nil
                        :background (or border-color 'unspecified)
                        :box `(:line-width 4 :color ,border-color :style nil)))

  (set-face-attribute 'mode-line-highlight nil
                      :box 'unspecified)

  (set-face-attribute 'mode-line-active nil
	                  :inherit 'mode-line))

(defun +config/refresh-modeline-faces ()
  "Re-derive mode-line/header-line faces from the current theme.
`sleek-modeline' (mode-line colour) and `spacious-padding' (box colour)
cache face attributes and refresh on their own timers after a theme
change, so force a final ordered recomputation once they have settled."
  (when (fboundp 'spacious-padding-set-faces)
    (spacious-padding-set-faces))
  (when (fboundp 'sleek-modeline--update-faces)
    (sleek-modeline--update-faces))
  (+config/update-modeline-color)
  (+config/apply-theme-modeline-colors))

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
  ;; `sleek-modeline', `spacious-padding', and the theme itself all set
  ;; mode-line/header-line faces on their own timers; once they settle,
  ;; recompute everything in a fixed order so the result does not stay one
  ;; theme behind (e.g. light then back to dark).
  (add-hook! +config/after-theme-change :append
    (run-with-timer 1 nil #'+config/refresh-modeline-faces)))

;;; Fix for weird behaviour of spacious-padding on tiling window managers
(defun +config/spacious-padding-refresh (&rest _)
  (when spacious-padding-mode
    (run-with-idle-timer
     1 nil
     (lambda ()
       (minibuffer-with-setup-hook #'abort-recursive-edit
         (ignore-errors (read-from-minibuffer "")))))))

;;; Keep spacious-padding's face colours in sync with sleek-modeline
(defun +config/refresh-spacious-padding-faces ()
  "Re-derive `spacious-padding' face colours from the current faces.
Run from `sleek-modeline-mode-hook' because `sleek-modeline' sets the
mode-line faces when it is enabled; re-deriving afterwards keeps
spacious-padding's box colours in sync."
  (when (and (bound-and-true-p spacious-padding-mode)
             (fboundp 'spacious-padding-set-faces))
    (spacious-padding-set-faces)))

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

(defvar +config/special-frame-names '("emacs-float" "emacs-capture" "emacs-agenda" "agenda")
  "Special frames, matched against a frame's `name' or `title' parameter.")

(defun +config/special-frame-p (frame)
  "Return non-nil if FRAME is one of the special frames.
Match either the `name' or the `title' frame parameter: external
launchers (e.g. the `emacs-frame' script) set only `title' via
`emacsclient -F', while frames made from Lisp set `name'."
  (or (member (frame-parameter frame 'name) +config/special-frame-names)
      (member (frame-parameter frame 'title) +config/special-frame-names)))

(defun +config/update-special-frame-windows (&optional frame)
  "Configure windows in special FRAMEs."
  (dolist (win (window-list frame))
    (let ((special-p (+config/special-frame-p (window-frame win))))
      (set-window-parameter win 'mode-line-format
                            (when special-p 'none))
      (when special-p
        (set-window-dedicated-p win t)))))

(add-hook! ('after-make-frame-functions window-configuration-change)
           #'+config/update-special-frame-windows)

(provide 'config-ui)

;; config-ui.el ends here

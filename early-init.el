;;; early-init.el --- early init -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars   . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
;;(push '(background-color . "#000000") default-frame-alist)

(setq read-process-output-max (* 2 1024 1024))

(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq load-prefer-newer t)

(setq use-package-expand-minimally t)
(setq use-package-always-ensure (not noninteractive))
(setq use-package-enable-imenu-support t)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add 'display-startup-screen :override #'ignore)

(setq initial-frame-alist
      (append '((title . "Emacs") (name . "Emacs"))
              initial-frame-alist))

;;; early-init.el ends here

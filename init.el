;;; init.el --- Emacs initial setup -*- lexical-binding: t; -*-

(defvar +config/projects-directory "~/Documents/projects"
  "Base projects directory.")
(defvar +config/journal-directory "~/Documents/journal"
  "Base journal directory.")
(defvar +config/notes-directory "~/Documents/notes"
  "Base notes directory.")
(defvar +config/tasks-file (expand-file-name "inbox/tasks.org" +config/notes-directory)
  "Tasks file.")
(defvar +config/inbox-file (expand-file-name "inbox/inbox.org" +config/notes-directory)
  "Inbox file.")
(defvar +config/emacs-directory (expand-file-name "kairo" +config/projects-directory)
  "Base Emacs directory.")
(defvar +config/setup-directory (expand-file-name "setup" +config/emacs-directory)
  "Base Setup directory.")

(add-to-list 'load-path (expand-file-name "libs/"   +config/emacs-directory))
(add-to-list 'load-path (expand-file-name "config/" +config/emacs-directory))
(add-to-list 'load-path (expand-file-name "config/langs/" +config/emacs-directory))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))

;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
;; startup overhead.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; libs
(require 'doom-macros)

;; config
(require 'config-core)
(require 'config-keybinds)
(require 'config-evil)
(require 'config-ui)
(require 'config-minibuffer)
(require 'config-helpful)
(require 'config-undo)
(require 'config-project)
(require 'config-dev)
(require 'config-shell)
(require 'config-completions)
(require 'config-buffers)
(require 'config-dired)
(require 'config-git)
(require 'config-writing)
(require 'config-ai)
(require 'config-hardware)
(require 'config-misc)

;; languages
(require 'lang-flutter)
(require 'lang-python)
(require 'lang-c)

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;; init.el ends here

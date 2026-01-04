;;; pre-early-init.el --- Pre Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

(defvar nox/emacs-directory (expand-file-name "~/.config/kairo")
  "Base Emacs directory.")
(defvar nox/emacs-config-file (expand-file-name "config.org" nox/emacs-directory)
  "Base Emacs config file.")
(defvar nox/notes-directory (expand-file-name "~/Documents/notes")
  "Base notes directory.")
(defvar nox/journal-directory (expand-file-name "journal" nox/notes-directory)
  "Base journal directory.")
(defvar nox/projects-directory (expand-file-name "~/Documents/projects")
  "Base projects directory.")
(defvar nox/tasks-file (expand-file-name "inbox/tasks.org" nox/notes-directory)
  "Tasks file.")
(defvar nox/events-file (expand-file-name "inbox/events.org" nox/notes-directory)
  "Events file.")
(defvar nox/holidays-file (expand-file-name "inbox/holidays.org" nox/notes-directory)
  "Holidays file.")
(defvar nox/inbox-file (expand-file-name "inbox/inbox.org" nox/notes-directory)
  "Inbox file.")
(defvar nox/todo-contexts '("@home" "@work" "@shop" "@phone")
  "Tasks contexts.")

(setq epa-file-encrypt-to "adwait@adhk.dev")

(add-to-list 'load-path (expand-file-name "libs/" nox/emacs-directory))

(require 'on)          ;; Doom Style Hooks
(require 'doom-macros) ;; Doom Style Macros
(require 'link-converter)
(require 'block-converter)

(setq minimal-emacs-package-initialize-and-refresh nil)

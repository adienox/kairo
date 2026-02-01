;;; nano-elfeed.el --- NANO elfeed -*- lexical-binding: t -*-

;;; Commentary:
;; Copyright (C) 2024 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/nano-elfeed
;; Keywords: news
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (elfeed) (nano-theme) (relative-date))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require 's)
(require 'hl-line)
(require 'relative-date)

(defun nano-elfeed-entry (source title date unread star &optional no-newline)
  (let* ((date (relative-date date))
         (title (replace-regexp-in-string "＆#039;" "'" title))
         (source (replace-regexp-in-string "／" "/" source))
         (title (s-truncate 90 title "…"))
         (foreground-color (if unread
                               (face-foreground 'default)
                             (face-foreground 'font-lock-comment-face nil t)))
         (background-color (face-background 'default))
         (face-upper    `(:foreground ,foreground-color
                                      :background ,background-color))
         (face-title    `(:foreground ,foreground-color
                                      :background ,background-color
                                      :inherit variable-pitch
                                      :weight bold))
         (face-source   `(:foreground ,foreground-color
                                      :background ,background-color
                                      :inherit variable-pitch
                                      :height 120
                                      :weight light))
         (face-lower    `(:foreground ,foreground-color
                                      :background ,background-color)))
    (insert (concat
             ;; Upper part
             (propertize " " 'face face-upper 'display '(raise 0.2))
             (propertize title 'face face-title 'elfeed-entry t)
             (if star
                 (concat
                  (propertize " " 'face face-upper
                              'display `(space :align-to (- right ,(length date) 4)))
                  (all-the-icons-faicon "star" :height 0.5 :v-adjust 0.3 :face face-upper)
                  (propertize " " 'face face-upper))
               (propertize " " 'face face-upper
                           'display `(space :align-to (- right ,(length date) 2))))
             (propertize date 'face face-upper)
             (propertize " " 'display "\n")

             ;; Lower part
             (propertize " " 'face face-lower 'display '(raise -0.2))
             (propertize source 'face face-source)
             (unless no-newline
               (propertize "\n"))))))


(defun nano-elfeed-search-print-entry (entry)
  "Alternative printing of elfeed entries using SVG tags."

  (let* ((date (elfeed-entry-date entry))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (unread (member 'unread (elfeed-entry-tags entry)))
         (star (member 'star (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed
                       (or (elfeed-meta feed :title)
                           (elfeed-feed-title feed)))))

    (nano-elfeed-entry feed-title title date unread star t)))

(defun nano-elfeed-search-mode ()
  (setq left-fringe-width 1
        right-fringe-width 1
        left-margin-width 0
        right-margin-width 0)
  (set-window-buffer nil (current-buffer))
  ;; fixes weird gaps in images
  (setq-local line-spacing 0)

  (setq hl-line-overlay-priority 100)
  (setq evil-normal-state-cursor '(bar . 0))
  (hl-line-mode t)
  (setq cursor-type nil))

(defun nano-elfeed-show-mode ()
  (visual-line-mode)
  ;;  (setq truncate-lines t)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    ;; (setq header-line-format nil)
    ;; (face-remap-set-base 'default '(:height 140))
    (set-buffer-modified-p nil)))

(defun nano-elfeed-next-entry ()
  (interactive)
  (setq-local cursor-type nil)
  (setq-local evil-normal-state-cursor '(bar . 0))
  (text-property-search-forward 'elfeed-entry t))

(defun nano-elfeed-prev-entry ()
  (interactive)
  (setq-local cursor-type nil)
  (setq-local evil-normal-state-cursor '(bar . 0))
  (text-property-search-backward 'elfeed-entry t))

(defun nano-elfeed-show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry
      (nano-elfeed-next-entry))
    (call-interactively #'elfeed-search-show-entry)))

(defun nano-elfeed-show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry (forward-line 1))
    (nano-elfeed-prev-entry)
    (call-interactively #'elfeed-search-show-entry)))

(setq elfeed-search-print-entry-function
      #'nano-elfeed-search-print-entry)

(bind-key "<down>" #'nano-elfeed-next-entry 'elfeed-search-mode-map)
(bind-key "j" #'nano-elfeed-next-entry 'elfeed-search-mode-map)

(bind-key "<up>" #'nano-elfeed-prev-entry 'elfeed-search-mode-map)
(bind-key "k" #'nano-elfeed-prev-entry 'elfeed-search-mode-map)

(bind-key "j" #'nano-elfeed-show-next 'elfeed-show-mode-map)
(bind-key "k" #'nano-elfeed-prev-next 'elfeed-show-mode-map)

(add-hook 'elfeed-search-mode-hook #'nano-elfeed-search-mode)
(add-hook 'elfeed-show-mode-hook #'nano-elfeed-show-mode)

(provide 'nano-elfeed)
;;; nano-elfeed.el ends here

;;; config-misc.el --- misc setup -*- lexical-binding: t; -*-

(use-package screenshot
  :ensure (:host github :repo "tecosaur/screenshot")
  :general
  (+config/leader-apps
    "s" '(screenshot :wk "Screenshot"))
  :config

  (defadvice! +config/screenshot-preserve-winner-mode-a (orig-fn &rest args)
    "Restore `winner-mode' after `screenshot--process', since screenshot.el
disables it globally as a side effect and never re-enables it. Also disables otpp-mode during screenshot."
    :around #'screenshot--process
    (let ((winner-was-on winner-mode)
          (otpp-was-on (and (fboundp 'otpp-mode) (bound-and-true-p otpp-mode))))
      (unwind-protect
          (progn
            (when otpp-was-on
              (otpp-mode -1))
            (apply orig-fn args))
        (when winner-was-on
          (winner-mode 1))
        (when otpp-was-on
          (otpp-mode 1)))))

  (defun screenshot--process-buffer (ss-buf)
    "Save a screenshot of SS-BUF to `screenshot--tmp-file' via `x-export-frames'."
    (let* (before-make-frame-hook
           delete-frame-functions
           (width (max screenshot-min-width
                       (min screenshot-max-width
                            (screenshot--max-line-length
                             ss-buf))))
           (height (screenshot--displayed-lines ss-buf))
           (frame (posframe-show
                   ss-buf
                   :position (point-min)
                   :internal-border-width screenshot-border-width
                   :min-width width
                   :width width
                   :max-width width
                   :min-height height
                   :height height
                   :max-height height
                   :lines-truncate screenshot-truncate-lines-p
                   :poshandler #'posframe-poshandler-point-bottom-left-corner
                   :hidehandler #'posframe-hide)))
      (with-current-buffer ss-buf
        (setq-local display-line-numbers screenshot-line-numbers-p)
        (when screenshot-text-only-p
          (setq-local display-line-numbers-offset
                      (if screenshot-relative-line-numbers-p
                          0 (1- screenshot--first-line-number))))
        (font-lock-ensure (point-min) (point-max))
        (redraw-frame frame)
        (redisplay t)
        (sit-for 0.15)
        (with-temp-file screenshot--tmp-file
          (insert (x-export-frames frame 'png))))
      (posframe-hide ss-buf))))

(provide 'config-misc)

;;; config-misc.el ends here

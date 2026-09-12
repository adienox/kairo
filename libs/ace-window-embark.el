;;; ace-window-embark.el --- embark integration for ace window -*- lexical-binding: t; -*-

;; copied and modified from
;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window

(require 'ace-window)
(require 'embark)


(defvar-keymap +config/window-prefix-map
  :doc "Keymap for various window-prefix maps"
  :suppress 'nodigits
  "o" #'+config/ace-window-prefix
  "0" #'+config/ace-window-prefix
  "1" #'same-window-prefix
  "2" #'split-window-vertically
  "3" #'split-window-horizontally
  "4" #'other-window-prefix
  "5" #'other-frame-prefix
  "6" #'other-tab-prefix
  "t" #'other-tab-prefix)

;; Look up the key in `+config/window-prefix-map' and call that function first.
;; Then run the default embark action.
(cl-defun +config/embark--call-prefix-action (&rest rest &key run type &allow-other-keys)
  (when-let* ((cmd (keymap-lookup
                    +config/window-prefix-map
                    (key-description (this-command-keys-vector)))))
    (funcall cmd))
  (funcall run :action (embark--default-action type) :type type rest))

;; Dummy function, will be overridden by running `embark-around-action-hooks'
(defun +config/embark-set-window () (interactive))

;; When running the dummy function, call the prefix action from above
(setf (alist-get '+config/embark-set-window embark-around-action-hooks)
      '(+config/embark--call-prefix-action))

(setf (alist-get 'buffer embark-default-action-overrides) #'pop-to-buffer-same-window
      (alist-get 'file embark-default-action-overrides) #'find-file
      (alist-get 'bookmark embark-default-action-overrides) #'bookmark-jump
      (alist-get 'library embark-default-action-overrides) #'find-library)

(map-keymap (lambda (key cmd)
              (keymap-set embark-general-map (key-description (make-vector 1 key))
                          #'+config/embark-set-window))
            +config/window-prefix-map)

(provide 'ace-window-embark)

;;; ace-window-embark.el ends here

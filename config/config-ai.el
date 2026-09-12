;;; config-ai.el --- AI Setup -*- lexical-binding: t; -*-

(use-package gptel
  :commands (gptel gptel-send)
  :hook
  (gptel-mode . gptel-highlight-mode)
  (gptel-mode . evil-insert-state)
  :general
  (+config/leader-apps
    "g" '(+config/gptel :wk "Gptel"))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist
   '((markdown-mode . "**Prompt:** ")
     (org-mode      . "*Prompt:* ")
     (text-mode     . "Prompt: ")))
  :config
  (add-hook! gptel-mode (setq-local doom-modeline-enable-word-count nil))
  (add-hook! gptel-mode (setq-local doom-modeline-position-column-line-format nil))
  (add-hook! gptel-mode (org-indent-mode -1))
  (add-hook! 'gptel-post-response-functions #'gptel-end-of-response)
  (add-hook! gptel-post-stream #'gptel-auto-scroll)

  (add-to-list 'display-buffer-alist
               '("\\*gptel-.*\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5)
                 (slot . 1)
                 (window-dedicated . t))))

(use-package gptel-openrouter
  :ensure (:host github :repo "bharadswami/gptel-openrouter")
  :after gptel
  :custom
  (gptel-backend
   (gptel-openrouter-make-backend "OpenRouter"
     :key #'+config/openrouter-api-key
     :stream t))
  :config
  (gptel-openrouter-refresh-models))

(defun +config/openrouter-api-key ()
  "Retrieve the OpenRouter API key from auth-source.
  Expects an entry in ~/.authinfo(.gpg) like:
  machine openrouter.ai login apikey password YOUR_KEY_HERE"
  (let ((match (car (auth-source-search
                     :host "openrouter.ai"
                     :require '(:secret)
                     :max 1))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "No OpenRouter API key found in auth-source"))))

;;;###autoload
(defun +config/gptel ()
  "Open gptel or gptel-send depending on universal-prefix."
  (interactive)
  (if current-prefix-arg
      (let ((current-prefix-arg '(4)))
        (call-interactively #'gptel-send))
    (pop-to-buffer (gptel (format "*gptel-%s*" +config/tab-name)))))

(provide 'config-ai)

;; config-ai.el ends here

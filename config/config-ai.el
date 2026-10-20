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
  (gptel-model 'inclusionai/ling-3.0-flash-vl)
  (gptel-prompt-prefix-alist
   '((markdown-mode . "**Prompt:** ")
     (org-mode      . "*Prompt:* ")
     (text-mode     . "Prompt: ")))
  :config
  (add-hook! gptel-mode (org-indent-mode -1))
  (add-hook! 'gptel-post-response-functions #'gptel-end-of-response)
  (add-hook! gptel-post-stream #'gptel-auto-scroll)

  (setf (alist-get 'default gptel-directives)
        (lambda () (+config/gptel-render-prompt "default")))

  (add-to-list 'display-buffer-alist
               '("\\*gptel-.*\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5)
                 (slot . 1)
                 (window-dedicated . t))))

(defun +config/gptel-render-prompt (prompt-name)
  "Read the prompt file for PROMPT-NAME."
  (let ((path (expand-file-name (format "prompts/%s.md" prompt-name)
                                +config/emacs-directory)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(use-package gptel-preset-collection
  :ensure (:host github :repo "karthink/gptel-preset-collection")
  :after gptel)

(use-package gptel-magit
  :ensure (:host github :repo "roife/gptel-magit")
  :hook (magit-mode . gptel-magit-install))

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
  "Open gptel or gptel-send depending on universal-prefix.
If the region is active, use gptel-rewrite."
  (interactive)
  (if (use-region-p)
      (call-interactively #'gptel-rewrite)
    (if current-prefix-arg
        (let ((current-prefix-arg '(4)))
          (call-interactively #'gptel-send))
      (pop-to-buffer (gptel (format "*gptel-%s*" +config/tab-name))))))

(use-package agent-shell
  :ensure-system-package
  ((pi-acp . "npm install -g pi-acp"))
  :general
  (+config/leader-apps
    "a" '(agent-shell :wk "Agent Shell"))
  :custom
  (agent-shell-preferred-agent-config '(auto . pi)))


(provide 'config-ai)

;; config-ai.el ends here

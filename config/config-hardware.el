;;; config-hardware.el --- hardware dev setup -*- lexical-binding: t; -*-

(require 'config-dev)
(require 'project-init)

(use-package pio
  :ensure
  `(:host nil
          :repo ,(expand-file-name "libs/" +config/emacs-directory)
          :files ("pio.el")))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

(use-package platformio-mode
  :ensure (:host github :repo "fabcontigiani/platformio-mode")
  :commands (platformio-upload platformio-build platformio-clean platformio-device-monitor platformio-generate-compiledb)
  :config
  (+config/add-to-consult-buffer-filter '("platformio-compilation")))

(+config/add-project-vc-root-markers "platformio.ini")

(defun +config/init-platformio ()
  "Prompt for a board and return the pio init command string."
  (let ((board (completing-read "Board ID: " (+config/pio-board-candidates)))
        (setup-files (expand-file-name "pio/" +config/setup-directory)))
    (format
     "cp %s/.* ./ && \
      pio project init --ide emacs --sample-code --board %s && \
      pio run -t compiledb && \
      echo 'layout pio' > .envrc && \
      direnv allow" setup-files board)))

(add-to-list '+config/project-init-commands
             '("platformio" . (+config/init-platformio . nil)))

(+config/compile-multi-add! '(file-exists-p "platformio.ini")
                            '("library:search"   . +config/pio-search-library)
                            '("library:install"  . +config/pio-install-library)
                            '("library:examples" . +config/pio-find-example)
                            '("platformio:build"     . platformio-build)
                            '("platformio:compiledb" . platformio-generate-compiledb)
                            '("platformio:monitor"   . platformio-device-monitor)
                            '("platformio:clean"     . platformio-clean)
                            '("platformio:upload"    . platformio-upload))

(provide 'config-hardware)

;; config-hardware.el ends here

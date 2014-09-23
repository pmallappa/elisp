;; Java mode settings

;; indent other people's java source code and use tab characters to indent
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode t)))

(provide 'emacs-java)

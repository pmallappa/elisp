(add-to-list 
 'load-path (expand-file-name (concat EMACS_PKGS "/helm")))

(require 'helm-config) 
(setq helm-candidate-number-limit 10)
;; From https://gist.github.com/antifuchs/9238468
(setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
      helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
      helm-quick-update t
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t)
(helm-mode)

  ;; I don't like the way switch-to-buffer uses history, since
  ;; that confuses me when it comes to buffers I've already
  ;; killed. Let's use ido instead.
  (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))

(global-set-key (kbd "C-c h") 'helm-mini)

 ;; Turn off ido mode in case I enabled it accidentally
(ido-mode -1)

(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'emacs-helm)

(add-to-list 
 'load-path (expand-file-name (concat EMACS_PKGS "/helm")))

(require 'helm-config) 
(helm-mode t)
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-source-buffers-list
                         helm-source-recentf
                         helm-source-buffer-not-found)
                       "*helm mini*")))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-mini)

 ;; Turn off ido mode in case I enabled it accidentally
(ido-mode -1)

(provide 'emacs-helm)

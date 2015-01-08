;; Helm configuration

;; good documentation sources
;; ==============================
;; https://tuhdo.github.io/helm-intro.html
;; http://www.xiaohanyu.me/oh-my-emacs/core/ome-completion.html
;; http://tuhdo.github.io/helm-intro.html
;; http://tuhdo.github.io/helm-projectile.html

;; I remapped the helm-command key to C-c h (vice C-x c, which is too close
;; to C-x C-c)
;; ==============================
;; C-c h l: helm-locate
;; C-c h /: helm-find
;; C-c h f: helm-for-files
;; C-c h M-x: helm-M-x
;; C-c h a: helm-apropos
;; C-c h r: helm-regexp
;; C-c h c: helm-colors
;; C-c h 8: helm-ucs
;; C-c h i: helm-imenu
;; C-c h m: helm-man-woman
;; C-c h t: helm-top
;; C-c h p: helm-list-emacs-process
;; C-c h M-y: helm-show-kill-ring

;; enable helm universally
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; display google suggestions in helm
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;; quickly find files within a project, defined in this case by a .git
;; directory
(require 'helm-ls-git)

;; other keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x c o") 'helm-occur)

(setq helm-M-x-fuzzy-match t)

;; load various helm packages

;; quickly find files within a project, defined in this case by a .git
;; directory
(require 'helm-ls-git)

(helm-mode 1)

(provide 'emacs-helm)



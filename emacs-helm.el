;; Helm configuration
;; good documentation sources
;; https://tuhdo.github.io/helm-intro.html
;; http://www.xiaohanyu.me/oh-my-emacs/core/ome-completion.html
;; http://tuhdo.github.io/helm-projectile.html

;; I remapped the helm-command key to C-c h (vice C-x c, which is too close to C-x C-c)
;; C-x c l: helm-locate
;; C-x c /: helm-find
;; C-x c f: helm-for-files
;; C-x c M-x: helm-M-x
;; C-x c a: helm-apropos
;; C-x c r: helm-regexp
;; C-x c c: helm-colors
;; C-x c 8: helm-ucs
;; C-x c i: helm-imenu
;; C-x c m: helm-man-woman
;; C-x c t: helm-top
;; C-x c p: helm-list-emacs-process
;; C-x c M-y: helm-show-kill-ring


;; enable helm universally
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

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




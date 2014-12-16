;; Desktop mode saves the state of emacs from one session to another

;; you can save multiple desktops by using different directories, and
;; specifying the directory name in the call (desktop-read dirname).
(setq desktop-path '("~/.emacs.d/desktops/"))
(setq desktop-base-file-name ".emacs.desktop")
(setq desktop-auto-save-timer nil)

;; only save desktop manually by calling `desktop-save'
(desktop-save-mode nil)

(provide 'emacs-desktop)

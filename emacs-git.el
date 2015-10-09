;;;======================================================================
;;; git installation

;;;_.======================================================================
;;;_. magit git interface
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;; ======================================================================
;;; Use ediff as a merge tool from git
;;; see http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool
;;; for details

(setq magit-diff-options '("-b")) ; ignore whitespace
(setq magit-log-arguments (quote ("--graph" "--color" "--decorate")))

;; I've seen these instructions, so don't display them on loading magit
;; anymore
(setq magit-last-seen-setup-instructions "1.4.0")

(if (eq system-type 'windows-nt)
    (setq magit-git-executable "c:/tools/Git/bin/git.exe"))
(if (eq system-type 'cygwin)
    (setq magit-git-executable "/usr/bin/git"))

(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;;==============================
;;; Add command to new Magit-2.10 popup to enable side-by-side comparison in
;;; the same way as older versions of magit
;(magit-define-popup-action 'magit-ediff-popup
;  ?e "Show" 'magit-ediff-show-unstaged)
  
;;(eval-after-load 'git-gutter+
;;  '(progn
;;     ;;; Jump between hunks
;;     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
;;     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
;;
;;     ;;; Act on hunks
;;     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
;;     (define-key git-gutter+-mode-map (kbd "C-x v r") 'git-gutter+-revert-hunks)))


;; fix the neon git gutter faces 
(copy-face 'diff-indicator-added 'git-gutter+-added)
(copy-face 'diff-indicator-removed 'git-gutter+-deleted)
(copy-face 'diff-indicator-changed 'git-gutter+-modified)

(provide 'emacs-git)

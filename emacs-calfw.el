;;;======================================================================
;;; enhanced calendar installation
;; Display diary items in the calfw buffer.

;; (require 'calfw-cal)
;;
;; M-x cfw:open-diary-calendar

;; Key binding
;; i : insert an entry on the date
;; RET or Click : jump to the entry
;; q : kill-buffer
(add-to-list 'load-path
             (expand-file-name (concat EMACS_PKGS "/calfw")))

;;;======================================================================
;;; calendar installation
;;; https://github.com/kiwanami/emacs-calfw
(require 'calfw)
(require 'calfw-org)

;;======================================================================
;; change to fixed-pitch font for this buffer
(add-hook 'cfw:calendar-mode-hook
          (lambda()
            (buffer-face-mode-invoke 'fixed-pitch t)))

(provide 'emacs-calfw)

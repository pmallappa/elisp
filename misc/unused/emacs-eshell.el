;;
;; Setup for eshell.
;;

;;;========================================
;;; Clear the eshell buffer
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;========================================
;;; Toggle full-screen eshell
(defun my_eshell ()
  "Bring up a full-screen eshell or restore previous config."
  (interactive)
  (if (string= "eshell-mode" major-mode)
      (jump-to-register :eshell-fullscreen)
    (progn
      (window-configuration-to-register :eshell-fullscreen)
      (eshell)
      (delete-other-windows))))

;;======================================================================
;; change to fixed-pitch font for this buffer (and shell mode
(add-hook 'shell-mode-hook
          (lambda()
            (buffer-face-mode-invoke 'fixed-pitch t)))

(add-hook 'eshell-mode-hook
          (lambda()
            (buffer-face-mode-invoke 'fixed-pitch t)))

(provide 'emacs-eshell)

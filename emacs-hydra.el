q;;======================================================================
;; Moving around windows
(require 'hydra)
;(key-chord-mode 1)

(defun hydra-universal-argument (arg)
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4)))))
(global-set-key
 (kbd "C-c C-v")
 (defhydra hydra-toggle (:color blue)
   "toggle"
   ("a" abbrev-mode "abbrev")
   ("d" toggle-debug-on-error "debug")
   ("f" auto-fill-mode "fill")
   ("t" toggle-truncate-lines "truncate")
   ("w" whitespace-mode "whitespace")
   ("q" nil "cancel")))

(global-set-key
 (kbd "C-c C-w")
 (defhydra hydra-window (:color blue)
  "window"
  ("b" windmove-left "left")
  ("n" windmove-down "down")
  ("p" windmove-up "up")
  ("f" windmove-right "right")
  ("a" ace-window "ace")
  ("u" hydra-universal-argument "universal")
  ("s" (lambda () (interactive) (ace-window 4)) "swap")
  ("d" (lambda () (interactive) (ace-window 16)) "delete")
  ("q" nil "cancel")))

;(key-chord-define-global "yy" 'hydra-window/body)

(provide 'emacs-hydra)


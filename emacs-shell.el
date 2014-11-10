
;; prevent yasnippets from consuming tab completion within a shell
(add-hook 'term-mode-hook
          (lambda()
            (yas-minor-mode -1))) 

(provide 'emacs-shell)

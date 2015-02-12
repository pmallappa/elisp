;;======================================================================
(if (eq system-type 'windows-nt)
    (progn
      (require 'setup-cygwin)))

(if (eq system-type 'cygwin)
    (progn
      ;; from http://superuser.com/questions/229544/running-emacs-in-cygwin

      ;; If you compile java or other things that generate filenames like
      ;; C:\whatever, you may first like to edit /etc/fstab and add a mount
      ;; from C:\ to /c such as:
      ;; C: /c ntfs binary,user 1 1

      ;; Then if you run emacs -nw inside mintty, you may like it to
      ;; recognize more keys, place into ~/.emacs:

      ;; ;; For mintty
      ;; (define-key function-key-map "\e[1;5m" [(control ?-)])
      ;; (define-key function-key-map "\e[1;5k" [(control ?=)])
      ;; (define-key function-key-map "\e[1;5q" [(control ?1)])
      ;; (define-key function-key-map "\e[1;5s" [(control ?3)])
      ;; (define-key function-key-map "\e[1;5t" [(control ?4)])
      ;; (define-key function-key-map "\e[1;5u" [(control ?5)])
      ;; (define-key function-key-map "\e[1;5w" [(control ?7)])
      ;; (define-key function-key-map "\e[1;5x" [(control ?8)])
      ;; (define-key function-key-map "\e[1;5y" [(control ?9)])
      ;; (define-key function-key-map "\e[1;5p" [(control ?0)])
      
      ;; When in cygwin, allow C:\whatever to turn into /c/whatever
      (defun cygwin-name-hook (operation &rest args)
        "Turn Windows filenames into Cygwin filenames."
        ;; Handle all operations the same
        (let ((first (car args))
              (inhibit-file-name-handlers
               (cons 'cygwin-name-hook
                     (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers)))
              (inhibit-file-name-operation operation))
          (setq first (replace-regexp-in-string "^C:" "/c" first t))
          (setq first (replace-regexp-in-string "\\\\" "/" first t))
          (apply operation (cons first (cdr args)))))

      (add-to-list 'file-name-handler-alist '("^[Cc]:" . cygwin-name-hook))      
      ))

(provide 'emacs-cygwin)

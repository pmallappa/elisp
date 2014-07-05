;; emacs package manager.
(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; Required packages
;;; everytime emacs starts, it will automatically check if those packages
;;; are missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar cm/packages
  '(bm
    bs-ext
    color-moccur
    csv-mode
    dash
    epl
    exec-path-from-shell
    git-commit-mode
    git-rebase-mode
    igrep
    magit
    org
    pkg-info
    s
    starter-kit-eshell
    sublime-themes
    tangotango-theme
    tree-mode
    w3m
    windata
    zenburn-theme)
  )

(dolist (p cm/packages)
      (if (not (package-installed-p p))
          (progn
            (if (y-or-n-p (format "%s: %s " "install missing package" p))
                (progn 
                  (package-install p)
                  (require p))
              nil))
        (require p)))

(provide 'emacs-package)

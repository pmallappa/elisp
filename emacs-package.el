;; emacs package manager.
(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Required packages
;; everytime emacs starts, it will automatically check if those packages
;; are missing
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
    tree-mode
    w3m
    windata
;    flatui-theme
;    gruber-darker-theme
;    heroku-theme
    nzenburn-theme
;    soft-charcoal-theme
;    tangotango-theme
;    zenburn-theme
  ))


;; cycle through the package list and prompt to install as necessary
(dolist (p cm/packages)
      (if (not (package-installed-p p))
          (progn
            (if (y-or-n-p (format "%s: %s " "install missing package" p))
                (progn 
                  (package-install p)
                  (require p))
              nil))
        (require p)))

;; Change the width of the package list displayed. Currently doing this by
;; redefining the entire method. Long term would be to introduce a patch to
;; allow user-defined widths, or based on the width of the emacs frame
(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq tabulated-list-format [("Package" 28 package-menu--name-predicate)
			       ("Version" 18 nil)
			       ("Status"  12 package-menu--status-predicate)
			       ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header))



(provide 'emacs-package)

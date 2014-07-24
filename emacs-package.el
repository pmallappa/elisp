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
  '(
    auto-complete
    bm
    browse-url-dwim
    bs-ext
    color-moccur
    csv-mode
    dash
    display-theme
    epl
    esh-help
    exec-path-from-shell
    git-commit-mode
    git-gutter+
    git-rebase-mode
    git-timemachine
    igrep
    list-utils
    magit
    markdown-mode
    org
    paredit
    pkg-info
    popup
    s
    starter-kit-eshell
    string-utils
    tree-mode
    w3m
    windata
    zenburn-theme
))

;; cycle through the package list and prompt to install as necessary
(if (y-or-n-p-with-timeout "Check packages? " 3 nil)
    (progn
      (dolist (pkg cm/packages)
        (if (not (package-installed-p pkg))
            (progn
              (if (y-or-n-p (format "%s: %s " "install missing package" pkg))
                  (progn 
                    (package-install pkg)
                    (require pkg))))
          (require pkg)))))

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

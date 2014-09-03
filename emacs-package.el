;; emacs package manager.
(require 'package)
(add-to-list 'package-archives
             '("melpa"        . "http://melpa.milkbox.net/packages/"))
;             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
;             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Required packages
;; everytime emacs starts, it will automatically check if those packages
;; are missing
;(when (not package-archive-contents)
;  (package-refresh-contents))

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
    elisp-slime-nav
    epl
    esh-help
    frame-cmds
    frame-fns
    git-commit-mode
    git-gutter+
    git-rebase-mode
    git-timemachine
    igrep
    js2-mode
    list-utils
    magit
    markdown-mode
    monokai-theme
    paredit
    pkg-info
    popup
    rainbow-mode-0.9
    s
    skewer-mode-readme.txt
    smartparens
    starter-kit-eshell
    string-utils
    w3m
    windata
    zenburn-theme
    ))

(if (eq system-type 'darwin)
    (add-to-list 'cm/packages 'exec-path-from-shell))


;; cycle through the package list and prompt to install as necessary
;(if (y-or-n-p-with-timeout "Check packages? " 3 nil)
;    (progn
;      (dolist (pkg cm/packages)
;        (if (not (package-installed-p pkg))
;            (progn
;              (if (y-or-n-p (format "%s: %s " "install missing package" pkg))
;                  (progn 
;                    (package-install pkg)
;                    (require pkg))))
;          (require pkg)))))

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

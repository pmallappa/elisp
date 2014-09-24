;; emacs package manager.
(require 'package)
(add-to-list 'package-archives
             '("melpa"         . "http://melpa.milkbox.net/packages/"))
;             '("marmalade"     . "http://marmalade-repo.org/packages/"))
;             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
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
    color-theme-sanityinc-solarized
    csv-mode
    dash
    diminish
    dired-hacks-utils
    elisp-slime-nav
    epl
    esh-help
    git-commit-mode
    git-gutter+
    git-rebase-mode
    git-timemachine
    ht
    htmlize
    igrep
    js2-mode
    list-utils
    magit
    markdown-mode
    neotree
    org
    org-dotemacs
    ox-pandoc
    pandoc-mode
    paredit
    pkg-info
    popup
    rainbow-mode
    s
    smartparens
    starter-kit-eshell
    string-utils
    w3m
    windata
    )
  "A list of packages to ensure are installed at launch")

(if (eq system-type 'darwin)
    (add-to-list 'cm/packages 'exec-path-from-shell))

(defun cm-packages-installed-p ()
  (loop for p in cm/packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (cm-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p cm/packages)
    (when (not (package-installed-p p))
      (package-install p))))

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

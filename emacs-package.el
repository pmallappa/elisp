;; emacs package manager.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;(when (not package-archive-contents)
;  (progn
;    (message "%s" "Emacs is now refreshing its package database...")
;    (package-refresh-contents)
;    (message "%s" " done.")))

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
    deferred
    diminish
    dired-hacks-utils
    elisp-slime-nav
    epl
    esh-help
    git-commit-mode
    git-gutter+
    git-rebase-mode
    git-timemachine
    hc-zenburn-theme
    highlight-symbol
    hl-sexp
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
    request
    request-deferred
    s
    smartparens
    sqlplus
    starter-kit-eshell
    string-utils
    w3m
    windata
    zenburn-theme)
  "Packages that will be installed/updated to the latest version on startup")

;; cycle through the package list and prompt to install as necessary
(defun cm-package-refresh ()
  (interactive)
  (if (y-or-n-p-with-timeout "Check packages? " 3 nil)
      (progn
	(dolist (pkg cm/packages)
	  (if (not (package-installed-p pkg))
	      (progn
		(if (y-or-n-p (format "%s %s " "install missing package:" pkg))
                    (package-install pkg))))))))

;; now execute the refresh code
(cm-package-refresh)

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

;; display installed packages that are not in the cm-packages list
(defun package-list-unaccounted-packages ()                          
  "Like `package-list-packages', but shows only the packages that    
            are installed and are not in `jpk-packages'.  Useful for           
            cleaning out unwanted packages."                                   
  (interactive)                                                      
  (package-show-package-list                                         
   (remove-if-not (lambda (x) (and (not (memq x cm/packages))       
                                   (not (package-built-in-p x))             
                                   (package-installed-p x)))                
                  (mapcar 'car package-archive-contents))))


(provide 'emacs-package)

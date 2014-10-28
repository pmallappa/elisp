;; emacs package manager.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup")
(setq cm/packages                                                  
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
        names
        neotree
        org
        org-dotemacs
        ox-pandoc
        pandoc-mode
        paredit
        pkg-info
        popup
        projectile
        request
        request-deferred
        s
        smartparens
        sqlplus
        starter-kit-eshell
        string-utils
        w3m
        windata
        ))

;; cycle through the package list and prompt to install as necessary
(defvar missing-pkgs '())
(defun cm-package-refresh ()
  (interactive)
  (setf missing-pkgs nil)
  (dolist (pkg cm/packages)
    (if (not (package-installed-p pkg))
        (progn
          (add-to-list 'missing-pkgs pkg)
          (setq cm-message "Done"))
      (setq cm-message "Nothing missing")))
  ;; for any missing packages, ask to load them all
  (if (and (> (length missing-pkgs) 0)
           (y-or-n-p-with-timeout
            (format "%s %s " "install missing packages:" missing-pkgs) 4 nil))
       (dolist (mpkg missing-pkgs)
        (package-install mpkg)))
  (message "%s" cm-message))

;; now check for missing packages
(if (y-or-n-p-with-timeout "Check packages? " 4 nil)
    (cm-package-refresh))

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
            are installed and are not in `cm/packages'.  Useful for           
            cleaning out unwanted packages."                                   
  (interactive)                                                      
  (package-show-package-list                                         
   (remove-if-not (lambda (x) (and (not (memq x cm/packages))       
                                   (not (package-built-in-p x))             
                                   (package-installed-p x)))                
                  (mapcar 'car package-archive-contents))))

(provide 'emacs-package)

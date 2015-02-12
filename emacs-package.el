;; emacs package manager.
(require 'package)
(add-to-list 'package-archives  '("elpa"  . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives  '("org"   . "http://orgmode.org/elpa/") t)
;(add-to-list 'package-archives  '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup")
(setq cm/packages
      '(
        2048-game
        ac-cider
        ac-helm
        anti-zenburn-theme
        async
        auto-complete
        bm
        browse-url-dwim
        bs-ext
        cider
        clojure-mode
        color-moccur
        color-theme-sanityinc-solarized
        csv-mode
        cygwin-mount
        dash
        deferred
        diminish
        dired-hacks-utils
        elisp-slime-nav
        emmet-mode
        epl
        esh-help
        f
        frame-cmds
        frame-fns
        git-commit-mode
        git-gutter
        git-gutter
        git-rebase-mode
        git-timemachine
        hc-zenburn-theme
        helm
        helm-emmet
        helm-git-grep
        helm-ls-git
        helm-package
        helm-swoop
        highlight-symbol
        hl-sexp
        ht
        htmlize
        hydra
        igrep
        js2-mode
        list-utils
        magit
        markdown-mode
        multi-web-mode
        names
        neotree
        oldlace-theme
        org
        org-dotemacs
        ox-pandoc
        pandoc-mode
        paredit
        pkg-info
        popup
        queue
        rainbow-mode
        rect+
        request
        request-deferred
        s
        setup-cygwin
        smartparens
        starter-kit-eshell
        string-utils
        tablist
        w3m
        web-mode
        windata
        wsd-mode
        yasnippet
        zenburn-theme
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
      (setq cm-message "Packages checked")))
  ;; for any missing packages, ask to load them all
  (if (and (> (length missing-pkgs) 0)
           (y-or-n-p-with-timeout
            (format "%s %s " "install missing packages:" missing-pkgs) 4 nil))
       (dolist (mpkg missing-pkgs)
        (package-install mpkg)))
  (message "%s" cm-message))

;; now check for missing packages
;; only ask if some are missing... a lot less intrusive this way
(cm-package-refresh)

;; redefining the entire method. Long term would be to introduce a patch to
;; allow user-defined widths, or based on the width of the emacs frame
(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq tabulated-list-format [("Package" 32 package-menu--name-predicate)
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
   (remove-if-not (lambda (x)
                    (and (not (memq x cm/packages))       
                         (not (package-built-in-p x))             
                         (package-installed-p x)))                
                  (mapcar 'car package-archive-contents))))

(provide 'emacs-package)

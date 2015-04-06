;; emacs package manager.
(require 'package)
(add-to-list 'package-archives  '("elpa"         . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives  '("melpa"        . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives  '("org"          . "http://orgmode.org/elpa/") t)
;(add-to-list 'package-archives  '("marmalade"    . "https://marmalade-repo.org/packages/") t)

(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup")
(setq my/packages
      '(auto-complete
        bm
        browse-url-dwim
        color-theme-sanityinc-solarized
        color-moccur
        diminish
        esh-help
        git-commit-mode
        git-gutter+
        git-rebase-mode
        git-timemachine
        gitconfig-mode
        helm
        highlight-symbol
        hl-sexp
        hydra
        js2-mode
        magit
        magit-find-file
        multi-web-mode
        org
        ox-pandoc
        rainbow-mode
        smartparens
        w3m
        xkcd))

(if (eq system-type 'darwin)
    (add-to-list 'my/packages 'exec-path-from-shell))

;; the list from the directory also includes the following, in addition to
;; the packages (dependencies)
;;archives
;;async-1.2
;;dash-2.10.0
;;gnupg
;;list-utils-20140508.1341
;;popup-20150315.612
;;string-utils-20140508.1341
;;yasnippet-0.8.0


;(setq cm/packages
;      '(
;        2048-game
;        ac-helm
;        ace-jump-mode
;        ace-link
;        async
;        auto-complete
;        bind-key
;        bm
;        browse-url-dwim
;        bs-ext
;        color-moccur
;        color-theme-sanityinc-solarized
;        csv-mode
;        cygwin-mount
;        dash
;        deferred
;        diminish
;        elisp-slime-nav
;        emmet-mode
;        epl
;        esh-help
;        f
;        frame-cmds
;        frame-fns
;        git-commit-mode
;        git-gutter+
;        git-gutter
;        git-rebase-mode
;        git-timemachine
;        helm
;        helm-bm
;        helm-c-moccur
;        helm-emmet
;        helm-firefox
;        helm-git
;        helm-git-grep
;        helm-package
;        helm-swoop
;        highlight-symbol
;        hl-sexp
;        ht
;        htmlize
;        hydra
;        igrep
;        js2-mode
;        list-utils
;        magit
;        markdown-mode
;        material-theme
;        minesweeper
;        monokai-theme
;        multi-web-mode
;        names
;        neotree
;        oldlace-theme
;        org
;        org-dotemacs
;        ox-pandoc
;        package+
;        pandoc-mode
;        paredit
;        pkg-info
;        popup
;        queue
;        rainbow-mode
;        rect+
;        request
;        request-deferred
;        s
;        setup-cygwin
;        smartparens
;        starter-kit-eshell
;        string-utils
;        tablist
;        use-package
;        w3m
;        web-mode
;        windata
;        wsd-mode
;        yasnippet
;        zenburn-theme
;        ))



;; Ensure packages are installed at startup. Prompt for any that are missing
;; Adapted from
;; http://camdez.com/blog/2014/12/07/automatically-installing-your-emacs-packages/
(require 'cl-lib)

(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (if (y-or-n-p-with-timeout
	   (format "%s %s " "install missing packages?" missing-packages) 4 nil)
	  (progn
	    (message "Installing %d missing package(s)" (length missing-packages))
	    (package-refresh-contents)
	    (mapc #'package-install missing-packages))))))

(my/install-packages)

;; redefining the entire method. Long term would be to introduce a patch to
;; allow user-defined widths, or based on the width of the emacs frame
;; <<<< here you have to adapt the number to your needs >>>>
(defcustom package-menu-column-width 30
  "Width of the package column in the package list."
  :type 'number
  :group 'package)

(defcustom version-menu-column-width 18
  "Width of the version column in the package list."
  :type 'number
  :group 'package)

(defcustom status-menu-column-width 12
  "Width of the staus column in the package list."
  :type 'number
  :group 'package)

(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq tabulated-list-format
        `[("Package" ,package-menu-column-width package-menu--name-predicate)
          ("Version" ,version-menu-column-width nil)
          ("Status"  ,status-menu-column-width package-menu--status-predicate)
          ,@(if (cdr package-archives)
                '(("Archive" 10 package-menu--archive-predicate)))
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
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

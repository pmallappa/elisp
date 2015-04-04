;; emacs package manager.
(require 'package)
(add-to-list 'package-archives  '("elpa"         . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives  '("melpa"        . "http://melpa.org/packages/") t)
(add-to-list 'package-archives  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives  '("org"          . "http://orgmode.org/elpa/") t)
;(add-to-list 'package-archives  '("marmalade"    . "https://marmalade-repo.org/packages/") t)

(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup")
(setq cm/packages
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
        org-pandoc
        rainbow-mode
        smartparens
        w3m))

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

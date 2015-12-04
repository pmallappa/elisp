;; emacs package manager.
(require 'package)
(add-to-list 'package-archives '("elpa"         . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives  '("marmalade"    . "https://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives  '("org"          . "http://orgmode.org/elpa/") t)

;; prioritize the package repositories (emacs 25+)
(if (>= emacs-major-version 25)
    (progn
     (setq package-archive-priorities
           '(("elpa" . 40)
             ("melpa-stable" . 20)
             ("melpa" . 10)
;             ("marmalade" . 20)
             ("gnu" . 10)))))

(setq package-menu-hide-low-priority t)

(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command in the .emacs.d/elpa directory:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup")
(setq cm/packages
      '(
        aggressive-indent
        alect-themes
        ample-zen-theme
        atom-one-dark-theme
        auto-complete
        autumn-light-theme
        base16-theme
        bind-key
        bm
        bookmark+
        browse-url-dwim
        color-moccur
        color-theme-sanityinc-solarized
        color-theme-sanityinc-tomorrow
        darktooth-theme
        diminish
        esh-help
        faff-theme
        frame-cmds
        git-gutter-fringe
        git-timemachine
        gitconfig-mode
        highlight-symbol
        hl-sexp
        js2-mode
        magit-find-file
        multi-web-mode
        org-pandoc
        orgit
        ox-pandoc
        pkg-info
        rainbow-mode
        s
        smartparens
        swiper-helm
        w3m
        web-mode
        worf
        xkcd
        yasnippet
        zenburn-theme
        ))

(if (eq system-type 'darwin)
    (add-to-list 'cm/packages 'exec-path-from-shell))

;; Ensure packages are installed at startup. Prompt for any that are missing
;; Adapted from
;; http://camdez.com/blog/2014/12/07/automatically-installing-your-emacs-packages/
(require 'cl-lib)

(defun cm/install-packages ()
  "Ensure the packages I use are installed. See `cm/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p cm/packages)))
    (when missing-packages
      (if (y-or-n-p-with-timeout
	   (format "%s %s " "install missing packages?" missing-packages) 10 nil)
	  (progn
	    (message "Installing %d missing package(s)" (length missing-packages))
	    (package-refresh-contents)
	    (mapc #'package-install missing-packages))))))

(cm/install-packages)

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

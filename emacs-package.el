;; emacs package manager.
(require 'package)
(add-to-list 'package-archives  '("elpa"         . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives  '("org"          . "http://orgmode.org/elpa/") t)
;(add-to-list 'package-archives  '("marmalade"    . "https://marmalade-repo.org/packages/") t)

;; prioritize the package repositories (emacs 25+)
(if (>= emacs-major-version 25)
    (progn
     (setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("marmalade" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))
     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)))


(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command in the .emacs.d/elpa directory:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup")
(setq my/packages
      '(aggressive-indent
        auto-complete
        bm
        bookmark+
        browse-url-dwim
        color-moccur
        color-theme-sanityinc-solarized
        diminish
        esh-help
        exec-path-from-shell
        frame-cmds
        git-timemachine
        gitconfig-mode
        helm
        highlight-symbol
        hl-sexp
        hydra
        js2-mode
        magit-find-file
        multi-web-mode
        org-pandoc
        orgit
        ox-pandoc
        rainbow-mode
        smartparens
        w3m
        web-mode
        xkcd))

(if (eq system-type 'darwin)
    (add-to-list 'my/packages 'exec-path-from-shell))

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

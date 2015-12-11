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
             ("gnu" . 10)))))
;             ("marmalade" . 20)

(setq package-menu-hide-low-priority t)

(package-initialize)

;; a good way to get a formatted list of the packages loaded is with the
;; following shell command in the .emacs.d/elpa directory:
;; ls | sed -e s/-[0-9.].*//
(defvar cm/packages nil
  "Packages that will be installed/updated to the latest version on startup.
These don't include the associated dependencies")
(setq cm/packages
      '(
        aggressive-indent                  ; Minor mode to aggressively keep your code always indented
        auto-complete                      ; Auto Completion for GNU Emacs
        bind-key                           ; A simple way to manage personal keybindings
        bm                                 ; Visible bookmarks in buffer.
        bookmark+                          ; Bookmark+: extensions to standard library `bookmark.el'.
        browse-url-dwim                    ; Context-sensitive external browse URL or Internet search
        color-moccur                       ; multi-buffer occur (grep) mode
        color-theme-sanityinc-solarized    ; A version of Ethan Schoonover's Solarized themes
        diminish                           ; Diminished modes are minor modes with no modeline display
        esh-help                           ; Add some help functions and support for Eshell
        frame-cmds                         ; Frame and window commands (interactive functions).
        git-gutter-fringe                  ; Fringe version of git-gutter.el
        git-timemachine                    ; Walk through git revisions of a file
        gitconfig-mode                     ; Major mode for editing .gitconfig files
        helm-swoop                         ; Efficiently hopping squeezed lines powered by helm interface
        highlight-symbol                   ; automatic and manual symbol highlighting
        hl-sexp                            ; highlight the current sexp
        ht                                 ; The missing hash table library for Emacs
        js2-mode                           ; Improved JavaScript editing mode
        magit-find-file                    ; completing-read over all files in Git
        multi-web-mode                     ; multiple major mode support for web editing
        orgit                              ; support for Org links to Magit buffers
        pkg-info                           ; Information about packages
        rainbow-mode                       ; Colorize color names in buffers
        s                                  ; The long lost Emacs string manipulation library.
        smartparens                        ; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
        w3m                                ; an Emacs interface to w3m
        web-mode                           ; major mode for editing web templates
        worf                               ; A warrior does not press so many keys! (in org-mode)
        xkcd                               ; View xkcd from Emacs
        yasnippet                          ; Yet another snippet extension for Emacs.
        zenburn-theme                      ; A low contrast color theme for Emacs.
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
(defcustom package-menu-column-width 34
  "Width of the package column in the package list."
  :type 'number
  :group 'package)

(defcustom version-menu-column-width 14
  "Width of the version column in the package list."
  :type 'number
  :group 'package)

(defcustom status-menu-column-width 12
  "Width of the staus column in the package list."
  :type 'number
  :group 'package)

(defcustom archive-menu-column-width 14
  "Width of the archive column in the package list."
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
                `(("Archive" ,archive-menu-column-width package-menu--archive-predicate)))
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

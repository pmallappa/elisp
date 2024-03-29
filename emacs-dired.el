;;; -*-mode: emacs-lisp -*-

;;; Time-stamp: <2007-04-11 11:08:01 ak9384>

(require 'dired)

;;======================================================================
;; Use the Recycle Bin when deleting from dired (t)
(setq delete-by-moving-to-trash nil)

;;======================================================================
;; Let dired guess the target directory
;; If another dired window is visible, use that as the target for
;; copy, move, etc
(setq dired-dwim-target t)

;;======================================================================
;; auto-compression-mode
;; Use the dired compression mode for compression instead of jka-compr, as it
;; supports many more operations than the jka package
(auto-compression-mode t)

;; Set the ls switches
(setq dired-listing-switches "-la")
(setq dired-use-ls-dired nil)

;;======================================================================
;; dired-a 
;; dired-a provides added functions to dired, including recursive
;; copy and delete (to handle entire directories) and archiving
(load "dired-a")

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies  'always)

;; Alist with information how to add files to an archive (from dired-a)
;; Each element has the form (REGEXP ADD-CMD NEW-CMD). If REGEXP matches
;; the file name of a target, that target is an archive and ADD-CMD is a command
;; that adds to an existing archive and NEW-CMD is a command that makes a new
;; archive (overwriting an old one if it exists). ADD-CMD and NEW-CMD are:
;; 1. Nil (meaning we cannot do this for this type of archive) (one of
;;    ADD-CMD and NEW-CMD must be non-nil).
;; 2. A symbol that must be a function e.g. dired-do-archive-op.
;; 3. A format string with two arguments, the source files concatenated into
;;    a space separated string and the target archive.
;; 4. A list of strings, the command and its flags, to which the target and
;;    the source-files are concatenated."
(setq dired-to-archive-copy-alist
      '(("\\.sh\\(ar\\|[0-9]\\)*$" nil "shar %s > %s")
	("\\.jar$" ("jar" "uvf") ("jar" "cvf"))
	("\\.tar$" ("tar" "-uf") ("tar" "-cf"))
	("\\.tgz$\\|\\.tar\\.g?[zZ]$" ("tar" "-uf %s" "|" "gzip > %s") ("tar" "-czvf"))
	("\\.ear$" ("zip" "-qr") ("zip" "-qr"))
;	("\\.rar$" ("rar" "a")   ("rar" "a"))
	("\\.war$" ("zip" "-qr") ("zip" "-qr"))
	("\\.zip$" ("zip" "-qr") ("zip" "-qr"))
	("\\.wmz$" ("zip" "-qr") ("zip" "-qr")) ;; for media player skins
	("\\.arc$" ("arc" "a") nil)
	("\\.zoo$" ("zoo" "aP") nil)
	))

;; use pkzip with manipulating zip files (t) from within dired (use zip
;; and unzip otherwise)
(setq archive-zip-use-pkzip nil)

;; add these file types to archive mode to allow viewing and changing
;; their contents
(add-to-list 'auto-mode-alist '("\\.[ejrw]ar$\\'" . archive-mode))

;; modify the dired-extract switches to use the directory
;; ~/temp/tryout as the default extract directory for zip files
(setq dired-extract-alist
      `(
	("\\.u\\(ue\\|aa\\)$" . dired-uud)
	("\\.jar$" . "jar -xvf %s")
	("\\.tar$" . ,(concat "tar -xf %s -C " MY_TRYOUT_DIR))
	("\\.tgz$\\|\\.tar\\.g?[zZ]$" . ,(concat "tar -xzf %s -C " MY_TRYOUT_DIR))
	("\\.arc$" . "arc x %s ")
	("\\.bz2$" . ,(concat "bunzip2 -q %s"))
	("\\.rar$" . ,(concat "unrar x %s " MY_TRYOUT_DIR "\\"))
	("\\.zip$" . ,(concat "unzip -qq -x %s -d " MY_TRYOUT_DIR))
	("\\.ear$" . ,(concat "unzip -qq -x %s -d " MY_TRYOUT_DIR))
	("\\.war$" . ,(concat "unzip -qq -x %s -d " MY_TRYOUT_DIR))
	("\\.zoo$" . "zoo x. %s ")
	("\\.lzh$" . "lha x %s ")
	("\\.g?[zZ]$" . "gzip -d %s")   ; There is only one file
	))

;;======================================================================
;; Mac Open/Execute from dired
;; Use the "j" key to launch the associated app.
(defun dired-custom-execute-file (&optional arg)
  (interactive "P")
  (mapcar #'(lambda (file)
              (if (eq system-type 'darwin)
                  (shell-command (concat "/usr/bin/open " (concat "\""(dired-get-filename)"\"")))
	      (w32-shell-execute nil (convert-standard-filename file))))
	  (dired-get-marked-files nil arg)))

(defun dired-custom-dired-mode-hook ()
  (define-key dired-mode-map "j" 'dired-custom-execute-file))
(add-hook 'dired-mode-hook 'dired-custom-dired-mode-hook)



;;======================================================================
;; change background color of dired buffers
;(custom-set-variables
; '(buffer-face-mode-face (quote (:background "#eee8d5"))))
;(add-hook 'dired-mode-hook 'buffer-face-mode)

;;======================================================================
;; hide the details from dired buffers. Show with (' key
;(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;======================================================================
;; wdired
;; allow in-place file name editing within dired mode
;; use the method wdired-change-to-wdired-mode to enable
(eval-after-load "dired" '(load-library "wdired"))

;;======================================================================
;; dired+
;(setq diredp-hide-details-initially-flag nil)
;(require 'dired+)


;;======================================================================
;; dired-x
;;; dired-x provides added functions to dired, including the ability
;;; to save a dired listing for later use as XXXXX.dired and the
;;; ability to run modes on a dired file listing
;;; I for info files, W for html file and so on...
(load "dired-x")

;; Set dired-x global variables here.  For example:
;; (setq dired-guess-shell-gnutar "gtar")
;; (setq dired-x-hands-off-my-keys nil)
(setq dired-x-hands-off-my-keys nil
      dired-bind-info t)

;;; associate files with a .dired extension with virtual dired mode.
;;; To get the file back into dired mode, hit 'g' to refresh the
;;; current file listing. This is good for multiple directory listings
;;; within projects!
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode) auto-mode-alist))


;;======================================================================
;; find-dired
;; set the find program to which dired-find point for executing find
;; commands and placing the output into a dired buffer.  Use the
;; command `find-name-dired' to create a list of find results in a
;; dired buffer
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;;======================================================================
;; find-lisp-dired
;; replaces the find program with a lisp equivalent, using emacs regex
;; engine. Because of this, the patterns will be different. Instead of
;; *.foo you would enter .foo$
;; example for all .el files in the emacs subdirectory
;; M-x find-lisp-find-dired  .el$
;;
;; search for files with names matching a wild card pattern and Dired the
;; output
(global-set-key [(control c) ?1] 'find-name-dired)

;; search for files with contents matching a wild card pattern and Dired the
;; output
(global-set-key [(control c) ?2] 'find-grep-dired)

;; run grep via find, with user-specified arguments
(global-set-key [(control c) ?3] 'grep-find)

;; ignore `.svn' and `CVS' directories
(setq grep-find-command
      (concat
       "find . \\( -path '*/.svn' -o -path '*/CVS' -o -path '\.git' \\) -prune -o -type f "
              "-print0 | xargs -0 -e grep -i -n -e "))

;;======================================================================
;; dired omit mode
;; filter out certain files and directories from within dired listing
;; toggle with C-x M-o
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol ".git" )    ;; git directories
              (seq bol "CVS" eol)  ;; CVS dirs
              )))

;; let's not get carried away with hiding files, however
(setq dired-omit-extensions nil)

;(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;;======================================================================
;; thumbs
;; An extension to dired to show thumbnails of images within a directory
;; M-x thumbs to generate
(setq thumbs-relief 0)
(setq thumbs-per-line 8)

(provide 'emacs-dired)

;;======================================================================
;; Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:

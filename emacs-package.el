  (require 'package)
  (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)

(when (eq 0 (length package-alist))
      (package-refresh-contents)
      (dolist
	  (pkg  '(clojure-mode
 		    clojure-test-mode
		    cider
		    company
		    company-cider
		    w3m
		    bm
		    bs-ext
		    exec-path-from-shell
		    helm
		    igrep
		    magit
		    org
		    s
		    w3m))
	(unless (package-installed-p pkg)
	  (package-install pkg))))

(provide 'emacs-package)

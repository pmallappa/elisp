;;==============================
;; associate .js files with js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; autocompletion
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;==============================
;; set up the simple-httpd
(require 'simple-httpd)
(setq httpd-root "~/public_html")
(httpd-start)

;;==============================
;; set up skewer mode, which provides browser interaction and a javascript
;; repl. Once a javascript file is loaded, execute M-x run-skewer to attach
;; it to a browser.
;; https://github.com/skeeto/skewer-mode
(skewer-setup)

(provide 'emacs-javascript)

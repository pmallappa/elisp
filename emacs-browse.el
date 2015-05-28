;;;_* -*-mode: emacs-lisp -*-

;;======================================================================
;; Load various web pages into the browser of choice
;;======================================================================


(require 'browse-url-dwim)
(browse-url-dwim-mode 1)

;(setq browse-url-generic-program IE_PRG)

(defun cmBrowse (browser &optional url)
  "Launch the browser specified with the optional page or home page if nil"
  (cond 
   ((string-equal system-type "darwin")
    (shell-command
     (concat "/usr/bin/open -a " browser " " (parse-url url))))
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "open" browser (parse-url url)))
   ))

;; set the maximum number of characters permitted in the suggested URL when
;; prompting (default is 40)
(setq browse-url-dwim-max-prompt-length 70)

(defun parse-url (url)
  (if (or (string= url "") (null url))
      (browse-url-dwim-get-url 1 "URL: " "http://www.google.com")
  url))

(defun fx (&optional url)
  "Launch the Firefox browser with an optional URL."
  (interactive)
  (cmBrowse FIREFOX_PRG url))

(defun ie (&optional url)
  "Launch the Internet Explorer browser with an optional URL."
  (interactive)
  (cmBrowse IE_PRG url))

(defun crm (&optional url)
  "Launch the Google Chrome browser with an optional URL."
  (interactive)
  (cmBrowse CHROME_PRG url))

(defun css ()
  "Load the Cascading Style Sheet specification into the default browser
Local or Remote (web-based) copies available"
  (interactive)
  (w3m-goto-url "http://www.htmlhelp.com/reference/css/index.html"))

(defun html ()
  "Load the HTML 4.0 specification into w3m
Local or Remote (web-based) copies available"
  (interactive)
    (w3m-goto-url "http://www.htmlhelp.com/reference/html40/"))

(defun gmail ()
  "Load the gmail page into Firefox"
  (interactive)
  (cmBrowse FIREFOX_PRG "https://mail.google.com"))


(provide 'emacs-browse)

;;;_* -*-mode: emacs-lisp -*-

;;======================================================================
;; Load various web pages into the browser of choice
;;======================================================================

(require 'browse-url-dwim)
(browse-url-dwim-mode 1)

(defun cmBrowse (browser &optional url)
  "Launch the browser specified with the optional page or home page if nil"
  (cond 
   ((string-equal system-type "darwin")
    (shell-command
     (concat "/usr/bin/open -a " browser " "
             (parse-url url))))
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "open" browser " "
             (parse-url url)))))

(defun parse-url (url)
  (if (or (string= url "") (null url))
      (browse-url-dwim-get-url 1 "URL: " "http://www.google.com")
  url))

(defun fx (&optional url)
  "Launch the Firefox browser with an optional URL."
  (interactive)
  (cmBrowse FIREFOX_PRG url))

(defun chrm (&optional url)
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


(provide 'emacs-browse)

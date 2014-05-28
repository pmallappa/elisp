;;;_*======================================================================
;;;_* Launch various tasks (apps with predefined switches) associated with Windows OS
;; the XTerm switches and geometry are defined in the .tcshrc
(defun console ()
  "Launch a shell in the normal console location using rxvt"
  (interactive)
  (cond 
   ((string-equal system-type "darwin")

  (if (eq system-type 'darwin)
      (shell-command 
       (concat "/usr/bin/open -a rxvt " 
               (getenv "CONSOLE_SWITCHES") " -T '" 
               (getenv "HOST") "' -e tcsh")))

  (w32-shell-execute
   "open"
   "rxvt"
   (concat (getenv "CONSOLE_SWITCHES") " -T '" (getenv "HOST") "' -e tcsh")))

(defun xterm ()
  "Launch a shell using the rxvt application."
  (interactive)
  (w32-shell-execute
   "open"
   "rxvt"
   (concat (getenv "XTERM_SWITCHES") " -T 'xterm' -e tcsh")))

(defun sshfwd (&optional targetIn)
  "Launch an ssh shell to [optional] target. (panix2.panix.com is
  default) using the rxvt application, with port forwarding.
  Forwarded ports are set in the env variable SSHFWD_PORTS in the
  ~/.tcshrc as are the default fonts and colors"
	(interactive "Mtarget (panix1.panix.com): ")
	(if (string= targetIn "")
			(setq target "panix1.panix.com")
		(setq target targetIn))
	(w32-shell-execute
   "open"
   "rxvt"
   (concat (getenv "XTERM_SWITCHES") " -T '" target "' -e ssh " target " " (getenv "SSHFWD_PORTS"))))

(defun panix1 ()
	"Launch an ssh shell to panix1.panix.com with port forwarding enabled"
	(interactive)
	(sshfwd "panix1.panix.com"))
(defun panix2 ()
	"Launch an ssh shell to panix1.panix.com with port forwarding enabled"
	(interactive)
	(sshfwd "panix2.panix.com"))
(defun panix3 ()
	"Launch an ssh shell to panix3.panix.com with port forwarding enabled"
	(interactive)
	(sshfwd "panix3.panix.com"))
(defun panix5 ()
	"Launch an ssh shell to panix5.panix.com with port forwarding enabled"
	(interactive)
	(sshfwd "panix5.panix.com"))

(defun sshcmd (&optional cmdIn)
  "Launch a given command at panix2.com using the rxvt application and SSH"
  (interactive "Mcmd (tcsh): ")
   (if (string= cmdIn "")
	   (setq cmd "")
	 (setq cmd cmdIn))
  (w32-shell-execute
   "open"
   "rxvt"
   (concat (getenv "XTERM_SWITCHES") "-T 'xterm panix.com' -e ssh -lcmcmahan -tt panix5.panix.com " cmd)))

(defun cmd ()
  "Launch the NT Command console"
  (interactive)
	(w32-shell-execute
	 "open"
	 "cmd"))

(defun dterm ()
  "Launch tcsh shell with the NT shell window"
  (interactive)
  (w32-shell-execute
   "open"
   "cmd"
   "/C tcsh"))

(defun explorer ()
  "Launch the windows explorer in the current directory"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/e, " (convert-standard-filename default-directory))))

;; Control panel applets
(defun control ()
  "Launch the Windows control panel"
  (interactive)
  (w32-shell-execute "open" "control.exe"))
(defun fonts ()
  "Launch the Windows font control panel applet"
  (interactive)
  (w32-shell-execute "open" "control.exe" "fonts"))
(defun display ()
  "Launch the Windows display control panel applet with the Appearance tab selected
Switches control these tab
 5 Themes
 0 Desktop
 1 Screen Saver
 2 Appearance
 3 Settings"
  (interactive)
  (w32-shell-execute "open" "control.exe" "desk.cpl, ,-2"))
(defun system ()
  "Launch the Windows system control panel applet"
  (interactive)
  (w32-shell-execute "open" "control.exe" "sysdm.cpl"))

;;;_*======================================================================
;;;_* Remote desktop shortcuts
(defvar rdp_directory "c:/remote_machines/"
  "Directory in which the XP Remote Desktop files are stored.
Also see the function `rd'")

; Shortcuts to get to remote machines
(defvar remote-servers
  "List of aliases and corresponding server names.")

(setq remote-servers
  '(("9nt38b1"  . "9nt38b1")
    ("b3wj6b1"  . "b3wj6b1")
    ("bdc1"     . "bdc-gaim001")
    ("bdc3"     . "bdc-gaim003")
    ("bdc4"     . "bdc-gaim004")
    ("bdc5"     . "bdc-gaim005")
    ("bdc8"     . "bdc-gaim008")
    ("bdc9"     . "bdc-gaim009")
    ("bdc10"    . "bdc-gaim010")
    ("bdc13"    . "bdc-gaim013")
    ("bdc14"    . "bdc-gaim014")
    ("dev"      . "mvl-gaimsd06")
    ("hy56d61"  . "hy56d61")
    ("intra792" . "bdc-intra792")
    ("intra799" . "bdc-intra799")
    ("intra101" . "lbn-intra101")
    ("mvl1"     . "mvl-gaimsd01")
    ("mvl3"     . "mvl-gaimsd03")
    ("mvl5"     . "mvl-gaimsd05")
    ("mvl6"     . "mvl-gaimsd06")
    ("mvl7"     . "mvl-gaimsd07")
    ("mvlcatalyst" . "mvl-catalyst001")
    ("mvlvader" . "mvl-vader")
    ("qa"       . "bdc-gaim010")
    ("m65"      . "9nt38b1")
    ("m60"      . "hy56d61")
    ("test"     . "bdc-gaim004")
    ("train"    . "mvl-gaimsd01")
    ("vader"    . "mvl-vader")))

(defun remote_desktop (target_machine &optional console_switch)
  "Launch the XP Remote Desktop app using the rdp file provided.
optional console_switch sets the remote desktop into console mode"
  (if (null console_switch)
      (w32-shell-execute "open" "c:/windows/system32/mstsc.exe" target_machine  1)
    (w32-shell-execute "open" "c:/windows/system32/mstsc.exe" (concat target_machine " /console") 1)))

(defun rcmd (alias)
  "Launch a cmd shell from a remote machine using the psexec command"
   (interactive
      (list
       (completing-read "Target machine: " remote-servers nil t)))
   (let ((target (or (cdr (assoc alias remote-servers))
                     (error "No such target: %s" alias))))
     (message  (concat "Opening terminal to " target))
     (w32-shell-execute "open" "cmd" (concat "/K psexec \\\\" target " cmd /K cd c:\\"))))

(defun rd (alias)
  "Launch the XP Remote Desktop app using the selected rdp file"
   (interactive
      (list
       (completing-read "Target machine: " remote-servers nil t)))
   (let ((target (or (cdr (assoc alias remote-servers))
                     (error "No such target: %s" alias))))
     (message  (concat rdp_directory (concat target ".rdp")))
     (remote_desktop (concat rdp_directory (concat target ".rdp")))))

(defun rdc (alias)
  "Launch the XP Remote Desktop app using the selected rdp file in console mode."
   (interactive
      (list
       (completing-read "Target machine: " remote-servers nil t)))
   (let ((target (or (cdr (assoc alias remote-servers))
                     (error "No such target: %s" alias))))
     (message  (concat rdp_directory (concat target ".rdp")))
     (remote_desktop (concat rdp_directory (concat target ".rdp")) 1)))

(defun rtelnet (alias)
  "Launch a cmd shell on from the remote machine"
   (interactive
      (list
       (completing-read "Target machine: " remote-servers nil t)))
   (let ((target (or (cdr (assoc alias remote-servers))
                     (error "No such target: %s" alias))))
     (message  (concat "opening " target))
     (rcmd target)))

;;;_*======================================================================
;;;_* Launch various applications
(defconst path_to_office "c:/Program Files/Microsoft Office/Office12/")

(defun access ()
  "Launch the MS Access database program"
  (interactive)
  (w32-shell-execute "open" (concat path_to_office "/msaccess.exe")))

(defun eclipse ()
  "Launch the Eclipse IDE"
  (interactive)
  ; must be in the eclipse directory to launch correctly
  (let ((eclipse-directory "c:/eclipse"))
  (w32-shell-execute
   "open"
   (concat eclipse-directory "/eclipse.exe"))))

(defun excel ()
  "Launch the MS Excel spreadsheet application"
  (interactive)
  (w32-shell-execute "open" (concat path_to_office "/excel.exe")))

(defun outlook ()
  "Launch the MS Outlook calander and email program"
  (interactive)
  (w32-shell-execute "open" (concat path_to_office "/outlook.exe") "/recycle"))

(defun powerpoint ()
  "Launch the MS PowerPoint presentation program"
  (interactive)
  (w32-shell-execute "open" (concat path_to_office "/powerpnt.exe")))

(defun project ()
  "Launch the MS Project application"
  (interactive)
  (w32-shell-execute "open" (concat path_to_office "/winproj.exe")))

(defun word ()
  "Launch the MS Word application"
  (interactive)
  (w32-shell-execute "open" (concat path_to_office "/winword.exe")))

(defun wordpad ()
  "Launch the Wordpad editor"
  (interactive)
  (w32-shell-execute
   "open"
   "C:/Program Files/Windows NT/Accessories/wordpad.exe"))

(defun hotsync ()
  "Launch the Palm hotsync program"
  (interactive)
  (w32-shell-execute "open" "c:/Program Files/palmOne/hotsync.exe"))

(defun palm ()
  "Launch the Palm desktop program"
  (interactive)
  (w32-shell-execute "open" "c:/Program Files/palmOne/palm.exe"))

(defun vern ()
  "Launch the Vern virtual desktop application"
  (interactive)
  (w32-shell-execute "open" (concat UTILS_DIR "/Vern/vern32.exe")))

(defun pskill (pid)
  "Kill the process with the specified pid"
  (interactive "sPID or Executable Name: ")
  (let ((default-directory TOP_LEVEL)))
  (shell-command (concat "pskill " pid)))

;;;_*======================================================================
;;;_* the following functions use the program nircmd
;;;_* found at http://www.nirsoft.net
(defun nircmd (cmd)
  (interactive "MCmd: " cmd)
  (w32-shell-execute "open"
                     (expand-file-name (concat UTILS_DIR "/NirCmd/nircmd.exe"))
                     cmd))

(defun cdeject ()
  "Eject the cd in drive d:"
  (interactive)
  (nircmd "cdrom open d:"))

(defun cdopen ()
  "Eject the cd in drive d:"
  (interactive)
  (cdeject))

(defun screensaver ()
  "Start the default screensaver"
  (interactive)
  (nircmd "screensaver"))

(defun lock ()
  "Lock the workstation"
  (interactive)
  (nircmd "lockws"))


;;;_*======================================================================
;;;_* Play with the volume control
(defun volset (percent &optional component)
  "Set the volume to specified percent.
Optional string 'component' refers to one of the following sound
components:
- aux
- cd
- headphones
- line
- master
- microphone
- phone
- synth
- wavein
- waveout
If a component is specified, only that component's volume is set,
otherwise, the master volume is set to the specified percentage."
  (interactive "nPercent: " percent)
  (let ((lvl (number-to-string (* 65535 (* .01 percent)))))
    (if (null component)
        (nircmd (concat "setsysvolume " lvl " master"))
    (nircmd (concat "setsysvolume " lvl " " component)))))

(defun volchange (percent &optional component)
  "Change the volume by arg percent up or down. Optional component
Optional 'component' refers to one of the possible sound components, and can be
master, waveout, synth, cd, microphone, phone, aux, line, headphones, wavein.

If a component is specified, only that component's volume is set.
The master volume is unchanged"
  (interactive "p")
  (let ((lvl (number-to-string (* 65535 (* .01 percent)))))
    (if (null component)
        (nircmd (concat "changesysvolume " lvl " master"))
    (nircmd (concat "changesysvolume " lvl " " component)))))

(defun volup (&optional component)
  "Raise the system volume 2%
Optional 'component' refers to one of the possible sound components, and can be
master, waveout, synth, cd, microphone, phone, aux, line, headphones, wavein

If a component is specified, only that component's volume is set.
The master volume is unchanged"
  (interactive)
  (if (null component)
      (volchange 2 "master")
    (volchange 2 component)))

(defun voldown (&optional component)
  "Raise the system volume 2%
Optional 'component' refers to one of the possible sound components, and can be
master, waveout, synth, cd, microphone, phone, aux, line, headphones, wavein

If a component is specified, only that component's volume is set.
The master volume is unchanged"
  (interactive)
  (if (null component)
      (volchange -2 "master")
    (volchange -2 component)))

(defun wavset (percent)
  "Set the wave output volume to the specified percent"
  (interactive "nPercent: " percent)
  (volset percent "waveout"))

(defun wavup ()
  "Raise the waveout volume"
  (interactive)
  (volup "waveout"))

(defun wavdown ()
  "Lower the waveout volume"
  (interactive)
  (voldown "waveout"))

(defun mute ()
  "UnMute the system volume"
  (interactive)
  (nircmd "mutesysvolume 1"))

(defun unmute ()
  "Mute the system volume"
  (interactive)
  (nircmd "mutesysvolume 0"))

(defun mutetoggle ()
  "Toggle the volume between mute and unmute"
  (interactive)
  (nircmd "mutesysvolume 2"))

(defun volheadphones ()
  "Reset the volume to a comfortable level for headphones"
  (interactive)
  (volset 10 "master")
  (volset 10 "waveout"))

(defun volspeakers ()
  "Reset the volume to a comfortable level for speakers"
  (interactive)
  (volset 60 "waveout")
  (volset 60 "master"))

(defun volapp()
  "Launch the windows volume application"
  (interactive)
  (w32-shell-execute "open" "C:/WINDOWS/system32/sndvol32.exe"))

(defun vol ()
  "Create a buffer to adjust the volume interactively. The
following keybindings are in effect within this buffer
  C-p or u raise volume
  M-p raise waveout volume
  C-n or d lower volume
  M-n lower waveout volume
  l launch the volume control app
  m mute/unmute
  h set the master and waveout to 10% for headphones
  s set the master and waveout to 20% for speakers
  q quit"

  ;Look at the function `read-from-minibuffer' for suggestions on how to
  ;use the minibuffer instead of a dedicated buffer for this function"
  (interactive)
  (switch-to-buffer "*volume*")
  (erase-buffer)
  (insert "Volume buffer,
- C-p raise master volume
- C-n lower master volume
- M-p raise waveout volume
- M-n lower waveout volume
- v launch the volume control app
- m toggle mute/unmute
- h set the master and waveout volume for headphones
- s set the master and waveout volume for speakers
- q quit\n\n-> ")
  (local-set-key "\C-n" (quote voldown))
  (local-set-key "\M-n" (quote wavdown))
  (local-set-key "\C-p" (quote volup))
  (local-set-key "\M-p" (quote wavup))
  (local-set-key "v"    (quote volapp))
  (local-set-key "m"    (quote mutetoggle))
  (local-set-key "h"    (quote volheadphones))
  (local-set-key "s"    (quote volspeakers))
  (local-set-key "q"    (quote bury-buffer)))


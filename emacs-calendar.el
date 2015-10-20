;;======================================================================
;; calendar mode stuff
;;======================================================================
(require 'calendar)

(setq calendar-view-diary-initially-flag nil)
;(setq diary-entry-marker 'font-lock-reference-face)

(setq calendar-time-zone -300)
(setq calendar-standard-time-zone-name "EST")
(setq calendar-daylight-time-zone-name "EDT")

;; To get lat lon, center the point in google maps and enter the
;; following URL
;; javascript:void(prompt('',gApplication.getMap().getCenter()));

;Degrees Minutes Seconds:
;Lat 391823N   Lon 0841648W
;Lat 39.30639  Lon -84.28
(setq calendar-latitude [39 18 north])
(setq calendar-longitude [84 16 west])

(setq calendar-location-name "Loveland, OH")
;(setq calendar-time-display-form '(24-hours ":" minutes))

;;; mark today as underlined
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;;------------------------------
;; Diary
;;------------------------------
(setq diary-file (concat HOME_DIR "/org/diary"))

;;; number of diary entries to display, from Sunday to Saturday
(setq number-of-diary-entries [2 1 1 1 1 4 2])

;; comments in diary entries
;; useful for setting custom notification times
(setq diary-comment-start "##")

;;------------------------------
;; Holidays
;;------------------------------
;(setq holiday-general-holidays t)

(setq holiday-bahai-holidays 'nil)
(setq holiday-christian-holidays 'nil)
(setq holiday-hebrew-holidays 'nil)
(setq holiday-islamic-holidays 'nil)
(setq holiday-local-holidays 'nil)
(setq holiday-oriental-holidays 'nil)
(setq holiday-other-holidays 'nil)
(setq holiday-solar-holidays 'nil)

(setq diary-show-holidays-flag t)
(setq calendar-mark-holidays-flag t)
(setq calendar-view-holidays-initially t)
(setq calendar-mark-diary-entries-flag t)

;(setq other-holidays
;      '((holiday-fixed 12 25 "Christmas")))


;;------------------------------
;; Appointments and notifications
;;------------------------------
;(setq appt-message-warning-time 10)
;(setq appt-audible t)
;(setq appt-visible t)
;(setq appt-display-mode-line nil)
;(setq appt-display-duration 10)
;(setq appt-display-interval 2)
;
;; set to window to allow growl to notify
;(setq appt-display-format 'window)
;
;; set to 1 to activate the appointment function
;(appt-activate 1)


;;------------------------------
;; Cal-desk Calendar
;;------------------------------
;(load-library "cal-desk-calendar")
;(add-hook 'diary-display-hook 'sort-diary-entries)
;(add-hook 'diary-display-hook 'fancy-schedule-display-desk-calendar t)
;
;(setq diary-schedule-interval-time 60)
;(setq diary-default-schedule-start-time 800)
;(setq diary-default-schedule-stop-time 1700)

;;------------------------------
;; Fancy-diary
;;------------------------------
;;; use these if not using the cal-desk-calendar package
;(add-hook 'diary-display-hook 'fancy-diary-display)
;(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
;
;(add-hook 'diary-hook 'appt-make-list)
;(setq diary-duplicate-time-display 'nil)
;(setq diary-schedule-odd-times-get-separate-entry t)

;;------------------------------
;; redefine some keys for calendar movement. The Meta-X combination
;; to move by the month goes in line with the standard text method of
;; using the Meta key to go a larger ingrement (paragraph)
(define-key calendar-mode-map "\M-n" 'calendar-forward-month) ; was ESC-}
(define-key calendar-mode-map "\M-p" 'calendar-backward-month) ; was ESC-{
(define-key calendar-mode-map "\C-\M-n" 'calendar-forward-year) ; was C-x [
(define-key calendar-mode-map "\C-\M-p" 'calendar-backward-year) ; was C-x [

;;======================================================================
;; change to fixed-pitch font for this buffer
;(add-hook 'calendar-mode-hook
;          (lambda()
;            (buffer-face-mode-invoke 'fixed-pitch t)))

(provide 'emacs-calendar)

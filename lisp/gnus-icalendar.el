;;; gnus-icalendar.el --- reply to iCalendar meeting requests

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>
;; Keywords: mail, icalendar, org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'icalendar)
(require 'eieio)
(require 'mm-decode)
(require 'gnus-sum)

;;;
;;; ical-event
;;;

(defclass gnus-icalendar-event ()
  ((organizer :initarg :organizer
              :accessor gnus-icalendar-event:organizer
              :initform ""
              :type (or null string))
   (summary :initarg :summary
            :accessor gnus-icalendar-event:summary
            :initform ""
            :type (or null string))
   (description :initarg :description
                :accessor gnus-icalendar-event:description
                :initform ""
                :type (or null string))
   (location :initarg :location
             :accessor gnus-icalendar-event:location
             :initform ""
             :type (or null string))
   (start :initarg :start
          :accessor gnus-icalendar-event:start
          :initform ""
          :type (or null string))
   (end :initarg :end
        :accessor gnus-icalendar-event:end
        :initform ""
        :type (or null string))
   (recur :initarg :recur
          :accessor gnus-icalendar-event:recur
          :initform ""
          :type (or null string))
   (uid :initarg :uid
        :accessor gnus-icalendar-event:uid
        :type string)
   (method :initarg :method
           :accessor gnus-icalendar-event:method
           :initform "PUBLISH"
           :type (or null string))
   (rsvp :initarg :rsvp
         :accessor gnus-icalendar-event:rsvp
         :initform nil
         :type (or null boolean)))
  "generic iCalendar Event class")

(defclass gnus-icalendar-event-request (gnus-icalendar-event)
  nil
  "iCalendar class for REQUEST events")

(defclass gnus-icalendar-event-cancel (gnus-icalendar-event)
  nil
  "iCalendar class for CANCEL events")

(defclass gnus-icalendar-event-reply (gnus-icalendar-event)
  nil
  "iCalendar class for REPLY events")

(defmethod gnus-icalendar-event:recurring-p ((event gnus-icalendar-event))
  "Return t if EVENT is recurring."
  (not (null (gnus-icalendar-event:recur event))))

(defmethod gnus-icalendar-event:recurring-freq ((event gnus-icalendar-event))
  "Return recurring frequency of EVENT."
  (let ((rrule (gnus-icalendar-event:recur event)))
    (string-match "FREQ=\\([[:alpha:]]+\\)" rrule)
    (match-string 1 rrule)))

(defmethod gnus-icalendar-event:recurring-interval ((event gnus-icalendar-event))
  "Return recurring interval of EVENT."
  (let ((rrule (gnus-icalendar-event:recur event))
        (default-interval 1))

    (string-match "INTERVAL=\\([[:digit:]]+\\)" rrule)
    (or (match-string 1 rrule)
        default-interval)))

(defmethod gnus-icalendar-event:start-time ((event gnus-icalendar-event))
  "Return time value of the EVENT start date."
  (date-to-time (gnus-icalendar-event:start event)))

(defmethod gnus-icalendar-event:end-time ((event gnus-icalendar-event))
  "Return time value of the EVENT end date."
  (date-to-time (gnus-icalendar-event:end event)))


(defun gnus-icalendar-event--decode-datefield (ical field zone-map &optional date-style)
  (let* ((calendar-date-style (or date-style 'european))
         (date (icalendar--get-event-property ical field))
         (date-zone (icalendar--find-time-zone
                     (icalendar--get-event-property-attributes
                      ical field)
                     zone-map))
         (date-decoded (icalendar--decode-isodatetime date nil date-zone)))

    (concat (icalendar--datetime-to-iso-date date-decoded "-")
            " "
            (icalendar--datetime-to-colontime date-decoded))))

(defun gnus-icalendar-event--find-attendee (ical name-or-email)
  (let* ((event (car (icalendar--all-events ical)))
         (event-props (caddr event)))
    (cl-labels ((attendee-name (att)
                               (plist-get (cadr att) 'CN))
                (attendee-email (att)
                                (replace-regexp-in-string "^.*MAILTO:" "" (caddr att)))
                (attendee-prop-matches (prop)
                                       (and (eq (car prop) 'ATTENDEE)
                                            (or (member (attendee-name prop) name-or-email)
                                                (let ((att-email (attendee-email prop)))
                                                  (cl-find-if (lambda (email)
                                                                (string-match email att-email))
                                                              name-or-email))))))

      (cl-find-if #'attendee-prop-matches event-props))))


(defun icalendar->gnus-icalendar-event (ical &optional attendee-name-or-email)
  (let* ((event (car (icalendar--all-events ical)))
         (zone-map (icalendar--convert-all-timezones ical))
         (organizer (replace-regexp-in-string
                     "^.*MAILTO:" ""
                     (or (icalendar--get-event-property event 'ORGANIZER) "")))
         (prop-map '((summary . SUMMARY)
                     (description . DESCRIPTION)
                     (location . LOCATION)
                     (recur . RRULE)
                     (uid . UID)))
         (method (third (assoc 'METHOD (third (car (nreverse ical))))))
         (attendee (when attendee-name-or-email
                     (gnus-icalendar-event--find-attendee ical attendee-name-or-email)))
         (args (list :method method
                     :organizer organizer
                     :start (gnus-icalendar-event--decode-datefield event 'DTSTART zone-map)
                     :end (gnus-icalendar-event--decode-datefield event 'DTEND zone-map)
                     :rsvp (string= (plist-get (cadr attendee) 'RSVP)
                                    "TRUE")))
         (event-class (pcase method
                        ("REQUEST" 'gnus-icalendar-event-request)
                        ("CANCEL" 'gnus-icalendar-event-cancel)
                        ("REPLY" 'gnus-icalendar-event-reply)
                        (_ 'gnus-icalendar-event))))

    (cl-labels ((map-property (prop)
                              (let ((value (icalendar--get-event-property event prop)))
                                (when value
                                  ;; ugly, but cannot get
                                  ;;replace-regexp-in-string work with "\\" as
                                  ;;REP, plus we should also handle "\\;"
                                  (replace-regexp-in-string
                                   "\\\\," ","
                                   (replace-regexp-in-string
                                    "\\\\n" "\n" (substring-no-properties value))))))
                (accumulate-args (mapping)
                                 (destructuring-bind (slot . ical-property) mapping
                                   (setq args (append (list
                                                       (intern (concat ":" (symbol-name slot)))
                                                       (map-property ical-property))
                                                      args)))))

      (mapc #'accumulate-args prop-map)
      (apply 'make-instance event-class args))))

(defun gnus-icalendar-event-from-buffer (buf &optional attendee-name-or-email)
  (let ((ical (with-current-buffer (icalendar--get-unfolded-buffer (get-buffer buf))
                (goto-char (point-min))
                (icalendar--read-element nil nil))))

    (when ical
      (icalendar->gnus-icalendar-event ical attendee-name-or-email))))

;;;
;;; gnus-icalendar-event-reply
;;;

(defun gnus-icalendar-event--build-reply-event-body (event status identity)
  (let ((summary-status (capitalize (symbol-name status)))
        (attendee-status (upcase (symbol-name status)))
        reply-event-lines)
    (cl-labels ((update-summary (line)
                                (if (string-match "^[^:]+:" line)
                                    (replace-match (format "\\&%s: " summary-status) t nil line)
                                  line))
                (update-dtstamp ()
                                (format-time-string "DTSTAMP:%Y%m%dT%H%M%SZ" nil t))
                (attendee-matches-identity (line)
                                           (cl-find-if (lambda (name) (string-match-p name line))
                                                       identity))
                (update-attendee-status (line)
                                        (when (and (attendee-matches-identity line)
                                                   (string-match "\\(PARTSTAT=\\)[^;]+" line))
                                          (replace-match (format "\\1%s" attendee-status) t nil line)))
                (process-event-line (line)
                                    (when (string-match "^\\([^;:]+\\)" line)
                                      (let* ((key (match-string 0 line))
                                             ;; NOTE: not all of the below fields are mandatory,
                                             ;; but they are present in MS Exchange replies. Need
                                             ;; to test with minimalistic setup, too.
                                             (new-line (pcase key
                                                         ("ATTENDEE" (update-attendee-status line))
                                                         ("SUMMARY" (update-summary line))
                                                         ("DTSTAMP" (update-dtstamp))
                                                         ((or "ORGANIZER" "DTSTART" "DTEND"
                                                              "LOCATION" "DURATION" "SEQUENCE"
                                                              "RECURRENCE-ID" "UID") line)
                                                         (_ nil))))
                                        (when new-line
                                          (push new-line reply-event-lines))))))

      (mapc #'process-event-line (split-string event "\n"))

      (unless (cl-find-if (lambda (x) (string-match "^ATTENDEE" x))
                          reply-event-lines)
        (error "Could not find an event attendee matching given identity"))

      (mapconcat #'identity `("BEGIN:VEVENT"
                              ,@(nreverse reply-event-lines)
                              "END:VEVENT")
                 "\n"))))

(defun gnus-icalendar-event-reply-from-buffer (buf status identity)
  "Build a calendar event reply for request contained in BUF.
The reply will have STATUS (`accepted', `tentative' or  `declined').
The reply will be composed for attendees matching any entry
on the IDENTITY list."
  (cl-flet ((extract-block (blockname)
                           (save-excursion
                             (let ((block-start-re (format "^BEGIN:%s" blockname))
                                   (block-end-re (format "^END:%s" blockname))
                                   start)
                               (when (re-search-forward block-start-re nil t)
                                 (setq start (line-beginning-position))
                                 (re-search-forward block-end-re)
                                 (buffer-substring-no-properties start (line-end-position)))))))

    (let (zone event)
      (with-current-buffer (icalendar--get-unfolded-buffer (get-buffer buf))
        (goto-char (point-min))
        (setq zone (extract-block "VTIMEZONE")
              event (extract-block "VEVENT")))

      (when event
        (let ((contents (list "BEGIN:VCALENDAR"
                              "METHOD:REPLY"
                              "PRODID:Gnus"
                              "VERSION:2.0"
                              zone
                              (gnus-icalendar-event--build-reply-event-body event status identity)
                              "END:VCALENDAR")))

          (mapconcat #'identity (delq nil contents) "\n"))))))

;;;
;;; gnus-calendar-org
;;;
;;; TODO: this is an optional feature, and it's only available with org-mode
;;; 7+, so will need to properly handle emacsen with no/outdated org-mode

(require 'org)
(require 'org-capture)

(defgroup gnus-calendar-org nil
  "Settings for Calendar Event gnus/org integration."
  :group 'gnus-calendar
  :prefix "gnus-calendar-org-")

(defcustom gnus-calendar-org-capture-file nil
  "Target Org file for storing captured calendar events."
  :type 'file
  :group 'gnus-calendar-org)

(defcustom gnus-calendar-org-capture-headline nil
  "Target outline in `gnus-calendar-org-capture-file' for storing captured events."
  :type '(repeat string)
  :group 'gnus-calendar-org)

(defcustom gnus-calendar-org-template-name "used by gnus-calendar-org"
  "Org-mode template name."
  :type '(string)
  :group 'gnus-calendar-org)

(defcustom gnus-calendar-org-template-key "#"
  "Org-mode template hotkey."
  :type '(string)
  :group 'gnus-calendar-org)

(defvar gnus-calendar-org-enabled-p nil)


(defmethod gnus-icalendar-event:org-repeat ((event gnus-icalendar-event))
  "Return `org-mode' timestamp repeater string for recurring EVENT.
Return nil for non-recurring EVENT."
  (when (gnus-icalendar-event:recurring-p event)
    (let* ((freq-map '(("HOURLY" . "h")
                       ("DAILY" . "d")
                       ("WEEKLY" . "w")
                       ("MONTHLY" . "m")
                       ("YEARLY" . "y")))
           (org-freq (cdr (assoc (gnus-icalendar-event:recurring-freq event) freq-map))))

      (when org-freq
        (format "+%s%s" (gnus-icalendar-event:recurring-interval event) org-freq)))))

(defmethod gnus-icalendar-event:org-timestamp ((event gnus-icalendar-event))
  "Build `org-mode' timestamp from EVENT start/end dates and recurrence info."
  (let* ((start (gnus-icalendar-event:start-time event))
         (end (gnus-icalendar-event:end-time event))
         (start-date (format-time-string "%Y-%m-%d %a" start t))
         (start-time (format-time-string "%H:%M" start t))
         (end-date (format-time-string "%Y-%m-%d %a" end t))
         (end-time (format-time-string "%H:%M" end t))
         (org-repeat (gnus-icalendar-event:org-repeat event))
         (repeat (if org-repeat (concat " " org-repeat) "")))

    (if (equal start-date end-date)
        (format "<%s %s-%s%s>" start-date start-time end-time repeat)
      (format "<%s %s>--<%s %s>" start-date start-time end-date end-time))))

;; TODO: make the template customizable
(defmethod gnus-icalendar-event->org-entry ((event gnus-icalendar-event) reply-status)
  "Return string with new `org-mode' entry describing EVENT."
  (with-temp-buffer
    (org-mode)
    (with-slots (organizer summary description location
                           recur uid) event
      (let* ((reply (if reply-status (capitalize (symbol-name reply-status))
                      "Not replied yet"))
             (props `(("ICAL_EVENT" . "t")
                      ("ID" . ,uid)
                      ("DT" . ,(gnus-icalendar-event:org-timestamp event))
                      ("ORGANIZER" . ,(gnus-icalendar-event:organizer event))
                      ("LOCATION" . ,(gnus-icalendar-event:location event))
                      ("RRULE" . ,(gnus-icalendar-event:recur event))
                      ("REPLY" . ,reply))))

        (insert (format "* %s (%s)\n\n" summary location))
        (mapc (lambda (prop)
                (org-entry-put (point) (car prop) (cdr prop)))
              props))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert description)
        (indent-region (point-min) (point-max) 2)
        (fill-region (point-min) (point-max)))

      (buffer-string))))

(defun gnus-calendar--deactivate-org-timestamp (ts)
  (replace-regexp-in-string "[<>]"
                            (lambda (m) (pcase m ("<" "[") (">" "]")))
                            ts))

(defun gnus-calendar--show-org-event (event org-file)
  (let (event-pos)
    (with-current-buffer (find-file-noselect org-file)
      (setq event-pos (org-find-entry-with-id (gnus-icalendar-event:uid event))))
    (when event-pos
      (switch-to-buffer (find-file org-file))
      (goto-char event-pos)
      (org-show-entry))))

(defun gnus-calendar--update-org-event (event org-file reply-status)
  (with-current-buffer (find-file-noselect org-file)
    (with-slots (uid summary description organizer location recur) event
      (let ((event-pos (org-find-entry-with-id uid)))
        (when event-pos
          (goto-char event-pos)

          ;; update the headline, keep todo, priority and tags, if any
          (save-excursion
            (let* ((priority (org-entry-get (point) "PRIORITY"))
                   (headline (delq nil (list
                                        (org-entry-get (point) "TODO")
                                        (when priority (format "[#%s]" priority))
                                        (format "%s (%s)" summary location)
                                        (org-entry-get (point) "TAGS")))))

              (re-search-forward "^\\*+ " (line-end-position))
              (delete-region (point) (line-end-position))
              (insert (mapconcat #'identity headline " "))))

          ;; update props and description
          (let ((entry-end (org-entry-end-position))
                (entry-outline-level (org-outline-level)))

            (save-restriction
              (org-narrow-to-element)
              (forward-line)
              (re-search-forward "^ *[^: ]" entry-end)
              (delete-region (point) entry-end))

            (save-restriction
              (narrow-to-region (point) (point))
              (insert description "\n")
              (indent-region (point-min) (point-max) (1+ entry-outline-level))
              (fill-region (point-min) (point-max)))
            (org-entry-put event-pos "DT" (gnus-icalendar-event:org-timestamp event))
            (org-entry-put event-pos "ORGANIZER" organizer)
            (org-entry-put event-pos "LOCATION" location)
            (org-entry-put event-pos "RRULE" recur)
            (when reply-status (org-entry-put event-pos "REPLY"
                                              (capitalize (symbol-name reply-status))))
            (save-buffer)))))))

(defun gnus-calendar--cancel-org-event (event org-file)
  (with-current-buffer (find-file-noselect org-file)
    (let ((event-pos (org-find-entry-with-id (gnus-icalendar-event:uid event))))
      (when event-pos
        (let ((ts (org-entry-get event-pos "DT")))
          (when ts
            (org-entry-put event-pos "DT" (gnus-calendar--deactivate-org-timestamp ts))
            (save-buffer)))))))

(defun gnus-calendar--get-org-event-reply-status (event org-file)
  (let ((id (gnus-icalendar-event:uid event)))
    (when (gnus-calendar-org-event-exists-p id org-file)
      (save-excursion
        (with-current-buffer (find-file-noselect org-file)
          (let ((event-pos (org-find-entry-with-id id)))
            (org-entry-get event-pos "REPLY")))))))

(defun gnus-calendar-org-event-exists-p (id org-file)
  "Return t when given event ID exists in ORG-FILE."
  (save-excursion
    (with-current-buffer (find-file-noselect org-file)
      (let ((event-pos (org-find-entry-with-id id)))
        (when event-pos
          (string= (cdr (assoc "ICAL_EVENT" (org-entry-properties event-pos)))
                   "t"))))))


(defun gnus-calendar-insinuate-org-templates ()
  (unless (cl-find-if (lambda (x) (string= (second x) gnus-calendar-org-template-name))
                      org-capture-templates)
    (setq org-capture-templates
          (append `((,gnus-calendar-org-template-key
                     ,gnus-calendar-org-template-name
                     entry
                     (file+olp ,gnus-calendar-org-capture-file ,@gnus-calendar-org-capture-headline)
                     "%i"
                     :immediate-finish t))
                  org-capture-templates))

    ;; hide the template from interactive template selection list
    ;; (org-capture)
    ;; NOTE: doesn't work when capturing from string
    ;; (when (boundp 'org-capture-templates-contexts)
    ;;   (push `(,gnus-calendar-org-template-key "" ((in-mode . "gnus-article-mode")))
    ;;         org-capture-templates-contexts))
    ))

(defun gnus-calendar:org-event-save (event reply-status)
  (with-temp-buffer
    (org-capture-string (gnus-icalendar-event->org-entry event reply-status)
                        gnus-calendar-org-template-key)))

(defun gnus-calendar:org-event-update (event reply-status)
  (gnus-calendar--update-org-event event gnus-calendar-org-capture-file reply-status))

(defun gnus-calendar:org-event-cancel (event)
  (gnus-calendar--cancel-org-event event gnus-calendar-org-capture-file))

(defun gnus-calendar:org-entry-exists-p (event)
  (gnus-calendar-org-event-exists-p (gnus-icalendar-event:uid event)
                                    gnus-calendar-org-capture-file))

(defun gnus-calendar-show-org-entry (event)
  (gnus-calendar--show-org-event event gnus-calendar-org-capture-file))

(defun gnus-calendar-show-org-agenda (event)
  (let* ((time-delta (time-subtract (gnus-icalendar-event:end-time event)
                                    (gnus-icalendar-event:start-time event)))
         (duration-days (1+ (/ (+ (* (first time-delta) (expt 2 16))
                                  (second time-delta))
                               86400))))

    (org-agenda-list nil start duration-days)))

(defun gnus-calendar:org-event-reply-status (event)
  (gnus-calendar--get-org-event-reply-status event gnus-calendar-org-capture-file))

(defmethod cal-event:sync-to-org ((event gnus-icalendar-event-request) reply-status)
  (if (gnus-calendar-org-event-exists-p (gnus-icalendar-event:uid event) gnus-calendar-org-capture-file)
      (gnus-calendar:org-event-update event reply-status)
    (gnus-calendar:org-event-save event reply-status)))

(defmethod cal-event:sync-to-org ((event gnus-icalendar-event-cancel))
  (when (gnus-calendar-org-event-exists-p
         (gnus-icalendar-event:uid event) gnus-calendar-org-capture-file)
    (gnus-calendar:org-event-cancel event)))

(defun gnus-calendar-org-setup ()
  (if (and gnus-calendar-org-capture-file gnus-calendar-org-capture-headline)
      (progn
        (gnus-calendar-insinuate-org-templates)
        (setq gnus-calendar-org-enabled-p t))
    (message "Cannot enable Calendar->Org: missing capture file, headline")))

;;;
;;; gnus-calendar
;;;

(defgroup gnus-calendar nil
  "Settings for inline display of iCalendar events."
  :group 'gnus-article
  :prefix "gnus-calendar-")

(defcustom gnus-calendar-reply-bufname "*CAL*"
  "Buffer used for building iCalendar reply."
  :type '(string)
  :group 'gnus-calendar)

(make-variable-buffer-local
 (defvar gnus-calendar-reply-status nil))

(make-variable-buffer-local
 (defvar gnus-calendar-event nil))

(make-variable-buffer-local
 (defvar gnus-calendar-handle nil))

(defvar gnus-calendar-identities
  (cl-mapcan (lambda (x) (if (listp x) x (list x)))
             (list user-full-name (regexp-quote user-mail-address)
                   ; NOTE: this one can be a list
                   gnus-ignored-from-addresses)))

;; TODO: make the template customizable
(defmethod gnus-icalendar-event->gnus-calendar ((event gnus-icalendar-event) &optional reply-status)
  "Format an overview of EVENT details."
  (with-slots (organizer summary description location recur uid method rsvp) event
    (let ((headers `(("Summary" ,summary)
                     ("Location" ,location)
                     ("Time" ,(gnus-icalendar-event:org-timestamp event))
                     ("Organizer" ,organizer)
                     ("Method" ,method))))

      (when (and (not (gnus-icalendar-event-reply-p event)) rsvp)
        (setq headers (append headers
                              `(("Status" ,(or reply-status "Not replied yet"))))))

      (concat
       (apply #'concat (mapcar (lambda (x)
                                 (format "%-12s%s\n"
                                         (propertize (concat (car x) ":") 'face 'bold)
                                         (cadr x)))
                               headers))
       "\n"
       description))))

(defmacro with-decoded-handle (handle &rest body)
  "Execute BODY in buffer containing the decoded contents of HANDLE."
  (let ((charset (make-symbol "charset")))
    `(let ((,charset (cdr (assoc 'charset (mm-handle-type ,handle)))))
       (with-temp-buffer
         (mm-insert-part ,handle)
         (when (string= ,charset "utf-8")
           (mm-decode-coding-region (point-min) (point-max) 'utf-8))

         ,@body))))


(defun gnus-icalendar-event-from-handle (handle &optional attendee-name-or-email)
  (with-decoded-handle handle
                       (gnus-icalendar-event-from-buffer (current-buffer) attendee-name-or-email)))

(defun gnus-calendar-insert-button (text callback data)
  ;; FIXME: the gnus-mime-button-map keymap does not make sense for this kind
  ;; of button.
  (let ((start (point)))
    (gnus-add-text-properties
     start
     (progn
       (insert "[ " text " ]")
       (point))
     `(gnus-callback
       ,callback
       keymap ,gnus-mime-button-map
       face ,gnus-article-button-face
       gnus-data ,data))
    (widget-convert-button 'link start (point)
                           :action 'gnus-widget-press-button
                           :button-keymap gnus-widget-button-keymap)))

(defun gnus-calendar-send-buffer-by-mail (buffer-name subject)
  (let ((message-signature nil))
    (with-current-buffer gnus-summary-buffer
      (gnus-summary-reply)
      (message-goto-body)
      (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=UTF-8" nil "inline")
      (message-goto-subject)
      (delete-region (line-beginning-position) (line-end-position))
      (insert "Subject: " subject)
      (message-send-and-exit))))

(defun gnus-calendar-reply (data)
  (let* ((handle (first data))
         (status (second data))
         (event (third data))
         (reply (with-decoded-handle handle
                                     (gnus-icalendar-event-reply-from-buffer (current-buffer)
                                                                   status gnus-calendar-identities))))

    (when reply
      (cl-flet ((fold-icalendar-buffer ()
                                       (goto-char (point-min))
                                       (while (re-search-forward "^\\(.\\{72\\}\\)\\(.+\\)$" nil t)
                                         (replace-match "\\1\n \\2")
                                         (goto-char (line-beginning-position)))))
        (let ((subject (concat (capitalize (symbol-name status))
                               ": " (gnus-icalendar-event:summary event))))

          (with-current-buffer (get-buffer-create gnus-calendar-reply-bufname)
            (delete-region (point-min) (point-max))
            (insert reply)
            (fold-icalendar-buffer)
            (gnus-calendar-send-buffer-by-mail (buffer-name) subject))

          ;; Back in article buffer
          (setq-local gnus-calendar-reply-status status)
          (when gnus-calendar-org-enabled-p
            (gnus-calendar:org-event-update event status)
            ;; refresh article buffer to update the reply status
            (with-current-buffer gnus-summary-buffer
              (gnus-summary-show-article))))))))

(defun gnus-calendar-sync-event-to-org (event)
  (cal-event:sync-to-org event gnus-calendar-reply-status))

(defun gnus-calendar-mm-inline (handle)
  (let ((event (gnus-icalendar-event-from-handle handle gnus-calendar-identities))
        (reply-status "Not replied yet")
        reply-buttons
        org-buttons)

    (setq gnus-calendar-reply-status nil)

    (when event
      (when (and (not (gnus-icalendar-event-reply-p event))
                 (gnus-icalendar-event:rsvp event))
        (when gnus-calendar-org-enabled-p
          (setq reply-status (or (gnus-calendar:org-event-reply-status event)
                                 reply-status)))

        (setq reply-buttons
              `(("Accept" gnus-calendar-reply (,handle accepted ,event))
                ("Tentative" gnus-calendar-reply (,handle tentative ,event))
                ("Decline" gnus-calendar-reply (,handle declined ,event)))))

      (when gnus-calendar-org-enabled-p
        (let* ((org-entry-exists-p (gnus-calendar:org-entry-exists-p event))
               (export-button-text (if org-entry-exists-p "Update Org Entry" "Export to Org")))

          (setq org-buttons (append org-buttons
                                    `(("Show Agenda" gnus-calendar-show-org-agenda ,event))))

          (when (gnus-icalendar-event-request-p event)
            (setq org-buttons (append org-buttons
                                      `((,export-button-text gnus-calendar-sync-event-to-org ,event)))))
          (when org-entry-exists-p
            (setq org-buttons (append org-buttons
                                      `(("Show Org Entry" gnus-calendar-show-org-entry ,event)))))))

      (cl-flet ((insert-button-group (buttons)
                                     (when buttons
                                       (mapc (lambda (x)
                                               (apply 'gnus-calendar-insert-button x)
                                               (insert "    "))
                                             buttons)
                                       (insert "\n\n"))))

        (insert-button-group reply-buttons)
        (insert-button-group org-buttons))

      (setq gnus-calendar-event event
            gnus-calendar-handle handle)
      (insert (gnus-icalendar-event->gnus-calendar event reply-status)))))

(defun gnus-calendar-save-part (handle)
  (let (event)
    (when (and (equal (car (mm-handle-type handle)) "text/calendar")
               (setq event (gnus-icalendar-event-from-handle handle gnus-calendar-identities)))

      (cal-event:sync-to-org event))))


(defun gnus-calendar-save-event ()
  "Save the Calendar event in the text/calendar part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (gnus-calendar-save-part data))))

(defun gnus-calendar-reply-accept ()
  "Accept invitation in the current article."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-reply (list gnus-calendar-handle 'accepted gnus-calendar-event))
    (setq-local gnus-calendar-reply-status 'accepted)))

(defun gnus-calendar-reply-tentative ()
  "Send tentative response to invitation in the current article."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-reply (list gnus-calendar-handle 'tentative gnus-calendar-event))
    (setq-local gnus-calendar-reply-status 'tentative)))

(defun gnus-calendar-reply-decline ()
  "Decline invitation in the current article."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-reply (list gnus-calendar-handle 'declined gnus-calendar-event))
    (setq-local gnus-calendar-reply-status 'declined)))

(defun gnus-calendar-event-export ()
  "Export calendar event to `org-mode', or update existing agenda entry."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-sync-event-to-org gnus-calendar-event))
  ;; refresh article buffer in case the reply had been sent before initial org
  ;; export
  (with-current-buffer gnus-summary-buffer
    (gnus-summary-show-article)))

(defun gnus-calendar-event-show ()
  "Display `org-mode' agenda entry related to the calendar event."
  (interactive)
  (gnus-calendar-show-org-entry
   (with-current-buffer gnus-article-buffer
     gnus-calendar-event)))

(defun gnus-calendar-event-check-agenda ()
  "Display `org-mode' agenda for days between event start and end dates."
  (interactive)
  (gnus-calendar-show-org-agenda
   (with-current-buffer gnus-article-buffer gnus-calendar-event)))

(defun gnus-calendar-setup ()
  (add-to-list 'mm-inlined-types "text/calendar")
  (add-to-list 'mm-automatic-display "text/calendar")
  (add-to-list 'mm-inline-media-tests '("text/calendar" gnus-calendar-mm-inline identity))

  (gnus-define-keys (gnus-summary-calendar-map "i" gnus-summary-mode-map)
    "a" gnus-calendar-reply-accept
    "t" gnus-calendar-reply-tentative
    "d" gnus-calendar-reply-decline
    "c" gnus-calendar-event-check-agenda
    "e" gnus-calendar-event-export
    "s" gnus-calendar-event-show)

  (require 'gnus-art)
  (add-to-list 'gnus-mime-action-alist
               (cons "save calendar event" 'gnus-calendar-save-event)
               t))


(provide 'gnus-icalendar)
;;; gnus-icalendar.el ends here

;;; journal.el --- Working with journal (diary) entries   -*- lexical-binding: t -*-

;; Copyright © 2012–2015, 2018 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 15 Nov 2012
;; Version: 0.1
;; URL: https://github.com/alezost/journal.el
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some miscellaneous functionality for creating and
;; managing org-mode based diary entries.
;;
;; "diary" would be a more suitable name for this package, but there is
;; an in-built Emacs package with that name already, so this one is
;; named "journal".

;; This package is highly specialized to suit my own needs so I think
;; it's unlikely that it may be used as it is by anyone else.  But
;; anyway, here are my "diary needs":
;;
;; 1. At first, from time to time I make some quick notes in a special
;;    file (`journal-notes-file'), for which I have an org template:
;;
;; (setq org-capture-templates
;;       '(("n" "notes" entry (file org-default-notes-file)
;;          "* %T\n  %?\n")))
;;
;; 2. When enough notes were accumulated there, I use
;;    `journal-create-entry' command and then I try to expand all that
;;    stuff into something readable.
;;
;; 3. My journal files have names "2004", "2005", … and are placed in
;;    `journal-directory'.  The structure of those org files is the
;;    following (it is created automatically):
;;
;; * 2014
;; ** 2014-01 January
;; *** 2014-01-02 Thursday
;; ...
;; ** 2014-12 December
;; *** 2014-12-31 Wednesday
;;     :PROPERTIES:
;;     :ID:       abb90b74-4fbd-4830-a9c6-35b841998213
;;     :described: <2014-12-30 Tue>--<2014-12-31 Wed>
;;     :created:  <2014-12-31 Wed 20:08>--<2014-12-31 Wed 21:20>
;;     :END:
;; ****                                              :guix:emacs:
;;    Yesterday I added 2 more files ...
;;
;; 4. My `journal-template-file' has the following contents:
;;
;; -*- mode: org; -*-
;; #+STARTUP: showeverything
;;
;; 5. Also my journal entries are full of links to other entries.  I use
;;    a usual pair of org commands (`org-store-link' and
;;    `org-insert-link') for that.  This file
;;    contains some code to add "journal" links, which look this:
;;
;;    [[journal:85192c5f-aa3c-4dab-89db-f752f8076911::record][17.12.2014]]
;;
;; 6. And finally I can quickly search all my diary files using
;;    `journal-search-by-date' and `journal-search-by-re' commands.
;;
;; ∞. Oof, I think that's all more or less.

;; This package optionally depends on "date-at-point" package
;; <https://gitlab.com/alezost-emacs/date-at-point>.

;; This package has nothing to do with
;; <https://github.com/bastibe/org-journal>.  It is a totally different
;; project.

;;; Code:

(require 'org-datetree)
(require 'org-id)
(require 'grep)
(require 'date-at-point nil t)

(defgroup journal nil
  "Org based journal/diary."
  :group 'convenience
  :group 'org)

(defcustom journal-directory "~/Documents/journal"
  "Directory with journal files."
  :type 'directory
  :group 'journal)

(defcustom journal-notes-file org-default-notes-file
  "File with temporary notes for journal."
  :type 'file
  :group 'journal)

(defcustom journal-template-file nil
  "If non-nil, used as a template for journal files."
  :type '(choice (const :tag "No" nil)
                 file)
  :group 'journal)

(defcustom journal-open-block "|"
  "String for openning an embedded block."
  :type 'string
  :group 'journal)

(defcustom journal-close-block "|"
  "String for closing an embedded block."
  :type 'string
  :group 'journal)

(defcustom journal-block-date-separator " — "
  "String to insert before a date in an embedded block."
  :type 'string
  :group 'journal)

(defcustom journal-late-time-seconds (* 3 60 60)
  "Number of seconds after midnight that counted as a previous date.
For instance, if this variable is set to 3 hours (default), when
you make a journal entry at '2:30 AM 12 Jul', it will belong to
11 Jul."
  :type 'integer
  :group 'journal)

(defvar journal-entry-heading-regexp "^\\*\\*\\* +\\(.*\\)$"
  "Regular expression matching journal entry heading.")

(defvar journal-entry-heading-time-format "%Y-%m-%d %A"
  "Format time string of a journal entry heading.")

(defvar journal-file-name-regexp "\\`[0-9]\\{4\\}\\'"
  "Regular expression matching journal file name.")

(defvar journal-files-pattern "20*"
  "Shell pattern matching journal files.")

(defvar journal-current-file nil
  "Last used journal file.
If non-nil, used in `journal-position-windows'.")

(defvar journal-described-property-name "described")
(defvar journal-created-property-name   "created")
(defvar journal-converted-property-name "converted")

(defun journal-get-time-stamp (time &optional with-hm)
  "Return org time stamp string from TIME (iso or system format).
WITH-HM means use the stamp format that includes the time of the day."
  (let ((fmt (funcall (if with-hm #'cdr #'car)
                      org-time-stamp-formats)))
    (and (stringp time)
         (setq time (org-read-date nil t time)))
    (format-time-string fmt time)))

(defun journal-get-time-from-stamp (org-time &optional end-time-p force)
  "Return time value from org time stamp or range ORG-TIME.
Use the start part of the time range if END-TIME-P is nil.
If ORG-TIME is a single time stamp and END-TIME-P is non-nil,
return nil; with FORCE return ORG-TIME time value. "
  (or (string-match org-tsr-regexp org-time)
      (error "Wrong org time stamp/range"))
  (let ((ts (if (string-match "---?" org-time)
                (if end-time-p
                    (substring org-time (match-end 0))
                  (substring org-time 0 (match-beginning 0)))
              (and (or force (not end-time-p))
                   org-time))))
    (and ts
         (apply #'encode-time
                (org-parse-time-string ts)))))

(defun journal-start-time ()
  "Return start time for a new journal entry.
Returning value is a time value defined from the first time stamp
from `journal-notes-file'.  Return nil if no time stamps found."
  (let ((buf (find-file-noselect journal-notes-file)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward org-ts-regexp nil t)
          (let ((ts (buffer-substring-no-properties
                     (match-beginning 0) (match-end 0))))
            (apply #'encode-time
                   (org-parse-time-string ts))))))))

(defun journal-date (time)
  "Return time value of TIME shifted by `journal-late-time-seconds'."
  (time-add time
            (seconds-to-time (- journal-late-time-seconds))))

(defun journal-date-list (time)
  "Return (MONTH DAY YEAR) list from time value TIME.
Such a list is used in `org-datetree-find-date-create'.
See also `calendar-gregorian-from-absolute'."
  (let ((date (butlast (nthcdr 3 (decode-time time)) 3)))
    (list (nth 1 date)
          (car date)
          (nth 2 date))))

(defun journal-insert-empty-entry (date-list)
  "Insert new DATE-LIST day entry in the current buffer.
DATE-LIST is a list (month day year).
This function is the same as `org-datetree-find-date-create', but
it creates a new entry even if an entry with this date already
exists."
  (org-datetree-find-date-create date-list)
  ;; Duplicate current heading if the day entry exists.
  (when (save-excursion
          (re-search-forward org-property-start-re
                             (save-excursion
                               (org-forward-heading-same-level 1)
                               (point))
                             'no-error))
    ;; Scroll through all entries with a specified described day to add
    ;; the new entry after the last one.
    (let ((heading (buffer-substring-no-properties
                    (point) (line-end-position))))
      (org-forward-heading-same-level 1)
      (while (string= heading
                      (buffer-substring-no-properties
                       (point) (line-end-position)))
        (org-forward-heading-same-level 1))
      (save-excursion (insert heading "\n")))))

(defun journal-insert-entry (start-date end-date created-time
                                        &optional buffer)
  "Add journal entry to a buffer BUFFER.
If BUFFER is nil, use current buffer.
START-DATE, END-DATE, CREATED-TIME should be time values."
  (let ((start-date-list (journal-date-list start-date))
        (end-date-list   (journal-date-list end-date)))
    (with-current-buffer (or buffer (current-buffer))
      (journal-insert-empty-entry end-date-list)
      (org-id-get-create)
      (org-set-property journal-described-property-name
                        ;; Compare days, not microseconds.
                        (if (equal start-date-list end-date-list)
                            (journal-get-time-stamp start-date)
                          (concat (journal-get-time-stamp start-date)
                                  "--"
                                  (journal-get-time-stamp end-date))))
      (org-set-property journal-created-property-name
                        (journal-get-time-stamp created-time t))
      ;; Show properties, add subheading.
      (outline-show-entry)
      (re-search-forward org-property-end-re nil t)
      (insert "\n   ")
      (journal-insert-subheading))))

;;;###autoload
(defun journal-create-entry (start-date end-date)
  "Create journal entry for describing a date range.
START-DATE and END-DATE should be time values.
The entry is created in a file with START-DATE's year name in
`journal-directory'.
Interactively, prompt for a described range."
  (interactive
   (save-excursion
     (delete-other-windows)
     (find-file journal-notes-file)
     (list (org-read-date nil t nil "Start date" (journal-date
                                                  (journal-start-time)))
           (org-read-date nil t nil "End date" (current-time)))))
  (let ((file (expand-file-name (format-time-string "%Y" end-date)
                                journal-directory)))
    (find-file file)
    (when (and (not (file-exists-p file))
               (file-exists-p journal-template-file))
      (insert-file-contents journal-template-file)
      (save-buffer))
    (setq journal-current-file file)
    (or (eq major-mode 'org-mode) (org-mode))
    (journal-position-windows (current-buffer))
    (journal-insert-entry start-date end-date (current-time))))

;;;###autoload
(defun journal-position-windows (&optional buffer)
  "Position notes buffer and journal BUFFER into 2 windows.
Notes buffer is a buffer visiting `journal-notes-file'.
If BUFFER is nil, use a buffer with `journal-current-file'."
  (interactive)
  (find-file journal-notes-file)
  (delete-other-windows)
  (split-window-sensibly)
  (other-window 1)
  (if buffer
      (switch-to-buffer buffer)
    (find-file journal-current-file)))

;;;###autoload
(defun journal-insert-subheading ()
  "Insert subentry heading before the current line and prompt for tags."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (unless (outline-on-heading-p)
      (newline)
      (insert "**** "))
    (org-set-tags)))

;;;###autoload
(defun journal-insert-block ()
  "Insert block at point."
  (interactive)
  (insert journal-open-block)
  (save-excursion
    (insert journal-block-date-separator)
    (org-insert-time-stamp (current-time) 't)
    (insert journal-close-block)))

(defun journal-back-to-entry-heading ()
  "Move to the beginning of previous entry (3rd level) heading."
  (interactive)
  (if (re-search-backward journal-entry-heading-regexp nil t)
      (beginning-of-line)
    (error "Before first entry heading")))

(defun journal-at-heading-p ()
  "Return non-nil, if the current line is an entry heading."
  (save-excursion
    (beginning-of-line)
    (looking-at journal-entry-heading-regexp)))

(defmacro journal-with-current-entry (&rest body)
  "Evaluate BODY with point placed on a current entry heading."
  `(save-excursion
     ;; Move to an entry heading.  If already in a heading, don't move
     ;; to the previous one.
     (end-of-line)
     (journal-back-to-entry-heading)
     ,@body))

(put 'journal-with-current-entry 'lisp-indent-function 0)

(defun journal-id ()
  "Return ID property of the current journal entry."
  (journal-with-current-entry
    (or (org-id-get)
        (error "No ID property"))))

(defun journal-file-p (file)
  "Return non-nil if FILE is a journal file."
  (let ((journal-dir (file-name-as-directory
                      (expand-file-name journal-directory)))
        (file-dir    (file-name-directory
                      (expand-file-name file)))
        (file-name   (file-name-nondirectory file)))
    (and (string= journal-dir file-dir)
         (string-match-p journal-file-name-regexp
                         file-name))))

(defun journal-buffer-p (&optional buffer)
  "Return non-nil if file in BUFFER is a journal file.
If BUFFER is nil, check the current buffer."
  (let ((file (buffer-file-name buffer)))
    (and file (journal-file-p file))))

(defun journal-get-entry-property (property)
  "Get PROPERTY of the current entry."
  (save-excursion
    (journal-back-to-entry-heading)
    (org-entry-get nil property)))

(defun journal-set-entry-property (property value)
  "Set PROPERTY of the current entry to VALUE."
  (save-excursion
    (journal-back-to-entry-heading)
    (org-set-property property value)))


;;; Modifying time properties

(defun journal-change-start-time (range-or-stamp &optional time with-hm)
  "Change the start value of time RANGE-OR-STAMP."
  (let ((beg (or time
                 (org-read-date with-hm t nil nil
                                (journal-get-time-from-stamp
                                 range-or-stamp))))
        (end (journal-get-time-from-stamp range-or-stamp t)))
    (if end
        (concat (journal-get-time-stamp beg with-hm) "--"
                (journal-get-time-stamp end with-hm))
      (journal-get-time-stamp beg with-hm))))

(defun journal-change-end-time (range-or-stamp &optional time with-hm)
  "Change the end value of time RANGE-OR-STAMP."
  (let ((beg (journal-get-time-from-stamp range-or-stamp))
        (end (or time
                 (org-read-date with-hm t nil nil
                                (journal-get-time-from-stamp
                                 range-or-stamp t)))))
    (concat (journal-get-time-stamp beg with-hm) "--"
            (journal-get-time-stamp end with-hm))))

(defun journal-change-time-property (function property &optional time with-hm)
  "Change time of PROPERTY in the current entry using FUNCTION.
TIME is time value. If it is nil, prompt for it.
WITH-HM means use the stamp format that includes the time of the day.
If PROPERTY does not exist, create it.
FUNCTION is called with PROPERTY value, TIME and WITH-HM as arguments."
  (let* ((old (journal-get-entry-property property))
         (new (if old
                  (funcall function old time with-hm)
                (journal-get-time-stamp (or time
                                            (org-read-date with-hm t))
                                        with-hm))))
    (journal-set-entry-property property new)))

(defun journal-change-start-time-property (property &optional time with-hm)
  "Change start time of PROPERTY in the current entry with TIME.
See `journal-change-time-property' for details."
  (journal-change-time-property #'journal-change-start-time
                                property time with-hm))

(defun journal-change-end-time-property (property &optional time with-hm)
  "Change end time of PROPERTY in the current entry with TIME.
See `journal-change-time-property' for details."
  (journal-change-time-property #'journal-change-end-time
                                property time with-hm))

(defun journal-change-created-property ()
  "Modify \"created\" property in the current entry with the current time.
See `journal-change-time-property' for details."
  (interactive)
  (journal-change-end-time-property
   journal-created-property-name (current-time) t))

(defun journal-change-converted-property ()
  "Modify \"converted\" property in the current entry with the current time.
See `journal-change-time-property' for details."
  (interactive)
  (journal-change-end-time-property
   journal-converted-property-name (current-time) t))

(defun journal-change-described-property (&optional arg)
  "Modify \"described\" property in the current entry.
See `journal-change-time-property' for details.
If ARG is nil, change the start date of the range.
If ARG is \\[universal-argument], change the end date of the range.
If ARG is \\[universal-argument] \\[universal-argument], create a single time stamp."
  (interactive "P")
  (cond ((null arg)
         (journal-change-start-time-property
          journal-described-property-name))
        ((equal arg '(4))
         (journal-change-end-time-property
          journal-described-property-name))
        ((equal arg '(16))
         (journal-set-entry-property
          journal-described-property-name
          (journal-get-time-stamp (org-read-date nil t))))
        (t
         (user-error "Wrong prefix argument")))
  (when arg
    (let ((time (journal-get-time-from-stamp
                 (journal-get-entry-property
                  journal-described-property-name)
                 t t)))
      (save-excursion
        (re-search-backward journal-entry-heading-regexp nil t)
        (delete-region (match-beginning 1)
                       (progn (end-of-line) (point)))
        (insert (format-time-string
                 journal-entry-heading-time-format time))))))


;;; Searching entries

;;;###autoload
(defun journal-search-by-re (regexp)
  "Search journal files for REGEXP."
  (interactive
   (progn
     ;; This thing exists in every command from "grep.el" because it
     ;; wouldn't work otherwise.
     (grep-compute-defaults)
     (list (read-regexp "Search for regexp: "))))
  (lgrep regexp journal-files-pattern journal-directory))

;;;###autoload
(defun journal-grep ()
  "Search journal with grep.
Prompt for a searching regexp.
With \\[universal-argument] prompt for a whole grep command."
  (interactive)
  (if current-prefix-arg
      (let* ((cmd (grep-expand-template grep-template ""
                                        journal-files-pattern))
             (cmd (read-from-minibuffer "grep command: " cmd
                                        nil nil 'grep-history))
             (default-directory (file-name-as-directory
                                 journal-directory)))
        (grep cmd))
    (call-interactively #'journal-search-by-re)))

;;;###autoload
(defun journal-search-by-date (date)
  "Search for an entry containing DATE in a described property.
DATE is a time value."
  (interactive
   (list (org-read-date nil t (thing-at-point 'date))))
  (let ((file (expand-file-name (format-time-string "%Y" date)
                                journal-directory)))
    (or (file-regular-p file)
        (error "File '%s' doesn't exist" file))
    (push-mark)
    (find-file file)
    (goto-char (point-min))
    (let ((date-str (format-time-string
                     journal-entry-heading-time-format date))
          (seconds (float-time date)))
      (while (and (re-search-forward journal-entry-heading-regexp nil t)
                  (string< (match-string 1) date-str)))
      (let* ((prop (org-entry-get nil journal-described-property-name))
             (beg-seconds (float-time (journal-get-time-from-stamp prop)))
             (end-seconds (float-time (journal-get-time-from-stamp prop t))))
        (or (and (>= seconds beg-seconds)
                 (<= seconds end-seconds))
            (error "No entries with this date"))))))


;;; Org links to journal entries

(defvar journal-link-regexp
  ;; Originates from `org-bracket-link-regexp'.
  "\\[\\[journal:\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)\\]"
  "Regular expression matching a journal link.")

(defvar journal-link-date-format "%d.%m.%Y"
  "Format string of a date used as a description for a journal link.")

(defun journal-link-date ()
  "Return described or created date of the current journal entry."
  (journal-with-current-entry
    (format-time-string
     journal-link-date-format
     (journal-get-time-from-stamp
      (or (org-entry-get nil journal-described-property-name)
          (org-entry-get nil journal-created-property-name))
      t t))))

(defun journal-link-search-part ()
  "Return search part for a journal link.
Search part is:

  nil               if point is in an entry heading;
  selected region   if region is active;
  current word      otherwise."
  (unless (journal-at-heading-p)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (thing-at-point 'word))))
      (regexp-quote text))))

(defun journal-link ()
  "Return journal link to the current location."
  (let ((id     (journal-id))
        (search (journal-link-search-part)))
    (concat "journal:" id
            (and search (concat "::" search)))))

(defun journal-store-link ()
  "Store journal link to the current entry."
  (when (journal-buffer-p)
    (org-store-link-props
     :type        "journal"
     :link        (journal-link)
     :description (journal-link-date))))

(defun journal-find-link (id &optional regexp)
  "Visit journal file containing an entry with ID.
If REGEXP is non-nil, search for it in the found entry."
  (let* ((find-list (org-id-find id))
         (file      (car find-list))
         (pos       (cdr find-list)))
    (org-mark-ring-push)
    (find-file file)
    (goto-char pos)
    (when regexp
      (let ((end-of-entry
             (save-excursion
               (end-of-line)
               (and (re-search-forward "^\\*\\{1,3\\} " nil t)
                    (match-beginning 0))))
            foundp found-pos)
        (save-excursion
          (while (and (setq foundp (re-search-forward
                                    regexp end-of-entry t))
                      (setq found-pos (match-beginning 0))
                      ;; Ignore matches if they are placed inside
                      ;; journal links.  `org-in-regexp' modifies match
                      ;; data so set FOUND-POS before calling it.
                      (org-in-regexp journal-link-regexp))))
        (if foundp
            (goto-char found-pos)
          (message "Regexp '%s' not found." regexp))))))

(defun journal-open-link (link)
  "Follow journal LINK."
  (let ((id link) search)
    (when (string-match "::\\(.+\\)\\'" link)
      (setq search (match-string 1 link)
            id (substring link 0 (match-beginning 0))))
    (journal-find-link id search)))

(org-link-set-parameters
 "journal"
 :follow #'journal-open-link
 :store #'journal-store-link)

(provide 'journal)

;;; journal.el ends here

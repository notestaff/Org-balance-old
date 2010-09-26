;;; org-balance.el --- Set and check goals for spending your time or other resources
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.9
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; org-balance: orgmode tools for setting goals for how much time/effort you want to spend on various
;; areas of life, and tracking how well you meet them.
;; Lets you set goals for how much time/effort to spend on various areas,
;; at various levels of granularity.  E.g. you can say "read or try something new" on average
;; once a week, and be very flexible about how you meet that.
;; 
;; Also lets you track progress towards specific goals.
;;
;; Externally usable functions:
;;
;;   Top-level dispatcher:
;;
;;      org-balance-menu - show a menu of org-balance commands
;;
;;   Recording:
;;
;;      org-balance-record-time - record time spent under the current entry.
;;
;;   Reporting:
;;
;;      org-balance-done-in-range - show items completed in given date range (e.g. within last two weeks).
;;      org-balance-show-clocked-time - show things on which you worked recently
;;      org-balance-show-neglected-time - show things on which you spend less time than you want
;;      org-balance-show-non-neglected-time - show things on which you spend enough time
;;      org-balance-show-neglected-val - show things where you get less done than you want
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: external dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-clock)
(require 'org-agenda)
(require 'org-compat)
(require 'org-macs)
(require 'rxx)
(eval-when-compile (require 'cl))

(defvar org-clock-report-include-clocking-task)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: org-balance customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup org-balance nil
  "Options for the org-balance package"
  :tag "Org Balance"
  :group 'org
  :link '(url-link "http://sourceforge.net/projects/org-balance/")
  )

(defcustom org-balance-default-margin-percent 5.0
  "Report entries as neglected if they are neglected by at least percentage"
  :group 'org-balance
  :type 'float)

(defcustom org-balance-default-interval "1 week"
  "By default, report goal compliance averaged over this time interval
before today."
  :group 'org-balance
  :type 'string)   ;; todo: add a validation function to make sure this can be parsed as a duration.


(defcustom org-balance-default-polarity (quote (("clockedtime" . atleast) ("done" . atleast) ("spend" . atmost)))
  "Default polarity to use for specific properties"
  :group 'org-balance
  :type '(alist :key-type string :value-type (radio (const :tag "at least" atleast)
						    (const :tag "at most" atmost))))

(defcustom org-balance-agenda-sorting-strategy
  '(priority-down category-keep user-defined-down)
  "Sorting rules for sorting org-balance agenda views."
  :group 'org-balance
  :link '(variable-link org-agenda-sorting-strategy)
  :type `(repeat ,org-sorting-choice))

(defgroup org-balance-faces nil
  "Faces in org-balance"
  :tag "org-balance faces"
  :group 'org-balance)

(defface org-balance-malformed-goal
  '((((background light)) (:foreground "Firebrick"))
    (((background dark)) (:foreground "black")))
  "Face used for showing malformed goals"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: org-balance overlays
;; 
;; Code for managing org-balance overlays
;;
;; Aadapted from org-clock.el; it might make sense to
;; make a generic overlay facility for use by various org-modules.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-balance-overlays nil
  "We use overlays to display summary information about how much got done
under each category.  Modeled on org-clock-overalys.")
(make-variable-buffer-local 'org-balance-overlays)

(defun org-balance-put-overlay (txt &optional error)
  "Put an overlay on the current line, displaying TXT.
This creates a new overlay and stores it in `org-balance-overlays', so that it
will be easy to remove."
  (unless (listp txt) (setq txt (list txt)))
  (let* ((c 60)    ;; column in which the overlay starts 
	 (off 0)
	 ov tx)
    (org-move-to-column c)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (setq ov (make-overlay (1- (point)) (point-at-eol))
	  tx (concat (buffer-substring (1- (point)) (point))
		     (make-string (+ off (max 0 (- c (current-column)))) ?.)
		     (org-add-props (first txt)
			 (list 'face (if error 'org-balance-malformed-goal 'org-clock-overlay)))
		     (mapconcat (lambda (tx)
				  (let* ((result (copy-sequence tx))
					 (dummy (org-add-props result
						    (list 'face 'org-clock-overlay))))
				    (concat "\n" (make-string c (string-to-char " ")) result)))
				(cdr txt)
				"")
		     ""))
    (if (not (featurep 'xemacs))
	(overlay-put ov 'display tx)
      (overlay-put ov 'invisible t)
      (overlay-put ov 'end-glyph (make-glyph tx)))
    (push ov org-balance-overlays)))

(defun org-balance-remove-overlays (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (if (or (not org-balance-overlays) org-inhibit-highlight-removal)
      nil
    (progn
	(mapc 'delete-overlay org-balance-overlays)
	(setq org-balance-overlays nil)
	(unless noremove
	  (remove-hook 'before-change-functions
		       'org-clock-remove-overlays 'local))
	t)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-balance-remove-overlays)

(defun org-balance-unload-function ()
  "Remove any hooks pointing to org-balance functions"
  (remove-hook 'org-ctrl-c-ctrl-c-hook 'org-balance-remove-overlays))

(defun org-balance-reset-overlays ()
  "Reset org-balance overlays if present, preparing to put on new ones"
  (org-overview)
  (org-balance-remove-overlays)
  (org-unhighlight)
  (when org-remove-highlights-with-change
    (org-add-hook 'before-change-functions 'org-balance-remove-overlays
		  nil 'local)))

(defun org-balance-unhighlight ()
  (interactive)
  (org-unhighlight))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Utils
;;
;; General-purpose utility routines used in org-balance module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun org-balance-groupby (z key-func)
  "Group items in a list by their key, using the specified key extractor.
Return an a-list mapping keys to items with that key.  "
  (setq z (copy-sequence z))
  (setq z (sort z (lambda (x y) (< (funcall key-func x) (funcall key-func y)))))
  (let (result)
    (dolist (x z)
      (let* ((k (funcall key-func x)))
	(when (or (null result) (not (equal k (car (first result)))))
	  (push (cons k nil) result))
	(push x (cdr (first result)))))
    (reverse result)))
  
(defun org-balance-get-property (prop &optional default-val)
  "Get the value of a property, represented either as text property or org property,
at point.  If PROP is a symbol or keyword it is assumed
to be a text property; otherwise (if it is a string) it is assumed
to be an org entry property. Some properties are represented as text properties, and some as org properties;
this function gives uniform access to a named property regardless of how it is represented.
The advantage of org properties is that they're saved with the file; the advantage of
text properties is that they don't create clutter for the user.

Properties stored as text properties are named using keywords, such a :myprop,
or symbols, such as 'myprop; properties stored as org properties are named using strings.
"
  (let ((prop-val (if (stringp prop)
		      (org-entry-get nil prop)
		    (get-text-property (point) prop))))
    (or prop-val default-val)))

(defun org-balance-set-property (prop val &optional default-val clear-when-default)
  "Set the value of an org property or a text property PROP
to the value VAL.  If PROP is a symbol or keyword it is assumed
to be a text property; otherwise (if it is a string) it is assumed
to be an org entry property.  The point is assumed to be on an org entry headline.  For
text property, it is set for the whole headline.
If DEFAULT-VAL is given and VAL is the DEFAULT-VAL, then the property
is not set (and is removed if it was set before and CLEAR-WHEN_DEFAULT is non-nil)."
  (let ((is-default (equal val default-val)))
    (if (stringp prop)
	(progn
	  (if is-default
	      (when clear-when-default (org-entry-delete nil prop))
	    (org-entry-put nil prop (format "%s" val))))	
      (progn
	(when (or (not is-default) clear-when-default)
	  (remove-list-of-text-properties (point-at-bol) (point-at-eol) (list prop)))
	(unless is-default
	  (put-text-property (point-at-bol) (point-at-eol) prop val))))))

(defun org-balance-delete-property-globally (prop)
  "Delete either text or org property globally from all entries."
    (if (stringp prop)
	(org-delete-property-globally prop)
      (remove-list-of-text-properties (point-min) (point-max) (list prop))))

(defun org-balance-trim-whitespace (s)
  "Trim trailing and leading whitespace from string"
  (save-match-data
    (replace-regexp-in-string
     (rx (or (seq string-start (zero-or-more whitespace))
	     (seq (zero-or-more whitespace) string-end)))
     "" (if (symbolp s) (symbol-name s) s))))

(defun org-balance-full-match (re s)
  "Do a string match, but fail unless the regexp matches the full string"
  (and (string-match re s)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-balance-done-in-range ()
  "Sparse tree of items closed in a certain time range.
Still experimental, may disappear in the future.

Adapted from `org-closed-in-range' from org.el."
  (interactive)
  ;; Get the time interval from the user.
  (let* ((time1 (org-float-time
                 (org-read-date nil 'to-time nil "Starting date: ")))
         (time2 (org-float-time (org-current-time)))
         ;; callback function
         (callback (lambda ()
                     (let ((time
                            (org-float-time
                             (apply 'encode-time
                                    (org-parse-time-string
                                     (match-string 1))))))
                       ;; check if time in interval
                       (and (>= time time1) (<= time time2))))))
    ;; make tree, check each match with the callback
    (message "%s matches here" (org-occur "CLOSED: +\\[\\(.*?\\)\\]" nil callback))) )


(defun org-balance-record-time (&optional hours ago)
  "Record the given amount of time as time spent under the current org entry.
Useful when you did some task but weren't near a computer to start/stop the org timer
as you were doing it.
"
  (interactive)
  ;
  ; allow various specifications of time: :15 for 15 mins, 15m or any other (number, unit) thing.
  ; also compound specs such as 1 hour 15 mins (so, basically, ((value unit)+) as long as units
  ; are compatible and keep decreasing.
  ;
  ; also, have additional ways to enter some specific units -- e.g. $1 for money units,
  ; :15 for 15 mins, 01:15, etc.  1h30m, 1.5hrs, etc.
  ;
  (unless hours (setq hours (float (string-to-number (read-string "How many hours? ")))))
  (unless ago (setq ago (float (string-to-number (read-string "Finished how long ago (in hours)? " nil nil 0)))))

  (message "hours is %s ago is %s" hours ago)

  (let (target-pos (msg-extra ""))
      ;; Clock in at which position?
    (setq target-pos
	  (if (and (eobp) (not (org-on-heading-p)))
	      (point-at-bol 0)
	    (point)))

    (org-clock-find-position nil)

    (insert-before-markers "\n")
    (backward-char 1)
    (org-indent-line-function)
    (when (and (save-excursion
		 (end-of-line 0)
		 (org-in-item-p)))
      (beginning-of-line 1)
      (org-indent-line-to (- (org-get-indentation) 2)))
    (insert org-clock-string " ")

    (let* ((end-time (- (org-float-time) (* ago 60.0 60.0)))
	   (start-time (- end-time (* hours 60.0 60.0)))
	   (s (- end-time start-time))
	   (h (floor (/ s 3600.0)))
	   (s (- s (* 3600.0 h)))
	   (m (floor (/ s 60.0)))
	   (s (- s (* 60.0 s))))
      (org-insert-time-stamp (seconds-to-time start-time)
			     'with-hm 'inactive)
      (insert "--")
      (org-insert-time-stamp (seconds-to-time end-time)
			     'with-hm 'inactive)
      (insert " => " (format "%2d:%02d" h m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrxx org-balance-inactive-timestamp-regexp (seq "[" (named-grp time (1+ nonl)) "]" )
  (org-float-time (apply 'encode-time (org-parse-time-string time))))

(defrxx org-balance-clock-regexp (seq bol (0+ blank) (eval org-clock-string) (0+ blank)
				      (org-balance-inactive-timestamp-regexp from) (1+ "-")
				      (org-balance-inactive-timestamp-regexp to)
				      " => " (1+ nonl))
  (cons from to))

(defun org-balance-clock-sum (tstart tend)
  "Return the total clock time in the current file restriction. Adapted from `org-clock-sum'"
  (let ((total-minutes 0))
    (when (and
	   org-clock-report-include-clocking-task
	   (equal (org-clocking-buffer) (current-buffer))
	   (<= (point-min) (marker-position org-clock-hd-marker))
	   (<= (marker-position org-clock-hd-marker) (point-max)))
      (let* ((cs (org-float-time org-clock-start-time))
	     (ts (max tstart cs))
	     (te (min tend (org-float-time)))
	     (dt (- te ts)))
	(when (> dt 0) (setq total-minutes (floor (/ dt 60))))))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(rxx-do-search-fwd org-balance-clock-regexp clock-interval
	  (let* ((ts (car clock-interval)) (te (cdr clock-interval))
		 (ts (if tstart (max ts tstart) ts))
		 (te (if tend (min te tend) te))
		 (dt (- te ts)))
	    (when (> dt 0) (incf total-minutes(floor (/ dt 60))))))))
    total-minutes))

(defrxx org-balance-closed-regexp (seq bol (0+ blank) (eval org-closed-string) (1+ blank)
				       (org-balance-inactive-timestamp-regexp time))
  time)

(defun org-balance-sum-org-property (prop tstart tend unit)
  "Fast summing of property.  Returns the sum of the property under the current restriction.

Originally adapted from `org-closed-in-range'.
"

  ;; FIXOPT: if prop-default is zero then the regexp for that subtree should be, org-closed-string _and_ the prop is explicitly set _in that entry_.  (1+ (bol) (optional (not (any ?*))) (0+ nonl) (eol))
  ;; FIXME: find also state changes to DONE, or to any done state.
  (declare (special org-balance-num-warnings))
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((prop-sum (org-balance-make-valu 0 unit))
	    (prop-default (concat "default_" prop)))
	(rxx-do-search-fwd org-balance-closed-regexp closed-time
	  (when (and (<= tstart closed-time) (<= closed-time tend))
	    (save-excursion
	      (save-match-data
		(org-back-to-heading 'invis-ok)
		(let*
		    ((prop-here
		      (or (org-entry-get nil prop)
			  (org-entry-get nil prop-default 'inherit)
			  "1"))
		     (prop-valu-here
		      (condition-case err
			  (org-balance-parse-valu prop-here)
			(error
			 (message
			  "Warning: at line %d of file %s, could not add %s to sum for goal %s; sum-so-far is %s"
			  (line-number-at-pos (point)) (buffer-file-name (current-buffer)) prop-here prop prop-sum)
			 (incf org-balance-num-warnings)
			 nil))))
		  (when prop-valu-here
		    (setq prop-sum (org-balance-add-valu prop-sum prop-valu-here))))))))
	prop-sum))))

(defun org-balance-sum-property (prop tstart tend unit)
  "Sum a property in the specified period, within the current file restriction."
  (message "at %s summing %s" (point) prop)
  (cond ((string= prop "clockedtime")
	 (org-balance-make-valu (org-balance-clock-sum tstart tend) 'minutes))
	((string= prop "actualtime")
	 (org-balance-make-valu (- tend tstart) 'seconds))
	(t (org-balance-sum-org-property prop tstart tend unit))))

(defrxx org-balance-archive-regexp (seq bol (1+ blank) ":ARCHIVE:" (1+ blank) (named-grp loc (1+ nonl))) loc)

(defun org-balance-find-all-archive-targets ()
  "Find all the places where an entry from the current subtree could have been archived"

  (save-excursion
    (save-window-excursion
      (save-restriction
	(let ((archive-locs (list (org-get-local-archive-location))))
	  (org-narrow-to-subtree)
	  (rxx-do-search-fwd org-balance-archive-regexp loc
	    (message "found loc %s at %s" loc (point))
	    (add-to-list 'archive-locs loc))
	  (mapcar (lambda (loc) (cons (org-extract-archive-file loc)
				      (org-extract-archive-heading loc)))
		  archive-locs))))))

(defun org-balance-sum-property-with-archives (prop tstart tend unit)
  "Sum a property in the specified period.  Include any entries that may have been archived from the current subtree."
  
  )



(defrxxconst org-balance-goal-todo-keyword "GOAL")

;; struct: org-balance-goal-delta - information about how well one goal is being met.
;;      `org-balance-compute-goal-deltas' gathers this information from various entries and
;;      presents it in a list.
(defstruct org-balance-goal-delta heading goal entry-buf entry-pos goal-pos actual delta-val delta-percent error-msg)

(defrxx org-balance-prop-name-regexp (1+ alnum))
(defrxx org-balance-link-regexp (named-grp link (eval-regexp (rxx-make-shy org-any-link-re))) (rxx-match-beginning 'link))
(defstruct org-balance-prop prop link)
(defrxx org-balance-prop-regexp (seq (org-balance-prop-name-regexp prop)
				     (opt (1+ blank) "at" (1+ blank) (org-balance-link-regexp link)))
  (make-org-balance-prop :prop prop :link link))

(defstruct org-balance-prop-ratio num denom)
(defrxx org-balance-prop-ratio-regexp
  (seq (org-balance-prop-regexp num) (optional (0+ blank) "/" (0+ blank) (org-balance-prop-regexp denom)))
  (make-org-balance-prop-ratio :num num :denom (or denom (make-org-balance-prop :prop "actualtime"))))

(defrxx org-balance-goal-prefix-regexp
  (seq bol (1+ "*") (1+ blank) (eval org-balance-goal-todo-keyword) (1+ blank) (optional "[#" upper "]" (1+ blank))
       (org-balance-prop-ratio-regexp prop-ratio) (0+ blank) ":" (0+ blank)) prop-ratio)

(defun org-balance-compute-actual-prop (prop tstart tend unit)
  (save-excursion
    (save-window-excursion
      (save-match-data
	(when (org-balance-prop-link prop)
	  (let ((org-link-search-must-match-exact-headline t))
	    (org-open-at-point 'in-emacs)))
	(org-balance-sum-property (org-balance-prop-prop prop) tstart tend unit)))))

(defun org-balance-compute-actual-prop-ratio (prop-ratio tstart tend parsed-goal)
  (org-balance-convert-valu-ratio
   (make-org-balance-valu-ratio
    :num (org-balance-compute-actual-prop (org-balance-prop-ratio-num prop-ratio) tstart tend
					  (org-balance-valu-unit (org-balance-goal-numer-min parsed-goal)))
    :denom (org-balance-compute-actual-prop (org-balance-prop-ratio-denom prop-ratio) tstart tend
					    (org-balance-valu-unit (org-balance-goal-denom parsed-goal))))
   (make-org-balance-valu-ratio
    :num (org-balance-goal-numer-min parsed-goal)
    :denom (org-balance-goal-denom parsed-goal))))

(defun org-balance-compute-delta (parsed-goal prop-ratio actual)
  (let* ((polarity (or (org-balance-goal-polarity parsed-goal)
		       (cdr (assoc-string (org-balance-prop-prop
					   (org-balance-prop-ratio-num  prop-ratio))
					  org-balance-default-polarity))))
	 (margin (or (org-balance-goal-margin parsed-goal)
		     org-balance-default-margin-percent))
	 (goal-min (org-balance-valu-val (org-balance-goal-numer-min parsed-goal)))
	 (goal-max (org-balance-valu-val (org-balance-goal-numer-max parsed-goal)))
	 (range-min (- goal-min
		       (if (numberp margin)
			   (* (/ (float margin) 100.0) goal-min)
			 (org-balance-valu-val margin))))
	 
	 (range-max (+ goal-max
		       (if (numberp margin)
			   (* (/ (float margin) 100.0) goal-max)
			 (org-balance-valu-val margin))))
	 
	 (actual-num (org-balance-valu-val (org-balance-valu-ratio-num actual)))
	 (delta-val (cond ((and (<= range-min actual-num) (<= actual-num range-max)) 0)
			  ((< range-max actual-num)
			   (if (eq polarity 'atleast)
			       (- actual-num goal-max)
			     (- goal-max actual-num)))
			  ((< actual-num range-min)
			   (if (eq polarity 'atmost)
			       (- goal-min actual-num)
			     (- actual-num goal-min)))))
	 (delta-percent
	  (* 100 (/ delta-val (if (< range-max actual-num) goal-max goal-min)))))
    (cons delta-val delta-percent)))

(defun* org-balance-compute-goal-deltas2 (&key goals tstart tend)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  (let ((num-errors 0) (num-under 0) (num-met 0) (num-over 0)
	(days-in-interval (org-balance-make-valu (/ (float (- tend tstart)) 60.0 60.0 24.0) 'days)))
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(save-match-data
	    ;; FIXME if goals specified, make regexp for them
	  (rxx-do-search-fwd org-balance-goal-prefix-regexp prop-ratio
	    (let* ((goal-def-here (buffer-substring (point) (point-at-eol)))
		   (parsed-goal
		    (condition-case err
			(org-balance-parse-goal-or-link-at-point)
		      (error
		       (incf num-errors)
		       (message "Error parsing %s" goal-def-here)
		       (save-match-data
			 (org-toggle-tag "goal_error" 'on)
			 (org-entry-delete nil "goal_delta_val")
			 (org-entry-delete nil "goal_delta_percent")
			 (org-entry-put (point) "goal_updated" (format-time-string
								(org-time-stamp-format 'long 'inactive)
								(current-time))))
		       nil))))
	      (when parsed-goal
		(save-match-data (org-toggle-tag "goal_error" 'off))
		;;
		;; Compute the actual usage under this subtree, and convert to the same
		;; units as the goal, so we can compare them.
		;;
		(let (delta-val-and-percent)
		  (save-match-data
		    (save-excursion
		      (save-restriction
			(outline-up-heading 1 'invisible-ok)
			(when (string= (upcase (org-get-heading)) "GOALS")
			  (outline-up-heading 1 'invisible-ok))
			(org-narrow-to-subtree)
			(goto-char (point-min))
			(setq delta-val-and-percent
			      (org-balance-compute-delta
			       parsed-goal prop-ratio
			       (org-balance-compute-actual-prop-ratio prop-ratio tstart tend parsed-goal))))))
		  (let ((delta-val (car delta-val-and-percent))
			(delta-percent (cdr delta-val-and-percent)))
		    (cond ((< delta-val 0) (incf num-under))
			  ((> delta-val 0) (incf num-over))
			  ((= delta-val 0) (incf num-met)))
		    (org-entry-put (point) "goal_delta_val" (format "%s" delta-val))
		    (org-entry-put (point) "goal_delta_percent" (format "%s" delta-percent))
		    ;; FIXME: include in goal_updated the period for which it was updated.
		    (org-entry-put (point) "goal_updated" (format-time-string
							   (org-time-stamp-format 'long 'inactive)
							   (current-time)))))))))))
	(message "err %d under %d met %d over %d" num-errors num-under (+ num-met num-over) num-over)))

(defun org-balance-do () (interactive) (org-balance-compute-goal-deltas2))

(defun org-balance-save-amt-neglected (agenda-line)
  "Given an agenda line, save the 'neglect amount' value of the corresponding org entry
as a text property, for later use by org-balance-cmp.  Also, add the neglect amount value
to the agenda line.
"
  (let ((orig-entry (get-text-property 0 'org-hd-marker agenda-line)) cur-buf)
    (when orig-entry
      (save-excursion
	(setq cur-buf (current-buffer))
	(switch-to-buffer (marker-buffer orig-entry))
	(goto-char (marker-position orig-entry))
	(let ((org-balance-delta-val (string-to-number (or (org-entry-get (point) "goal_delta_val") "0")))
	      (org-balance-delta-percent (string-to-number (or (org-entry-get (point) "goal_delta_percent") "0")))
	      )
	  (progn
	    (put-text-property 0 1 :org-balance-delta org-balance-delta-val agenda-line)
	    (concat agenda-line "::: " (number-to-string org-balance-delta-val))))))))

(defun org-balance-cmp (a b)
  "Compare agenda entries by the amount of neglect, with the most-neglected at the top."
  (let ((pa (or (get-text-property 0 :org-balance-delta a) 0))
	(pb (or (get-text-property 0 :org-balance-delta b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))


(defun org-agenda-show-path ()
  "Display the Org-mode file which contains the item at point.
When called repeatedly, scroll the window that is displaying the buffer."
  (interactive)
  (let ((win (selected-window)))
      (org-agenda-goto t)
      (org-overview)
      (org-show-context)
      (show-subtree)
      (recenter)
      (setq org-agenda-show-window (selected-window))
      (select-window win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Values with units
;;
;; Code to represent values with units, e.g. "5 hours" or "100 dollars" or just "2 items".
;; 
;; Design note: the calc.el package included with emacs supports calculations on values with units;
;; but for several reasons it was better not to use that package here.  (It is too general and too complex,
;; and we don't want to mess up other calculations by adding org-balance units to emacs standard units.)
;;
;; notes
;;
;; an alternative would be to set calc.el units only temporarily, while org-balance code runs,
;; and restore them under unwind-protect.
;;
;; old comment:
;;
;; Code for parsing and representing time intervals and ratios.  Time intervals are phrases of the form "2 hours" or
;; "1 day"; code here parses them and converts them to a uniform representation (minutes).
;; Time ratios are phrases of the form "30 minutes per day" or "3 hours a week"; more generally,
;; "[time interval] per [time interval]".   Code here parses them and converts them to a uniform representation
;; (minutes per day).  Occurrence rates are phrases of the form "1 a week" or "3 every 2 days"; more generally,
;; [amount] per [time interval].  Code here parses them and converts them to a uniform representation
;; (amount per day).  There is also code to convert from the uniform representation back to the user's original
;; unit for display (e.g. from "8.57 minutes per day" to "1 hour a week").

(defrxxconst org-balance-units
  '((time . ((second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
	     (workweek . 7200)
	     (month . 43200) (year . 525600) (bluemoon 1e12)))
    (money . ((dollar . 1) (cent . .01) (k . 1000)))
    (count . ((item . 1) (time . 1)))))

;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defrxxconst org-balance-units
  (mapcar
   (lambda (dimension-info)
     (cons (car dimension-info)
	   (apply
	    'append
	    (mapcar
	     (lambda (unit-info)
	       (list unit-info (cons (intern (concat (symbol-name (car unit-info)) "s")) (cdr unit-info))))
	     (cdr dimension-info)))))
   org-balance-units))

;; var: org-balance-unit2dim-alist - assoc list mapping each unit to its dimension (time, money, count, ...)
(defrxxconst org-balance-unit2dim-alist
  (apply
   'append
   (mapcar
    (lambda (dimension-info)
      (mapcar (lambda (unit-info) (cons (car unit-info) (car dimension-info))) (cdr dimension-info)))
    org-balance-units)))

(put 'org-balance-error 'error-conditions '(error org-balance-errors org-balance-error))
(put 'org-balance-error 'error-message "org-balance error")

(defun org-balance-assoc-val (key alist &optional error-message)
  "Looks up KEY in association list ALIST.  Unlike `assoc', returns the associated value rather than the associated pair.
Also, converts key to a symbol if it is a string.
If ERROR-MESSAGE is given, and the key is not in the list, throws an error with this message.
"
  (let ((assoc-result (assoc (if (stringp key) (intern key) key) alist)))
    (if assoc-result (cdr assoc-result)
      (if (eq error-message 'nil-ok) nil
	(signal 'org-balance-error
		(list (if error-message error-message (format "key %s not in alist %s" key alist))))))))

(defun org-balance-is-unit (unit)
  (org-balance-assoc-val unit org-balance-unit2dim-alist 'nil-ok))

(defun org-balance-unit2dim (unit)
  "Given a unit, return the dimension that its measures"
  (org-balance-assoc-val unit org-balance-unit2dim-alist))

;; var: org-balance-unit2base-alist - assoc list mapping each unit to how many base units are in it
(defconst org-balance-unit2base-alist (apply 'append (mapcar 'cdr org-balance-units)))

(defun org-balance-unit2base (unit)
  "Return the number of base units in the given unit.  For each dimension we have a base unit in terms of which all other
units are measured; for example, for time we use minutes."
  (org-balance-assoc-val unit org-balance-unit2base-alist))

;; Struct: org-balance-valu - a value together with a given unit of measurement, e.g., 5 hours. 
(defstruct (org-balance-valu
	    (:constructor new-org-balance-valu
			  (val unit-name
			       &aux (unit
				     (if (stringp unit-name) (intern unit-name) unit-name)))))
	    val unit)

(defun org-balance-scale-valu (factor valu)
  "Return the value scaled by the factor"
  (new-org-balance-valu (* factor (org-balance-valu-val valu)) (org-balance-valu-unit valu)) 
  )

(defun org-balance-make-valu (val unit)
  (if (and (numberp val) (org-balance-is-unit unit))
      (new-org-balance-valu val unit)
    (error "Invalid valu: %s %s" val unit)))

(defun org-balance-convert-valu (valu new-unit &optional multiples-of-new-unit)
  "Convert a valu to new units in the same dimension, e.g. 1 hour to 1/24 of a day.  If MULTIPLES-OF-NEW-UNIT is given,
we convert to the specified multiples of new unit."
  (when (stringp new-unit) (setq new-unit (intern new-unit)))
  (unless (eq (org-balance-unit2dim (org-balance-valu-unit valu)) (org-balance-unit2dim new-unit))
    (error "Cannot convert between incompatible units: %s and %s" (org-balance-valu-unit valu) new-unit))
  (unless multiples-of-new-unit (setq multiples-of-new-unit 1))
  (make-org-balance-valu :val (/ (/ (float (* (org-balance-valu-val valu)
					      (org-balance-unit2base (org-balance-valu-unit valu))))
				    (float (org-balance-unit2base new-unit)))
				 multiples-of-new-unit) :unit new-unit))

(defun org-balance-add-valu (valu1 valu2)
  "Add two values with units, converting them to a common unit"
  (let* ((unit1 (org-balance-valu-unit valu1))
	 (unit2 (org-balance-valu-unit valu2))
	 (smaller-unit (if (< (org-balance-unit2base unit1) (org-balance-unit2base unit2))
			   unit1 unit2))
	 (conv1 (org-balance-convert-valu valu1 smaller-unit))
	 (conv2 (org-balance-convert-valu valu2 smaller-unit)))
    (org-balance-make-valu (+ (org-balance-valu-val conv1)
			      (org-balance-valu-val conv2))
			   smaller-unit)))

(put 'org-balance-parse-error 'error-conditions '(error org-balance-errors org-balance-parse-error))
(put 'org-balance-parse-error 'error-message "org-balance: Could not parse")

(defrxxconst org-balance-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(defrxx org-balance-number-name-regexp
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-balance-number-names))))
  (lambda (match) (cdr-safe (assoc-string match org-balance-number-names))))

(defrxx org-balance-number-regexp
  (seq
   (zero-or-more whitespace)
   
   (or
    ;; either an english number name
       (org-balance-number-name-regexp named-number)
       
       ;; or a floating-point number, possibly in scientific notation
       (seq
	(optional (any "+-"))
	(or (seq (one-or-more (any digit))
		 (optional ".")
		 (optional (one-or-more (any digit))))
	    (seq "." (one-or-more (any digit))))
	(optional
	 (seq (any "eE")
	      (optional (any "+-"))
	      (one-or-more (any digit))))))
   
   (zero-or-more whitespace))
  (lambda (match)
    (or named-number
	(string-to-number match)))
  "number")

(defun org-balance-is-valid-number-p (s)
  "Test if s is a number or a string representing a valid number (ignoring leading or trailing whitespace).
The whole string must match a floating-point number optionally surrounded by whitespace; extraneous characters in string
are not allowed.
"
  (if (numberp s) s
    (save-match-data
      (org-balance-full-match org-balance-number-regexp s))))

(defun org-balance-string-to-number (s)
  "Convert a string to a number, recognizing some number names for readability.  If s is already a number, just return it.
Unlike the standard `string-to-number', if the string as a whole cannot be interpreted as a valid number possibly surrounded
by whitespace, it throws an error rather than silently returning zero.
"
  (if (numberp s) s
    (rxx-parse org-balance-number-regexp s)))

(defalias 'org-balance-parse-number 'org-balance-string-to-number)

(defrxx org-balance-number-range-regexp
  (seq
   (org-balance-number-regexp range-start)
   (optional "-" (org-balance-number-regexp range-end)))
  (cons range-start (or range-end range-start)))

(defvar org-balance-parse-valu-hooks nil
  "List of hooks for parsing valu strings (value with units), such as `5 hours'.  Can be used e.g. to parse currency
such as $5 into the canonical form `5 dollars'.  Each hook must take a string as an argument and return either an
`org-balance-valu' struct if it successfully parsed the string, or nil if it didn't.")

(defrxx org-balance-unit-regexp
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-balance-unit2dim-alist)))))

(defrxx
    org-balance-valu-regexp
    ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
    ;; or an optional number (assumed to be 1 if not given) followed by a unit.
    ;; But either a number or a unit must be given.
    (or (seq (optional (org-balance-number-regexp val))
	     (org-balance-unit-regexp unit))
	(seq val (optional unit)))
     (org-balance-make-valu (or val 1) (or unit "item"))
     "value with unit")
  
(defun org-balance-parse-valu (valu-str)
  "Given a string representing a value with units, parse it into an org-balance-valu structure."
  (or
   (run-hook-with-args-until-success 'org-balance-parse-valu-hooks valu-str)
   (rxx-parse org-balance-valu-regexp valu-str)))

(defrxx
  org-balance-valu-range-regexp
  ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
  ;; But either a number or a unit must be given.
  (or (seq (optional (seq (org-balance-number-range-regexp range) (one-or-more whitespace)))
	   (org-balance-unit-regexp unit))
      (seq range (optional (one-or-more whitespace) unit)))
  (let ((number-range (or range (cons 1 1)))
	(unit (or unit "item")))
    (cons (org-balance-make-valu (car number-range) unit)
	  (org-balance-make-valu (cdr number-range) unit)))
  "value range")

(defun org-balance-parse-valu-range (valu-str)
  "Given a string representing a value range with units, parse it into an org-balance-valu structure."
  (rxx-parse org-balance-valu-range-regexp valu-str))

(defrxxconst org-balance-ratio-words (list "per" "every" "each" "/" "a" "in a"))
(defrxxconst org-balance-ratio-words-regexp (rxx (eval-regexp (regexp-opt org-balance-ratio-words 'words))))

;; Struct: org-balance-valu-ratio - a ratio of two valu's.
(defstruct org-balance-valu-ratio num denom
  ;; ratio- - the word from org-balance-ratio-words to use when printing the ratio.
  ratio-word)

(defun org-balance-convert-valu-ratio (old-valu-ratio new-valu-ratio)
  "Convert a valu ratio to new units, e.g. minutes per day to hours per week.  We keep the denominator of the new ratio,
changing only the numerator."
  (let ((new-num (org-balance-valu-ratio-num new-valu-ratio))
	(new-denom (org-balance-valu-ratio-denom new-valu-ratio)))
    (make-org-balance-valu-ratio
     :num (org-balance-make-valu (/ (org-balance-valu-val (org-balance-convert-valu
							   (org-balance-valu-ratio-num old-valu-ratio)
							   (org-balance-valu-unit new-num)))
				    (org-balance-valu-val (org-balance-convert-valu
							   (org-balance-valu-ratio-denom old-valu-ratio)
							   (org-balance-valu-unit new-denom)
							   (org-balance-valu-val new-denom))))
				 (org-balance-valu-unit new-num))
     :denom new-denom :ratio-word (org-balance-valu-ratio-ratio-word new-valu-ratio)))) 


;; Struct: org-balance-goal - the user-specified goal you have for a ratio.
(defstruct org-balance-goal
  numer-min
  numer-max
  denom
  polarity
  margin
  priority
  ratio-word
  text)

(defun org-balance-scale-goal (factor goal)
  (let ((result (copy-org-balance-goal goal)))
    (setf (org-balance-goal-numer-min result)
	  (org-balance-scale-valu factor (org-balance-goal-numer-min result)))
    (setf (org-balance-goal-numer-max result)
	  (org-balance-scale-valu factor (org-balance-goal-numer-max result)))
    (setf (org-balance-goal-text result) (format "%.2f * (%s)" factor (org-balance-goal-text goal)))
    result
  ))

(defrxx org-balance-polarity-regexp
  (seq (zero-or-more whitespace)
       (or (named-grp atmost (eval-regexp (regexp-opt (list "at most"))))
	   (named-grp atleast (eval-regexp (regexp-opt (list "at least")))))
       (zero-or-more whitespace))
  (if atmost 'atmost 'atleast)
  "polarity")
  
(defrxx org-balance-goal-regexp
  (seq
   (0+ blank)
   (optional
    (seq (named-grp priority (regexp "\\[#[A-Z0-9]\\]")) (0+ blank)))
   (optional (org-balance-polarity-regexp polarity))
   (org-balance-valu-range-regexp numerator)
   (1+ blank) (org-balance-ratio-words-regexp ratio-word) (1+ blank)
   (org-balance-valu-regexp denominator)
   (optional
    ;; specify margin
    (seq
     (1+ blank) "+-" (0+ blank)
     (or
      (seq (org-balance-number-regexp margin) (0+ blank) "%")
      (org-balance-valu-regexp margin))))
   (0+ blank))
  
  (lambda (goal-str)
    (make-org-balance-goal 
     :numer-min (car numerator)
     :numer-max (cdr numerator)
     :denom denominator
     :polarity polarity
     :margin margin
     :priority priority
     :text goal-str
     :ratio-word ratio-word))
  "value ratio goal")

(defrxx org-balance-goal-link-regexp
  (seq (org-balance-number-regexp factor)
       (1+ blank)
       "of"
       (1+ blank)
       org-balance-link-regexp)
  factor)

(defrxx org-balance-goal-or-link-regexp
  (or (org-balance-goal-regexp goal)
      (org-balance-goal-link-regexp link))
  (or goal link))

(defun org-balance-parse-goal-or-link-at-point ()
  "Parse goal or link at point"
  ;; so, we need to know what the goal was, so that we can look for the same goal at the target entry.
  ;; and also, need to put in a check for infinite recursion.

  ;;
  ;; things to do:
  ;;    - check for infinite recursion
  ;;    - make sure the link we're following points to a file.  or is an internal link.
  ;;    - recognize also parent links, and/or paths
  ;;    - proper error reporting:
  ;;         - if a goal does not parse
  ;;         - if following a link fails to get us to an entry in an orgfile
  ;;         - if that entry does not have the (number'th) requisite goal.
  ;;    - factor out the follow-a-link code, so that we can use it 
  (save-match-data
    (let ((result (rxx-parse-fwd org-balance-goal-or-link-regexp (point-at-eol) 'partial-match-ok)))
      (if (org-balance-goal-p result) result
	(save-excursion
	  (save-window-excursion
	    (let ((org-link-search-must-match-exact-headline t))
	      (org-open-at-point 'in-emacs))
	    (unless (rxx-search-fwd org-balance-goal-prefix-regexp (point-at-eol))
	      (error "No goal found at link taget"))
	    (org-balance-scale-goal result (org-balance-parse-goal-or-link-at-point))))))))


(defun org-balance-remove-props ()
  (interactive)
  (dolist (prop '("goal_delta_val" "goal_delta_percent" "goal_updated"))
    (org-delete-property-globally prop)))

(provide 'org-balance)


;; test

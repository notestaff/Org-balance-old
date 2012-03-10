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
;;
;; See info node for this.
;;
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
;;      org-balance-record-
;;
;;   Reporting:
;;
;;      org-balance-done-in-range - show items completed in given date range (e.g. within last two weeks).
;;      org-balance-show-clocked-time - show things on which you worked recently
;;      org-balance-show-neglected-time - show things on which you spend less time than you want
;;      org-balance-show-non-neglected-time - show things on which you spend enough time
;;      org-balance-show-neglected-val - show things where you get less done than you want
;;

;; misc things to add:
;;   describe-structure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: external dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))
(require 'elu)
(elu-require time-date org org-clock org-agenda org-compat org-macs
	     org-archive rxx org-valu)

(rxx-start-module org-balance)
(rxx-import org-valu number-name number number-range unit valu valu-range
	    valu-ratio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: External variables referenced by org-balance module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-clock-report-include-clocking-task)
(defvar org-archive-reversed-order)

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
  "Report entries as neglected if they are neglected by at least this percentage"
  :group 'org-balance
  :type 'float)

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

(defconst org-balance-custom-commands
  (quote (("b" . "Org-balance commands")
	  ("bn" "Neglected items" tags "goal_delta_val<0/!GOAL"
	   ((org-agenda-overriding-header "Org-balance neglected items")
	    (org-agenda-sorting-strategy (quote (priority-down
						 category-keep
						 user-defined-up)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-amt-neglected))
	    (org-show-hierarchy-above (quote ((agenda . t))))))
	  ("bs" "Neglected goals in current file" tags-tree "goal_delta_val<0/!GOAL" nil)
	  ("bk" "Ok items" tags "goal_delta_val>=0/!GOAL"
	   ((org-agenda-overriding-header "Org-balance neglected items")
	    (org-agenda-sorting-strategy (quote (priority-down
						 category-keep
						 user-defined-up)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-amt-neglected))
	    (org-show-hierarchy-above (quote ((agenda . t))))))
	  ("bf" "Ok goals in current file" tags-tree "goal_delta_val>=0/!GOAL" nil)
	  ))
  "Custom agenda commands for accessing org results")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Utils
;;
;; General-purpose utility routines used in org-balance module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	  (elu-remove-list-of-text-properties (point-at-bol) (point-at-eol) (list prop)))
	(unless is-default
	  (put-text-property (point-at-bol) (point-at-eol) prop val))))))

(defun org-balance-delete-property-globally (prop)
  "Delete either text or org property globally from all entries."
    (if (stringp prop)
	(org-delete-property-globally prop)
      (elu-remove-list-of-text-properties (point-min) (point-max) (list prop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Intervals
;;
;; Code for dealing with lists of time intervals.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct org-balance-intervals
  "A set of intervals, with operations for quickly finding
intervals intersecting a given point or interval.

Currently works only for a set of same-size, contiguous, regularly spaced intervals.

Fields:

   FROM - start of first interval
   N - number of intervals
   SHIFT - distance between starts of adjacent intervals
   WIDTH - width of each interval.
"
  from n shift width)

(defun org-balance-intervals-start (intervals i)
  "Return start of I'th interval in interval set INTERVALS"
  (elu-with 'org-balance-intervals intervals (from shift n)
    (assert (and (integerp i) (<= 0 i) (< i n)))
    (+ from (* i shift))))

(defun org-balance-intervals-end (intervals i)
  "Return end of I'th interval in interval set INTERVALS"
  (+ (org-balance-intervals-start intervals i) (org-balance-intervals-width intervals)))

(defun org-balance-intervals-intersect-p (amin amax bmin bmax)
  "Test if two half-open intervals [AMIN,AMAX) and [BIN,BMAX) intersect."
  (and (< bmin amax) (< amin bmax)))

(defmacro do-org-balance-intervals-overlapping-interval (intervals pmin pmax i tstart tend &rest forms)
  "Iterate over intervals in INTERVALS which intersect the interval [pmin,pmax).   Assign interval
number to I, interval start to TSTART and interval end to TEND for each interval, then execute FORMS"
  (declare (indent 6))
  (elu-with-new-symbols
   (first-interval-idx last-interval-start last-interval-end last-interval-idx from n shift width dummy)
    `(elu-with 'org-balance-intervals intervals (,from ,n ,shift ,width)
       (when (and (> (org-balance-intervals-n intervals) 0)
		  (org-balance-intervals-intersect-p ,pmin ,pmax ,from (org-balance-intervals-end ,intervals (1- ,n))))
	 (let* ((,first-interval-idx (if (and ,shift (> ,shift 0)) (floor (/ (- ,pmin ,from) ,shift)) 0))
		(,last-interval-start (+ ,from (* ,shift (1- ,n))))
		(,last-interval-end (+ ,last-interval-start ,width))
		(,last-interval-idx (if (and ,shift (> ,shift 0))
					(- ,n (floor (/ (- ,last-interval-end ,pmax) ,shift)))
				      0))
		(,tstart (org-balance-intervals-start ,intervals ,first-interval-idx))
		(,tend (+ ,tstart ,width))
		(,i ,first-interval-idx))
	   (while (<= ,i ,last-interval-idx)
	     (when (org-balance-intervals-intersect-p ,pmin ,pmax ,tstart ,tend)
	       (let ((,tstart (max ,tstart ,pmin))
		     (,tend (min ,tend ,pmax)))
		 ,@forms))
	     (incf ,tstart ,width)
	     (incf ,tend ,width)
	     (incf ,i)))))))


(defmacro do-org-balance-intervals-containing-point (intervals p i tstart tend &rest forms)
  "Iterate over intervals in INTERVALS which contain the point p.   Assign interval
number to I, interval start to TSTART and interval end to TEND for each interval, then execute FORMS"
  `(do-org-balance-intervals-overlapping-interval ,intervals ,p ,p ,i ,tstart ,tend ,forms))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defconst org-balance-seconds-per-minute 60)
(defconst org-balance-minutes-per-hour 60)

(defun org-balance-record-done (&optional hours-ago)
  "Mark the current Org headline as DONE a specified time ago.
Useful for recording things done when you were not at the computer,
since for org-balance purposes it matters _when_ things got done.
See also `org-balance-record-time' and Info node `(org) MobileOrg'.
"
  (interactive)
  (save-excursion
    (org-back-to-heading 'invis-ok)
    (unless hours-ago
      (setq hours-ago (float (string-to-number (read-string "Finished how long ago (in hours)? " nil nil 0)))))
    (let* ((seconds-ago (* hours-ago org-balance-minutes-per-hour org-balance-seconds-per-minute))
	   (done-time (time-subtract (org-current-time) (seconds-to-time seconds-ago))))
      (flet ((org-current-time () done-time))
	(org-todo 'done)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrxx inactive-timestamp
  "An Org-mode inactive timestamp, such as '[2010-09-07 Tue 18:30]'.
Parsed as a floating-point value of time in seconds."
  (seq "[" (named-grp time (1+ nonl)) "]" )
  (org-float-time (apply 'encode-time (org-parse-time-string time))))

(defrxx clock
  "An Org-mode clock line giving a time interval, such as when
logging work time (see Info node `(org)Clocking work time').
E.g. '	CLOCK: [2010-09-09 Thu 06:30]--[2010-09-09 Thu 12:46] =>  6:16'.
Parsed as a cons of the start and end times."
  (seq bol blanks? (eval org-clock-string) blanks?
		   (inactive-timestamp from) (1+ "-")
		   (inactive-timestamp to)
		   " => " (1+ nonl))
  (cons from to))

(defun org-balance-clock-sum-add-interval (intervals totals tmin tmax)
  "For each time interval in INTERVALS that overlap [TMIN,TMAX], add the length of the overlap
to the corresponding element of the vector TOTALS.  Used by `org-balance-clock-sum'."
  (do-org-balance-intervals-overlapping-interval intervals tmin tmax i tstart tend
    (incf (aref totals i) (- tend tstart))))

(defun org-balance-clock-sum (intervals)
  "For each time interval in INTERVALS, return the total clocked time intersecting that interval, in the current file restriction.
Adapted from `org-clock-sum'"
  (let (; var: total-seconds - for each interval in INTERVALS, the total number of seconds of clocked time overlapping that interval
	;      in the current file restriction.
	(total-seconds (make-vector (org-balance-intervals-n intervals) 0)))
    ; Add the running clock, if there is one
    (when (and
	   org-clock-report-include-clocking-task
	   (equal (org-clocking-buffer) (current-buffer))
	   (<= (point-min) (marker-position org-clock-hd-marker))
	   (<= (marker-position org-clock-hd-marker) (point-max)))
      (org-balance-clock-sum-add-interval intervals total-seconds (org-float-time org-clock-start-time) (org-float-time)))
    ; Loop over the clock records in the restriction; for each, find the intervals in INTERVALS overlapping that clock record,
    ; and add the length of the overlap to the corresponding TOTAL-SECONDS entry
    (elu-save (excursion match-data)
      (goto-char (point-min))
      (rxx-do-search-fwd org-balance-clock-regexp clock-interval
	(org-balance-clock-sum-add-interval intervals total-seconds (car clock-interval) (cdr clock-interval))))
    ; Convert TOTAL-SECONDS (array of floating-point interval lengths in seconds) to a vector of org-valus with seconds as the unit,
    ; and return.
    (let ((result (make-vector (length total-seconds) nil)))
      (dotimes (i (length total-seconds) result)
	(aset result i (new-org-valu (aref total-seconds i) 'seconds))))))

(defrxx closed
  "An Org-mode line indicating when an entry was closed.
Parsed as the floating-point time."
  (sep-by blanks bol (or (eval org-closed-string) "- State \"DONE\"       from \"NEXT\"      ") (inactive-timestamp closed-time))
  ;; FIXME: support state logging.  for now, assume the default format.
  ;; so, if it goes as - State from   etc -- we take the timestamp.  but only if it was the done state!
  ;; figure out how to customize this in case the user's thing has been customized.
  closed-time)

(defun org-balance-gather-org-property (prop intervals prop-default-val)
  "For each interval in INTERVALS, for todo items in the current buffer or restriction that were
closed within that interval, compute the list of values of Org property PROP from these items.
The value of the property at an item 
Originally adapted from `org-closed-in-range'.
"
  ;; FIXOPT: if prop-default is zero then the regexp for that subtree should be, org-closed-string _and_ the prop is explicitly set _in that entry_.  (1+ (bol) (opt (not (any ?*))) (0+ nonl) (eol))
  ;; FIXME: find also state changes to DONE, or to any done state.
  (declare (special org-balance-num-warnings))
  (elu-save (excursion match-data)
    (goto-char (point-min))
    (let ((prop-occurrences (make-vector (org-balance-intervals-n intervals) nil))
	  (prop-default (concat "default_" prop)))
      (rxx-do-search-fwd org-balance-closed-regexp closed-time
	
	;; first, if closed-time is completely outside our range of intervals, just skip.
	
	;; FIXME: then, if tags matcher is given, evaluate it and only count this headline if it says true.
	
	(do-org-balance-intervals-overlapping-interval intervals closed-time closed-time i tstart tend
	  (elu-save (excursion match-data)
	    (org-back-to-heading 'invis-ok)
	    (let*
		((pos-here (point))
		 (prop-here
		  (or (org-entry-get nil prop)
		      (org-entry-get nil prop-default 'inherit)
		      prop-default-val
		      "1"))
		 (prop-valu-here
		  (condition-case err
		      (parse-org-valu prop-here)
		    (error
		     (message
		      "Warning: at line %d of file %s, could not add %s to list for goal %s; list-so-far is %s"
		      (line-number-at-pos (point)) (buffer-file-name (current-buffer)) prop-here prop prop-occurrences)
		     (incf org-balance-num-warnings)
		     nil))))
	      (when (and prop-valu-here (not (zerop (org-valu-val prop-valu-here))))
		(push (cons pos-here prop-valu-here) (aref prop-occurrences i)))))))
      prop-occurrences)))


(defun org-balance-sum-org-property (prop intervals unit prop-default-val)
  "For each interval in INTERVALS, compute the sum of values of Org property PROP from todo items closed in that interval.
The sum will be represented in unit UNIT.  The sum is computed within the current restriction, if any."
  (elu-map-vectors
   (lambda (vals)
     (org-valu-do-sum (new-org-valu 0 unit)
		      (mapcar 'cdr vals)))
   (org-balance-gather-org-property
    prop intervals prop-default-val)))


(defun org-balance-sum-property (prop intervals unit prop-default-val)
  "For each time interval in INTERVALS, compute the sum of property PROP over the current subtree
for that time interval.  The value of the property for each entry is determined as follows.  If property is the special
property 'clockedtime', then for each CLOCK line in the entry, we compute the intersection of that CLOCK line with
the interval, and the value is the duration of that intersection.  If property is the special property 'actualtime',
then the value is just the total length of the interval.  Otherwise, the property is an Org property; if the entry is
closed during the interval, we add the value of the property in that entry to the total."
  (cond ((string= prop "clockedtime")
	 (org-balance-clock-sum intervals))
	((string= prop "actualtime")
	 (make-vector (org-balance-intervals-n intervals)
		      (new-org-valu (org-balance-intervals-width intervals) 'seconds)))
	(t (org-balance-sum-org-property prop intervals unit prop-default-val))))

(defstruct
  (org-balance-archive-loc
   (:constructor
    create-org-balance-archive-loc
    (loc
     &aux
     (file (elu-not-blank (org-extract-archive-file loc)))
     (heading (elu-not-blank (org-extract-archive-heading loc))))))
  "A destination for archived trees: an org file, and a heading within that file."
  file heading)

(defrxx archive-loc
  "The archive location, specified as a property of an Org entry.
See Info node `(org) Archiving' and variable `org-archive-location'.
Parsed as the structure `org-balance-archive-loc'."
  (sep-by blanks bol ":ARCHIVE:" (named-grp loc (1+ nonl)))
  (create-org-balance-archive-loc loc))


(defun org-balance-find-all-archive-targets ()
  "Find all the places where an entry from the current subtree could have been archived."
  (elu-save (excursion window-excursion restriction)
    (let ((archive-locs (list (create-org-balance-archive-loc (org-get-local-archive-location)))))
      (org-narrow-to-subtree)
      (rxx-do-search-fwd org-balance-archive-loc-regexp loc
	(elu-add-to-list 'archive-locs loc))
      archive-locs)))


(defun org-balance-sum-property-with-archives (prop intervals unit)
  "Sum a property within the current subtree, within each of a list of specified time intervals.
Include any entries that may have been archived from the current subtree."

  ;; special-case for actualtime: do not go to archives.

  ;; if num and denom are for same subtree, go over the archives only once.
  ;; (and determine which closed items intersect this).

  (let ((prop-sum (org-balance-sum-property prop intervals unit (not 'prop-default-val)))
	(prop-default-val (org-entry-get nil (concat "default_" prop) 'inherit)))
    (unless (string= prop "actualtime")
      (let ((olpath-regexp (concat "^[ \t]+:ARCHIVE_OLPATH: " (mapconcat 'identity (org-get-outline-path) "/"))))
	(dolist (loc (org-balance-find-all-archive-targets))
	  (elu-save (excursion window-excursion restriction match-data)
	    (when (org-balance-archive-loc-file loc)
	      (find-file (org-balance-archive-loc-file loc)))
	    (widen)
	    (when (org-balance-archive-loc-heading loc)
	      (save-match-data
		(when (re-search-forward
		       (concat "^" (regexp-quote (org-balance-archive-loc-heading loc))
			       (org-re "[ \t]*\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*\\($\\|\r\\)"))
		       nil t)
		  (progn
		    (goto-char (match-beginning 0))
		    (org-narrow-to-subtree)))))
	    
	    (goto-char (if org-archive-reversed-order (point-min) (point-max)))
	    (while (funcall (if org-archive-reversed-order 're-search-forward 're-search-backward)
			    olpath-regexp (not 'bounded) 'no-error)
	      (elu-save (excursion restriction match-data)
		(org-back-to-heading 'invisible-ok)
		(setq prop-sum
		      (add-org-valu-vec
		       prop-sum
		       (org-balance-sum-property prop intervals unit
						 prop-default-val)))))))))
    prop-sum))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrxxconst org-balance-goal-todo-keyword "GOAL")

(defrxx prop-name "A valid name of an Org entry property" (& alpha (0+ (any alnum "_-"))))
(defrxx link "An Org link; we're only interested in links that point to a GOAL entry.
Parsed as the buffer position of the start of the link."
  (named-grp link (eval-regexp (rxx-make-shy org-any-link-re))) (rxx-match-beginning 'link))

(defstruct org-balance-prop
  "The name of a property to be summed for a subtree for a given time interval, and an optional restriction of
which entries to consider when summing this property.  See defrxx prop below.

Fields:

   PROP - an Org property name, 'clockedtime', or 'actualtime'.
        When summing the value of a property in a given subtree in a given time interval,
        for an Org property name we get that property's value in matching TODO entries closed
        in the time interval;
        for clockedtime we get the intersection of any clocked time in the matching entries
        with the interval; and for actualtime we get the full length of the interval.

   LINK - if not nil, follow this link to an Org entry and do the summing under that entry.
   MATCHER - if not nil, only consider Org entries matching the matcher, as specified at
      URL 'http://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties'.
"
  prop link matcher)

(defrxx matcher "A tags and properties matcher, as described at
URL 'http://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties'."
  (1+ nonl) org-make-tags-matcher)

(defrxx prop
  "The name of a property to be summed for a subtree within a given time interval,
optionally followed by a link to the subtree.
The property can be an Org property name, 'clockedtime' or 'actualtime'.  Parsed as a 
struct with fields for the property name and the link."
  (& prop-name (opt blanks "at" blanks link) (opt blanks "(" blanks? matcher blanks? ")") )
  (make-org-balance-prop :prop prop-name :link link :matcher matcher))

(defrxx ratio-word "A word separating the numerator and denominator of a fraction."
  (or "per" "every" "each" "for every" "for each" "/" "a" "in a"))

(defstruct org-balance-prop-ratio
  "The ratio of two properties: for example, hours per week.  See defrxx prop-ratio below."
  num denom)

(defrxx prop-ratio
  "The ratio of two properties.   The denominator, if not given, defaults to 'actualtime'."
  (seq (prop num) (opt blanks? ratio-word blanks? (prop denom)))
  (make-org-balance-prop-ratio :num num :denom (or denom (make-org-balance-prop :prop "actualtime"))))

(defrxx priority
  "A priority cookie on an Org headline.   Parsed as the entire cookie (e.g. [#A])."
  (seq "[#" (any upper digit) "]"))

(defrxx goal-prefix
  "The part of a GOAL line up to the goal name: the stars, the GOAL keyword, and optionally the priority. "
  (seq bol (sep-by blanks (seq (1+ "*")) (seq bow (eval org-balance-goal-todo-keyword) eow) priority?)))

(defrxx tag-name (1+ (any alnum "_@#%")))
(defrxx tags (& blanks? (opt ":" (1+ (& tag-name ":")) eol)) tag-name-list)

(defun org-balance-start-of-tags ()
  "Return position where tags start on the current headline"
  (elu-save (match-data excursion)
    (goto-char (point-at-eol))
    (rxx-search-bwd org-balance-tags-regexp (point-at-bol))
    (point)))

(defun org-balance-compute-actual-prop (prop intervals unit)
  "Computes the actual total value of the property PROP for the current subtree,
expressed in units UNIT, for each of the given INTERVALS."
  (elu-save (excursion window-excursion match-data)
    (when (org-balance-prop-link prop)
      (let ((org-link-search-must-match-exact-headline t))
	(org-open-at-point 'in-emacs)))
    (org-balance-sum-property-with-archives (org-balance-prop-prop prop) intervals unit)))

(defun org-balance-compute-actual-prop-ratio (prop-ratio intervals parsed-goal)
  "For each interval in INTERVALS, compute the ratio of the actual value of the property to the
goal value of the property."
  (elu-with 'org-balance-goal (aref parsed-goal 0) (numer-min denom)
    (let ((target-ratio (make-org-valu-ratio :num numer-min :denom denom)))
      (elu-map-vectors
       (lambda (a-num a-denom)
	 (convert-org-valu-ratio (make-org-valu-ratio :num a-num :denom a-denom) target-ratio))
       (org-balance-compute-actual-prop (org-balance-prop-ratio-num prop-ratio) intervals
					(org-valu-unit numer-min))
       (org-balance-compute-actual-prop (org-balance-prop-ratio-denom prop-ratio) intervals
					(org-valu-unit denom))))))

(defun org-balance-compute-delta (parsed-goals prop-ratio actuals)
  ""
  (elu-gen-vector i (length parsed-goals)
    (let ((parsed-goal (aref parsed-goals i))
	  (actual (aref actuals i)))
      (let* ((polarity (or (org-balance-goal-polarity parsed-goal)
			   (cdr (assoc (org-balance-prop-prop
					(org-balance-prop-ratio-num  prop-ratio))
				       org-balance-default-polarity))))
	     (dummy (assert polarity))
	     (margin (or (org-balance-goal-margin parsed-goal)
			 org-balance-default-margin-percent))
	     (goal-min (org-valu-val (org-balance-goal-numer-min parsed-goal)))
	     (goal-max (org-valu-val (org-balance-goal-numer-max parsed-goal)))
	     (range-min (- goal-min
			   (if (numberp margin)
			       (* (/ (float margin) 100.0) goal-min)
			     (org-valu-val margin))))
	     
	     (range-max (+ goal-max
			   (if (numberp margin)
			       (* (/ (float margin) 100.0) goal-max)
			     (org-valu-val margin))))
	     
	     (actual-num (org-valu-val (org-valu-ratio-num actual)))
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
	(cons delta-val delta-percent)))))


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
	      (heading
	       (save-excursion
		 (outline-up-heading 1 'invisible-ok)
		 (when (string= (upcase (org-get-heading)) "GOALS")
		   (outline-up-heading 1 'invisible-ok))
		 (org-get-heading 'no-tags 'no-todo))))
	    (put-text-property 0 1 :org-balance-delta org-balance-delta-val agenda-line)
	    (concat agenda-line "::: " heading " ::: " (number-to-string org-balance-delta-val)))))))

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

;; Struct: org-balance-goal - the user-specified goal you have for a ratio.
(defstruct org-balance-goal
  numer-min
  numer-max
  denom
  polarity
  margin
  ratio-word
  text)

(defun org-balance-scale-goal (factor goal)
  (let ((result (copy-org-balance-goal goal)))
    (setf (org-balance-goal-numer-min result)
	  (scale-org-valu factor (org-balance-goal-numer-min result)))
    (setf (org-balance-goal-numer-max result)
	  (scale-org-valu factor (org-balance-goal-numer-max result)))
    (setf (org-balance-goal-text result) (format "%.2f * (%s)" factor (org-balance-goal-text goal)))
    result))

(defrxx polarity
  (or (named-grp atmost (or "at most" "<=" "<"))
      (named-grp atleast (or "at least" ">=" ">")))
  (if atmost 'atmost 'atleast)
  "polarity")

(defrxx margin
  (seq "+-" blanks?
       (or
	(seq (number margin) blanks? "%")
	(valu margin))) margin)

(defrxx goal
  (sep-by blanks
    polarity?
    (valu-range numerator)
    ratio-word
    (valu denominator)
    margin?)
  
  (lambda (goal-str)
    (make-org-balance-goal 
     :numer-min (car numerator)
     :numer-max (cdr numerator)
     :denom denominator
     :polarity polarity
     :margin margin
     :text goal-str
     :ratio-word ratio-word))
  "value ratio goal")

(defstruct org-balance-goal-link
  "Definition of one goal in terms of another goal.  See defrxx goal-link below.
Fields:

   LINK - the link to the target goal
   ACTUAL - if nil, use the goal's target value; if non-nil, use the actual amount in the relevant
     interval.
   FACTOR - multiply the actual or target amount of the goal at link, by this factor.
 "
  factor actual link)

(defrxx goal-link (sep-by blanks (number factor) "of" (opt (named-grp actual "actual")) link)
  "Definition of one goal in terms of another goal."  
  (make-org-balance-goal-link :factor factor :actual actual :link link))

(defrxx goal-or-link
  "Definition of a goal -- either directly specified, or in terms of another goal."
  (& blanks? (| goal-link goal) blanks?)
  (if goal (make-vector (org-balance-intervals-n intervals) goal)
    (elu-save (excursion window-excursion)
      (let ((org-link-search-must-match-exact-headline t))
	(goto-char (org-balance-goal-link-link goal-link))
	(org-open-at-point 'in-emacs))
      ;; move to where the parsed-goal is.
      ;; on the other hand, for "actual", here need to call to get the ratio.
      ;; FIXME
      (unless (rxx-search-fwd org-balance-goal-prefix-regexp (point-at-eol))
	(error "No goal found at link taget"))
      (elu-map-vectors (elu-apply-partially 'org-balance-scale-goal (org-balance-goal-link-factor goal-link))
		       (rxx-parse-fwd org-balance-goal-prefix-with-spec-regexp
				      (org-balance-start-of-tags))))))



  

(defrxx goal-spec
  "A goal definition.  What follows a GOAL keyword.  Parses as ( prop-ratio . goal )
Examples:
   at least once per week  --> parsed as 
   at least three hours per week
   at most $50 a month
 "
  (& blanks? (sep-by blanks? (opt prop-ratio ":" ) goal-or-link) blanks? tags?)
  (cons
   (or prop-ratio
       (let ((goal-dim (org-valu-unit2dim (org-valu-unit (org-balance-goal-numer-min (aref goal-or-link 0))))))
	 (make-org-balance-prop-ratio
	  :num
	  (make-org-balance-prop
	   :prop 
	   (cond ((eq goal-dim 'time) "clockedtime")
		 ((eq goal-dim 'count) "done")
		 (t (error "Can only omit property name if it's clockedtime or done"))))
	  :denom (make-org-balance-prop :prop "actualtime"))))
   goal-or-link))

(defrxx goal-prefix-with-spec (& blanks? goal-spec) (cdr goal-spec))

(defun org-balance-remove-props ()
  "Remove from each GOAL in the file, any previous record of how well this goal is being met.
Usually done in preparation for generating a new record of how well each goal is being met."
  (interactive)
  (elu-save (excursion restriction)
    (widen)
    (dolist (prop '("goal_delta_val" "goal_delta_percent" "goal_updated"))
      (org-delete-property-globally prop))
    (org-map-entries '(org-toggle-tag "goal_error" 'off) "+goal_error/!GOAL" 'file)))

(defun* org-balance-compute-goal-deltas (&key intervals)
  "For each goal, determines the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period in each interval in INTERVALS.  Stores the results as properties
under each goal.
"
  (declare (special goal-update-time))
  (unless intervals (error "org-balance-compute-goal-deltas: need intervals"))
  (let ((num-errors 0)  ; number of errors (such as malformed goals) encountered
	(num-under 0)   ; number of goals where actual expenditure < goal
	(num-met 0)     ; number of goals where actual expenditure meets goal
	(num-over 0))   ; number of goals where actual expenditure > goal
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(save-match-data
	  ;; FIXOPT if goals specified, make regexp for them

	  ;; Loop over all goals defined in the org file
	  (rxx-do-search-fwd org-balance-goal-prefix-regexp nil
	    (elu-save (match-data restriction excursion)
	      (condition-case err
		  (let* (; var: goal-spec - the parsed specification of this goal, e.g. "clockedtime: one hour per week"
					;   or "done: two times per day"
			 (goal-spec (rxx-parse-fwd org-balance-goal-spec-regexp (org-balance-start-of-tags)))
			 (prop-ratio (car goal-spec))
			 (parsed-goal (cdr goal-spec)))
		    
					; Some goal specs we encounter in the org file may be malformed (since they're user-typed).
					; We tag such goal entries with the goal_error tag, so that the user can quickly find them.
					; Initially, turn this tag off since we haven't found a problem with this goal spec yet.
		    (save-match-data (org-toggle-tag "goal_error" 'off))
		    
		    ;;
		    ;; Compute the actual usage under this subtree, and convert to the same
		    ;; units as the goal, so we can compare them.
		    ;;
		    (let (delta-val-and-percent)
		      (elu-save (match-data excursion restriction)
			(outline-up-heading 1 'invisible-ok)
			(when (string= (upcase (org-get-heading)) "GOALS")
			  (outline-up-heading 1 'invisible-ok))
			(org-narrow-to-subtree)
			(goto-char (point-min))
			(setq delta-val-and-percent
			      (aref
			       (org-balance-compute-delta
				parsed-goal prop-ratio
				(org-balance-compute-actual-prop-ratio prop-ratio intervals parsed-goal))
			       0)))
		      (let ((delta-val (car delta-val-and-percent))
			    (delta-percent (cdr delta-val-and-percent)))
			(cond ((< delta-val 0) (incf num-under))
			      ((> delta-val 0) (incf num-over))
			      ((= delta-val 0) (incf num-met)))
			(org-entry-put (point) "goal_delta_val" (format "%.2f" delta-val))
			(org-entry-put (point) "goal_delta_percent" (format "%.1f" delta-percent))
			;; FIXME: include in goal_updated the period for which it was updated.
			;; (org-entry-put (point) "goal_interval" (elu-format-seconds "%Y, %T, %W, %D%z"
			;; 							       (- (org-balance-intervals-end intervals 0)
			;; 								  (org-balance-intervals-start intervals 0))))
			(org-entry-put (point) "goal_updated"
				       (format-time-string
					(org-time-stamp-format 'long 'inactive)
					(if (boundp 'goal-update-time) goal-update-time (current-time)))))))
		(error
		 (unless (string= (upcase (org-get-heading)) "GOALS")
		   (incf num-errors)
		   (message "At %s: Error processing %s : %s " (point) (buffer-substring (point) (point-at-eol))
			    err)
		   (save-match-data
		     (org-toggle-tag "goal_error" 'on)
		     (org-entry-delete nil "goal_delta_val")
		     (org-entry-delete nil "goal_delta_percent")
		     (org-entry-put (point) "goal_updated"
				    (format-time-string
				     (org-time-stamp-format 'long 'inactive)
				     (if (boundp 'goal-update-time) goal-update-time (current-time))))))
		 nil)))))))
    (message "err %d under %d met %d over %d" num-errors num-under (+ num-met num-over) num-over)))


(defun org-balance-setup ()
  "Do the initial configuration needed for org-balance"
  ;; add (type "|" "GOAL") to org-todo-keywords, if not there already
  ;; set org-log-done to at least time
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst org-balance-regtest-dir "/cvar/selection/sweep2/nsvn/Tools/org/sf/trunk/regtests/")
(defconst org-balance-regtest-defs
  '(("mythings.org" 1283015820.0 1285607849.393998 (19616 55415 443943))
    ("rt1.org" 1284760020.0 1285624025.292002 (19617 4313 292003))))

(defconst org-balance-parse-test-defs
  '((inactive-timestamp "[2010-09-28 Tue 16:11]" 1285704660.0)
    (clock "		 CLOCK: [2010-09-07 Tue 21:07]--[2010-09-08 Wed 00:07] =>  3:00" (1283908020.0 . 1283918820.0))
    (closed "			 	CLOSED: [2009-08-27 Thu 11:58]" 1251388680.0)
    (closed "		- State \"DONE\"       from \"NEXT\"       [2009-08-27 Thu 11:58]" 1251388680.0)
;    (archive-loc "	 :ARCHIVE:  %s_archive::work archive"
;		 [cl-struct-org-balance-archive-loc "/cvar/selection/sweep2/nsvn/Tools/org/sf/trunk/org-balance.el_archive"
;						    "work archive"])
    (number "three" 3)
    (number "3." 3)
    (number "3.3737" 3.3737)
    (number ".012340" 0.01234)
    (number "1e-5" 1e-5)
    (number "1.35e5" 1.35e5)
    (valu "$10.37" [cl-struct-org-valu 10.37 $])
    (valu "33" [cl-struct-org-valu 33 item])
    (valu "33 items" [cl-struct-org-valu 33 items])
    (valu "item" [cl-struct-org-valu 1 item])
    (valu "0" [cl-struct-org-valu 0 item])
    (valu "week" [cl-struct-org-valu 1 week])
    (valu-range "2-3 weeks" ([cl-struct-org-valu 2 weeks] . [cl-struct-org-valu 3 weeks]))
    (valu-range "2-3" ([cl-struct-org-valu 2 item] . [cl-struct-org-valu 3 item]))
;    (rxx-parse org-valu-range-regexp "2-3")
;    (rxx-parse org-balance-goal-regexp "at least once every two weeks +- 11%")
    (goal "once a month"
	  [cl-struct-org-balance-goal [cl-struct-org-valu 1 item] [cl-struct-org-valu 1 item]
				      [cl-struct-org-valu 1 month] nil nil "a" "once a month"])
    (goal "at least 2-3 times a week"
	  [cl-struct-org-balance-goal [cl-struct-org-valu 2 times]
				      [cl-struct-org-valu 3 times]
				      [cl-struct-org-valu 1 week] atleast nil "a" "at least 2-3 times a week"])
    (goal "at most 1200 dollars per year +- 100 dollars"
	  [cl-struct-org-balance-goal [cl-struct-org-valu 1200 dollars] [cl-struct-org-valu 1200 dollars]
				      [cl-struct-org-valu 1 year] atmost [cl-struct-org-valu 100 dollars]
				      "per" "at most 1200 dollars per year +- 100 dollars"])
    (goal "at least once every two weeks +- 11%"
	  [cl-struct-org-balance-goal [cl-struct-org-valu 1 item] [cl-struct-org-valu 1 item]
				      [cl-struct-org-valu 2 weeks] atleast 11
				      "every" "at least once every two weeks +- 11%"])))

(defun org-balance-test-parsing ()
  (interactive)
  (let ((num-ok 0))
    (dolist (parse-test org-balance-parse-test-defs)
      (message "parse-test is %s" parse-test)
      (let* (rxx-prefix
	     rxx-imports
	     (dummy (rxx-start-module org-balance))
	     (dummy (rxx-import org-valu number-name number number-range unit valu valu-range
				valu-ratio))
	     (dummy (message "prefix is %s import is %s" rxx-prefix rxx-imports))
	     (the-symbol 
			     (rxx-symbol (first parse-test)))
	     (dummy (message "the-symbol is %s" the-symbol))
	     (parse-result (rxx-parse
			    (symbol-value the-symbol)
			    (second parse-test))))
	(unless (equal parse-result
		       (third parse-test))
	  (message "failed test: %s %s" parse-test parse-result)
	  (assert nil)))
      (incf num-ok))
    (message "%d parsing tests ok" num-ok)))

(defun org-balance-regtests ()
  (interactive)
  (message "====================== org-balance regtests ===========================")
  (org-balance-test-parsing)
  (elu-save (excursion window-excursion restriction match-data)
    (let ((num-ok 0) (num-failed 0))
      (dolist (regtest org-balance-regtest-defs)
	(let* ((test-file (concat org-balance-regtest-dir (first regtest)))
	       (ref-file (concat (file-name-sans-extension test-file) "_ref.org")))
	  (if (not (and (file-readable-p test-file) (file-readable-p ref-file)))
	      (progn
		(incf num-failed)
		(message "Could not read test file %s or reference file %s" test-file ref-file))
	    (find-file test-file)
	    (widen)
	    (goto-char (point-min))
	    (org-mode)
	    (org-balance-remove-props)
	    (let ((goal-update-time (fourth regtest)))
	      (org-balance-compute-goal-deltas
	       :intervals (make-org-balance-intervals :from (second regtest) :n 1 :width
						      (- (third regtest) (second regtest)) :shift 0)))
	    (save-buffer)
	    (if (zerop (call-process "diff" (not 'infile) (not 'destination) (not 'display)
				     "-bBE" test-file ref-file))
		(progn
		  (incf num-ok)
		  (kill-buffer (current-buffer)))
	      (incf num-failed)
	      (message "Failed test on %s" test-file)
	      (show-all)
	      (find-file ref-file)
	      (show-all)
	      (find-file test-file)
	      (ediff-files test-file ref-file))))
	(message "%s tests ok, %s tests failed" num-ok num-failed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(rxx-end-module org-balance)

(provide 'org-balance)

;;; org-balance.el ends here

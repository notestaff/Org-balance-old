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
(eval-when-compile (require 'cl))
(require 'rx)

(defvar org-clock-report-include-clocking-task)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: org-balance customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup org-balance nil
  "Options for the org-balance package"
  :tag "Org Balance"
  :group 'org)

(defcustom org-balance-min-neglect-percent 5
  "Report entries as neglected if they are neglected by at least percentage"
  :group 'org-balance
  :type 'float)

(defcustom org-balance-default-interval "1 week"
  "By default, report goal compliance averaged over this time interval
before today."
  :group 'org-balance
  :type 'string)   ;; todo: add a validation function to make sure this can be parsed as a duration.

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

(defun org-balance-put-overlay (txt &optional level)
  "Put an overlay on the current line, displaying TIME.
If LEVEL is given, prefix time with a corresponding number of stars.
This creates a new overlay and stores it in `org-balance-overlays', so that it
will be easy to remove."
  (let* ((c 60)    ;; column in which the overlay starts 
	 (off 0)
	 ov tx)
    (org-move-to-column c)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (setq ov (make-overlay (1- (point)) (point-at-eol))
	  tx (concat (buffer-substring (1- (point)) (point))
		     (make-string (+ off (max 0 (- c (current-column)))) ?.)
		     (org-add-props txt
			 (list 'face 'org-clock-overlay))
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

(defun org-balance-rx-numbered-group (form)
  "Extend the rx macro and rx-to-string function to support numbered groups
when building regexps. Parse and produce code from FORM, which is `(org-balance-numbered-group group-number ...)'."
  (concat "\\(?"
	  (number-to-string (second form))
	  ":"
	  ;; Several sub-forms implicitly concatenated.
	  (mapconcat (lambda (re) (rx-form re ':)) (cddr form) nil)
          "\\)"))

(unless (assoc 'org-balance-numbered-group rx-constituents)
  (push '(org-balance-numbered-group . (org-balance-rx-numbered-group 1 nil)) rx-constituents))

(defun org-balance-full-match (re s)
  "Do a string match, but fail unless the regexp matches the full string"
  (and (string-match re s)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length s))))

(defconst org-balance-re-next-group-num 10)
(defun org-balance-re-get-group-num ()
  "Create a unique number to be used for a particular numbered
group within a particular regexp."  
  (incf org-balance-re-next-group-num))

(defmacro org-balance-re-make-group-names (&rest names)
  "Declare the specified variables as constants and assign them
unique integer ids, for use as ids of group numbers in numbered
groups in regexps." 
  (cons
   'progn
   (mapcar
    (lambda (name)
      (let* ((name-str (symbol-name name)))
	`(defconst ,name (org-balance-re-get-group-num))))
    names)))

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

(defun org-balance-sum-property (prop unit tstart tend)
  "Fast summing of property.  Returns the sum of the property under the current restriction.

Originally adapted from `org-closed-in-range'.
"

  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((prop-sum 0)
	    (prop-default (concat "default_" prop)))
	(while (re-search-forward (concat "^[ \t]*" org-closed-string " +\\[\\(.*?\\)\\]") nil t)
	  (let ((closed-time (org-float-time (apply 'encode-time (org-parse-time-string (match-string 1))))))
	    (when (and (<= tstart closed-time) (<= closed-time tend))
	      (save-excursion
		(save-match-data
		  (org-back-to-heading 'invis-ok)
		  (let ((prop-here
			 (or (org-entry-get nil prop)
			     (org-entry-get nil prop-default 'inherit)
			     "1")))
		    (setq prop-sum
			  (+ prop-sum
			     (condition-case err
				 (org-balance-valu-val
				  (org-balance-convert-valu
				   (org-balance-parse-valu prop-here)
				    unit))
				  (error
				   (message
				    "Warning: at line %d of file %s, could not add %s to sum for goal %s; sum-so-far is %s unit is %s"
					    (line-number-at-pos (point)) (buffer-file-name (current-buffer))
					    prop-here prop prop-sum unit)
				   (setq org-balance-num-warnings (1+ org-balance-num-warnings))
				   0))))))))))
		
	prop-sum))))

(defun org-balance-record-time (&optional hours ago)
  "Record the given amount of time as time spent under the current org entry.
Useful when you did some task but weren't near a computer to start/stop the org timer
as you were doing it.
"
  (interactive)
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

(defconst org-balance-minutes-per-day (* 60 24))


(defvar org-balance-goal-prefix "goal_")


(defun org-balance-clock-sum (tstart tend)
  "Return the total clock time in the current file restriction. Adapted from `org-clock-sum'"
  (let ((total-minutes
	 (if (and
	      org-clock-report-include-clocking-task
	      (equal (org-clocking-buffer) (current-buffer))
	      (<= (point-min) (marker-position org-clock-hd-marker))
	      (<= (marker-position org-clock-hd-marker) (point-max)))
	     (let* ((cs (org-float-time org-clock-start-time))
		    (ts (max tstart cs))
		    (te (min tend (org-float-time)))
		    (dt (- te ts)))
	       (message "tstart is %s tend is %s ts is %s te is %s dt is %s"
			(- tstart cs) (- tend cs) (- ts cs) (- te cs) dt)
	       (if (> dt 0) (floor (/ dt 60)) 0))
	   0)))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(while (re-search-forward
		(concat "^[ \t]*" org-clock-string
			"[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)")
		nil t)
	  (cond
	   ((match-end 1)
	    ;; Two time stamps
	    (setq ts (match-string 1)
		  te (match-string 2)
		  ts (org-float-time
		      (apply 'encode-time (org-parse-time-string ts)))
		  te (org-float-time
		      (apply 'encode-time (org-parse-time-string te)))
		  ts (if tstart (max ts tstart) ts)
		  te (if tend (min te tend) te)
		  dt (- te ts)
		  total-minutes (if (> dt 0) (+ total-minutes (floor (/ dt 60))) total-minutes)))
	   ((match-end 3)
	    ;; A naked time
	    (setq total-minutes (+ total-minutes (string-to-number (match-string 4))
			(* 60 (string-to-number (match-string 3))))))))))
    total-minutes))
    

(defun org-balance-read-date (s)
  "Parse a date/time point.   The point is always in the past, since the user is entering a past interval
during which they want to measure their adherence to their goals.  This differs from `org-read-date', which assumes
its input is in the future (as when entering a future appointment).  So we interpret the input here differently.

A time interval such as '3h' is assumed to go from 3h back up to the present.

We will also have a default interval, which can be overriden (or used?) with the prefix argument.
"

  ; temporarily change org-read-date-analyze to additionally recognize relative time strings such as
  ; -3h or -03:04 relative to current time.
  ;
  
  (save-match-data
    (when (string-match (concat "\\(" org-balance-number-regexp "\\)" "[hH]") s)
      (org-balance-string-to-number (match-string 1 s))
    )
  ))

(defun* org-balance-compute-goal-deltas (&key goals tstart tend callback error-handler)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))
  (message "interval is %s" (- tend tstart))

  (let ((days-in-interval (org-balance-make-valu (/ (float (- tend tstart)) 60.0 60.0 24.0) 'days)))
    (save-excursion
      (save-restriction
	(save-match-data
	  (let ((org-balance-num-warnings 0))
	    
	    ;;
	    ;; Find all entries where one of our goals is set.
	    ;; (if goals not specified, find all entries where _some_ goal is set).
	    ;; possibly, make a regexp for parsing goals, and match for that.  though, then,
	    ;; we won't find the malformed goals and won't be able to warn about them.
	    ;;
	    
					;
					; loop over agenda files if needed
					;
	    
	    (goto-char (point-min))
	    (while (re-search-forward
		    (org-re (concat "^[ \t]*:"
				    (if goals
					(regexp-opt
					 (mapcar
					  (lambda (goal)
					    (concat org-balance-goal-prefix goal)) goals) 'words)
				      "\\(goal_.+\\)")
				    ":[ \t]*\\(\\S-.*\\)?")) nil t)
	      ;; here we check that this goal is within a correct properties buffer, is not archived,
	      ;; and that any other restrictions of this search (such as priority) are respected.
	      ;; (e.g. using looking-back-at)
	      ;; then:
	      
	      (let* ((goal-name-here (org-match-string-no-properties 1))
		     (goal-def-here (org-match-string-no-properties 2))
		     (parsed-goal
		      (condition-case err
			  (org-balance-parse-valu-ratio-goal goal-def-here)
			(error
			 (incf org-balance-num-warnings)
			 (message "Error parsing %s" goal-def-here)
			 nil))))
		(when parsed-goal
		  ;;
		  ;; Compute the actual usage under this subtree, and convert to the same
		  ;; units as the goal, so we can compare them.
		  ;;
		  (save-match-data
		    (save-excursion
		      (save-restriction
			(org-narrow-to-subtree)
			(goto-char (point-min))
			(let* ((is-time (equal goal-name-here (concat org-balance-goal-prefix "clockedtime")))
			       (sum-here
				(if is-time (org-balance-clock-sum tstart tend)
				  (org-balance-sum-property
				   goal-name-here
				   (org-balance-valu-unit
				    (org-balance-valu-ratio-goal-num-min parsed-goal))
				   tstart tend))))
					; we only really need to set the text for entries where there is a goal.
					; but is it faster just to set it?
			  (let ((actual
				 (org-balance-convert-valu-ratio
				  (make-org-balance-valu-ratio
				   :num
				   (org-balance-make-valu sum-here
							  (if is-time 'minutes
							    (org-balance-valu-unit (org-balance-valu-ratio-goal-num-min parsed-goal))))
				   :denom days-in-interval)
				  (make-org-balance-valu-ratio
				   :num (org-balance-valu-ratio-goal-num-min parsed-goal)
				   :denom (org-balance-valu-ratio-goal-denom parsed-goal)))))
			    (message "goal %s actual %s" goal-def-here actual)

			    
			    
			    ))))))))
	    (when (> org-balance-num-warnings 0)
	      (message "There were %d warnings; check the *Messages* buffer." org-balance-num-warnings))))))))

(defun try-goals ()
  (interactive)
  (org-balance-compute-goal-deltas))


(defun org-balance-show-neglected-time (&optional tstart tend)
  (interactive)
  (org-balance-reset-overlays)
  (let (org-balance-result-list)
    (message "got %d"
	     (length (delq nil 
			   (org-balance-compute-goal-deltas
			    :goal "time" :tstart tstart :tend tend
			    :callback
			    (lambda (goal-delta total-here per-day-here per-day-goal-here)
			       (when 
				 (org-show-context)
				 (org-balance-put-overlay (format "goal-delta %s total %s per-day %s per-day-goal %s"
								  goal-delta total-here per-day-here per-day-goal-here)))
			       t)
			    :error-handler
			    (lambda (err)
			      (org-show-context)
			      (org-balance-put-overlay (format "Error: %s" (error-message-string err))))
			      
			    ))))))


(defun org-balance-show-neglected-val (&optional tstart tend)
  (interactive)
  (org-balance-reset-overlays)
  (org-balance-compute-goal-deltas :goal "done" :tstart tstart :tend tend
				   :callback
				   (lambda (goal-delta total-here per-day-here per-day-goal-here)
				     (when (< goal-delta 0)
				       (org-show-context)
				       (org-balance-put-overlay (format "goal-delta %s total %s per-day %s per-day-goal %s" goal-delta total-here per-day-here per-day-goal-here))))))


(defun org-balance-show-non-neglected-val (&optional tstart tend)
  (interactive)
  (org-balance-reset-overlays)
  (org-balance-compute-goal-deltas :goal "done" :tstart tstart :tend tend
				   :callback
				   (lambda (goal-delta total-here per-day-here per-day-goal-here)
				     (when (>= goal-delta 0)
				       (org-show-context)
				       (org-balance-put-overlay
					(format "goal-delta %s total %s per-day %s per-day-goal %s" goal-delta total-here per-day-here per-day-goal-here))))))



(defun org-balance-show-non-neglected-time (&optional tstart tend)
  (interactive)
  (org-balance-check-goals :goal "time" :tstart tstart :tend tend
			   :neglect-tolerance 30 :show-if-too-much-p t :show-if-just-right t))

(defun org-balance-show-clocked-time (&optional tstart tend)
  "Show entries that have at least some clocked time in the given interval"
  (interactive)

  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Start date: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))
  
  (org-clock-sum tstart tend)
  (message "finished clock sum")

  (org-overview)
  (org-balance-remove-overlays)
  (when org-remove-highlights-with-change
    (org-add-hook 'before-change-functions 'org-balance-remove-overlays
		  nil 'local))
  
  (org-map-entries
   (lambda ()
     (let* ((total-minutes-here (get-text-property (point) :org-clock-minutes)))
       (when (and total-minutes-here (> total-minutes-here 0))
	 (org-show-context)
	 (org-balance-put-overlay (format "tot: %s" (org-minutes-to-hh:mm-string (round total-minutes-here))))
	 )))))


(defun org-balance-show-val (&optional tstart tend)
  "Show entries that have at least some clocked time in the given interval"
  (interactive)

  (when (not tstart) (setq tstart (org-float-time (org-read-date nil 'to-time nil "Start date: "))))
  (when (not tend) (setq tend (org-float-time (org-current-time))))
  
  (org-balance-sum-property "val" :val-sum 1 tstart tend)
  (message "finished clock sum")

  (org-overview)
  (org-balance-remove-overlays)
  (when org-remove-highlights-with-change
    (org-add-hook 'before-change-functions 'org-balance-remove-overlays
		  nil 'local))
  
  (org-map-entries
   (lambda ()
     (let* ((total-minutes-here (get-text-property (point) :val-sum)))
       (when (and total-minutes-here (> total-minutes-here 0))
	 (org-show-context)
	 (org-balance-put-overlay (format "tot: %.1f" total-minutes-here))
	 )))))


(defmacro org-balance-replacing-function (fname new-func body)
  ;; Execute code, temporarily replacing a given function with a new one
  `(let ((old-func (symbol-function ,fname)))
     (unwind-protect
	 (progn
	   (fset ,fname ,new-func)
	   ,body)
       (fset ,fname old-func))))

(defsubst org-balance-cmp (a b)
  "Compare agenda entries by the amount of neglect, with the most-neglected at the top."
  (let ((pa (or (get-text-property 0 :org-balance-neglect a) 0))
	(pb (or (get-text-property 0 :org-balance-neglect b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))

(defun org-balance-save-amt-neglected (agenda-line)
  "Given an agenda line, save the 'neglect amount' value of the corresponding org entry
as a text property, for later use by org-balance-cmp.  Also, add the neglect amount value
to the agenda line.
"
  (let ((orig-entry (get-text-property 0 'org-hd-marker agenda-line)) cur-buf)
    (if orig-entry
	(save-excursion
	  (setq cur-buf (current-buffer))
	  (switch-to-buffer (marker-buffer orig-entry))
	  (goto-char (marker-position orig-entry))
	  (let ((org-balance-neglect-val (get-text-property (point) :org-balance-neglect)))
	    (if (and org-balance-neglect-val (> org-balance-neglect-val 0.01)) 
		(progn
		  (put-text-property 0 1 :org-balance-neglect org-balance-neglect-val agenda-line)
		  (let ((r (concat agenda-line "::: " (number-to-string org-balance-neglect-val))))
		    r))
	      nil)))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: User interaction
;;
;; Interactive commands callable by the user
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-balance-set-goal ()
  "Set a goal property on the current entry.
Offer completion for the goal.  Optionally show help screen with goal examples.
"
  (interactive)
  )

(defun org-balance-check-goals ()
  "Run a check of compliance with goals."
  (interactive)
  )


(defun* org-balance-dispatcher-api (&key (action 'tree) tstart tend (goal "time")
					 (goal-delta-relative-p t) (goal-delta-max .05)
					 (show-goals-under t) show-goals-over show-goals-within
					 )
  "Top-level dispatcher for the org-balance package: all user functions can be invoked by calling this routine with
appropriate parameters.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  (cond ((eq action 'tree)
	 (org-balance-reset-overlays)
	 (org-balance-compute-goal-deltas
	  :goal goal :goal-delta-relative-p goal-delta-relative-p :tstart tstart :tend tend
	  :callback
	  (lambda (goal-delta total-here per-day-here per-day-goal-here per-day-here-in-their-units
			      per-day-goal-in-their-units their-units)
	    (when (or (and show-goals-under (< goal-delta (- goal-delta-max)))
		      (and show-goals-over (> goal-delta goal-delta-max))
		      (and show-goals-within (<= (- goal-delta-max) goal-delta goal-delta-max)))
	      (org-show-context)
	      (org-balance-put-overlay (format "%s%d%%: %.2f (not %s) %s"
					       (if (>= goal-delta 0) "+" "")
					       (round (* goal-delta 100.0)) 
					       per-day-here-in-their-units per-day-goal-in-their-units
					       their-units
					       ))))
	  :error-handler
	  (lambda (err)
	    (org-show-context)
	    (org-balance-put-overlay (format "Error: %s" (error-message-string err))))	  
	  ))))


(defun orgb-test ()
  (interactive)
  (let ((displayed-month 8)
	(displayed-year 2010))
    (org-balance-dispatcher-api :show-goals-within nil :show-goals-over nil )
  ))


(defun org-balance-make-agenda-custom-commands ()
       '(("b" "org-balance: neglected items" tags "val_goal>=0"
	  ((org-agenda-sorting-strategy '(user-defined-down))
	   (org-agenda-cmp-user-defined 'org-balance-cmp)
	   (org-agenda-before-sorting-filter-function 'org-balance-save-amt-neglected)))))

(setq org-agenda-custom-commands (org-balance-make-agenda-custom-commands))
	
(require 'widget)

(eval-when-compile
  (require 'wid-edit)
  (require 'cl)
  (require 'cl-19))

(defun org-balance-hide-menu ()
  "Hides the org-balance menu"
  (interactive)
  (org-unhighlight)
  (when (string= (buffer-name (current-buffer))
		 "*Org-Balance Menu*")
    (org-balance-remove-overlays)
    (kill-buffer (current-buffer))
    (delete-window)
    ))



(defun org-balance-menu ()
  "Show a menu of org-balance functions"
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "This function only works in org-mode buffers")
    (let* ((orig-window-config (current-window-configuration))
	   (restore-func `(lambda (&rest ignore)
			    (org-balance-hide-menu)
			    (set-window-configuration ,orig-window-config)
			    (org-unhighlight))))
      (delete-other-windows)
      (let ((org-buf (current-buffer))
	    (org-window (selected-window)))
	(org-highlight (point-at-bol) (point-at-eol))
	(recenter)
	(split-window)
	(switch-to-buffer "*Org-Balance Menu*")
	(kill-all-local-variables)
	(let ((inhibit-read-only t))
	  (erase-buffer))
	(let ((all (overlay-lists)))
	  ;; Delete all the overlays.
	  (mapcar 'delete-overlay (car all))
	  (mapcar 'delete-overlay (cdr all)))
	(widget-insert "Choose an org-balance command:\n\n")
	(widget-create 'push-button
		       :notify `(lambda (&rest ignore)
				  (delete-other-windows)
				  (switch-to-buffer ,org-buf)
				  (org-balance-record-time)
				  (,restore-func))
		       "Record time")
	(widget-insert "\n")
	(widget-create 'push-button
		       :notify `(lambda (&rest ignore)
				  (delete-other-windows)
				  (switch-to-buffer ,org-buf)
				  (org-balance-record-time)
				  (,restore-func))
		       "Record completion")


	(widget-insert "\n")
	(widget-insert "-------------------------\n")

	(widget-create 'push-button
		       :notify `(lambda (&rest ignore)
				  (delete-other-windows)
				  (switch-to-buffer ,org-buf)
				  (org-tags-view nil val_)
				  (,restore-func))
		       "Show agenda time")
	
	(widget-create 'push-button
		       :notify `(lambda (&rest ignore)
				  (delete-other-windows)
				  (switch-to-buffer ,org-buf)
				  (org-balance-show-neglected-time)
				  (,restore-func))
		       "Show neglected time")
	(widget-insert "\nHow long ago?   ")
	(widget-create 'editable-field :size 5 "-3w")
	
	(use-local-map widget-keymap)
	(widget-setup)
	(set-buffer-modified-p nil)
	(local-set-key "q"
		       `(lambda ()
			  (interactive)
			  (,restore-func)
			  (org-balance-hide-menu)))
	
	))))

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


(defconst org-balance-unit2minutes
  `((second . 0.0166666666667) (seconds . 0.0166666666667) (minute . 1) (minutes . 1)
    (min . 1) (mins . 1)
    (hour . 60) (hours . 60) (hr . 60) (hrs . 60) (day . ,org-balance-minutes-per-day)
    (days . ,org-balance-minutes-per-day) (week . 10080) (weeks . 10080)
    (month . 43200) (months . 43200) (year . 525600) (years . 525600)
    (bluemoon 1e12))
 "Number of minutes in each unit" )

(defconst org-balance-units
  '((time . ((second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
	     (workweek . 7200)
	     (month . 43200) (year . 525600) (bluemoon 1e12)))
    (money . ((dollar . 1) (cent . .01) (k . 1000)))
    (count . ((item . 1) (time . 1)))))

;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defconst org-balance-units
      (mapcar
       (lambda (dimension-info)
	 (cons (car dimension-info)
	       (reduce 'append
		       (mapcar
			(lambda (unit-info)
			  (list unit-info (cons (intern (concat (symbol-name (car unit-info)) "s")) (cdr unit-info))))
			(cdr dimension-info)))))
       org-balance-units))

;; var: org-balance-unit2dim-alist - assoc list mapping each unit to its dimension (time, money, count, ...)
(defconst org-balance-unit2dim-alist
  (reduce 'append
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
(defconst org-balance-unit2base-alist (reduce 'append (mapcar 'cdr org-balance-units)))

(defun org-balance-unit2base (unit)
  "Return the number of base units in the given unit.  For each dimension we have a base unit in terms of which all other
units are measured; for example, for time we use minutes."
  (org-balance-assoc-val unit org-balance-unit2base-alist))

;; Struct: org-balance-valu - a value together with a given unit of measurement, e.g., 5 hours. 
(defstruct (org-balance-valu
	    (:constructor nil)
	    (:constructor new-org-balance-valu
			  (val unit-name
			       &aux (unit
				     (if (stringp unit-name) (intern unit-name) unit-name)))))
	    val unit)

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

(defconst org-balance-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(defconst org-balance-number-regexp
  (rx-to-string
   `(seq
     (zero-or-more whitespace)
     
     (or
      ;; either an english number name
      ,(cons 'or (mapcar (lambda (number-info) (symbol-name (car number-info))) org-balance-number-names))
      
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
     
     (zero-or-more whitespace)))
   "Regular expression for a floating-point number")
			

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
    (if (org-balance-is-valid-number-p s)
	(or (org-balance-assoc-val (org-balance-trim-whitespace s)
				   org-balance-number-names 'nil-ok)
	    (string-to-number s))
      (error "Could not parse as number: %s" s))))

(defalias 'org-balance-parse-number 'org-balance-string-to-number)

(org-balance-re-make-group-names
 org-balance-number-range-start org-balance-number-range-end)

(defconst org-balance-number-range-regexp
  (rx-to-string
   `(seq
     (org-balance-numbered-group
      ,org-balance-number-range-start
      (regexp ,org-balance-number-regexp))
     (optional
      "-"
      (org-balance-numbered-group
       ,org-balance-number-range-end
       (regexp ,org-balance-number-regexp))))))

(defun org-balance-parse-number-range (s)
  "Parse a range of numbers, such as 1-2.  A single number N is parsed as the range
N-N."
  (save-match-data
    (if (org-balance-full-match org-balance-number-range-regexp s)
	(cons (org-balance-parse-number (match-string org-balance-number-range-start s))
	      (org-balance-parse-number
	       (or (match-string org-balance-number-range-end s)
		   (match-string org-balance-number-range-start s))))
      (error "Could not parse %s as number range" s))))

(defvar org-balance-parse-valu-hooks nil
  "List of hooks for parsing valu strings (value with units), such as `5 hours'.  Can be used e.g. to parse currency
such as $5 into the canonical form `5 dollars'.  Each hook must take a string as an argument and return either an
`org-balance-valu' struct if it successfully parsed the string, or nil if it didn't.")

(defvar org-balance-unit-regexp
  (regexp-opt (mapcar (lambda (unit-info) (symbol-name (car unit-info))) org-balance-unit2dim-alist)))


(org-balance-re-make-group-names
 org-balance-re-valu-number
 org-balance-re-valu-unit)

(defconst
  org-balance-valu-regexp
  (rx-to-string
   ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   `(or (seq (optional
	      (org-balance-numbered-group
	       ,org-balance-re-valu-number (regexp ,org-balance-number-regexp)))
	     (org-balance-numbered-group
	      ,org-balance-re-valu-unit (regexp ,org-balance-unit-regexp)))
	(seq (org-balance-numbered-group
	      ,org-balance-re-valu-number (regexp ,org-balance-number-regexp))
	     (optional
	      (org-balance-numbered-group
	       ,org-balance-re-valu-unit (regexp ,org-balance-unit-regexp)))))))

  
(defun org-balance-parse-valu (valu-str)
  "Given a string representing a value with units, parse it into an org-balance-valu structure."
  (or
   (run-hook-with-args-until-success 'org-balance-parse-valu-hooks valu-str)

   (save-match-data
     (if (org-balance-full-match org-balance-valu-regexp valu-str)
	 (org-balance-make-valu (org-balance-string-to-number
				 (or (match-string org-balance-re-valu-number valu-str) "1"))
				(or (match-string org-balance-re-valu-unit valu-str) "item"))
       (error "Could not parse %s as a value with a unit" valu-str)))))


(org-balance-re-make-group-names
 org-balance-valu-range-grp-range
 org-balance-valu-range-grp-unit)

(defconst
  org-balance-valu-range-regexp
  (rx-to-string
   ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   `(or (seq (optional
	      (seq
	       (org-balance-numbered-group
		,org-balance-valu-range-grp-range
		(regexp ,org-balance-number-range-regexp))
	       (one-or-more whitespace)))
	     (org-balance-numbered-group
	      ,org-balance-valu-range-grp-unit (regexp ,org-balance-unit-regexp)))
	(seq (org-balance-numbered-group
	      ,org-balance-valu-range-grp-range (regexp ,org-balance-number-range-regexp))
	     (optional
	      (seq
	       (one-or-more whitespace)
	       (org-balance-numbered-group
		,org-balance-valu-range-grp-unit (regexp ,org-balance-unit-regexp))))))))

(defun org-balance-parse-valu-range (valu-str)
  "Given a string representing a value range with units, parse it into an org-balance-valu structure."
  (or
   (run-hook-with-args-until-success 'org-balance-parse-valu-hooks valu-str)

   (save-match-data
     (if (org-balance-full-match org-balance-valu-range-regexp valu-str)
	 (let ((number-range (org-balance-parse-number-range
			      (or (match-string org-balance-valu-range-grp-range valu-str) "1")))
	       (unit (or (match-string org-balance-valu-range-grp-unit valu-str) "item")))
	   (cons (org-balance-make-valu (car number-range) unit)
		 (org-balance-make-valu (cdr number-range) unit)))
       (error "Could not parse %s as a value range with a unit" valu-str)))))


(defconst org-balance-ratio-words (regexp-opt (list "per" "every" "each" "/" "a" "in a")))

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


;; Struct: org-balance-valu-ratio-goal - the user-specified goal you have for a ratio.
(defstruct org-balance-valu-ratio-goal
  num-min
  num-max
  denom
  polarity
  margin
  text)

(org-balance-re-make-group-names
 org-balance-valu-ratio-goal-grp-polarity
 org-balance-valu-ratio-goal-grp-num
 org-balance-valu-ratio-goal-grp-denom
 org-balance-valu-ratio-goal-grp-margin-percent
 org-balance-valu-ratio-goal-grp-margin-val) 

(defconst org-balance-valu-ratio-goal-regexp
  (rx-to-string
   `(seq
     (optional
      (org-balance-numbered-group
       ,org-balance-valu-ratio-goal-grp-polarity
       (seq
	(seq (or "at most" "at least"))
	(one-or-more whitespace))))
      
     (org-balance-numbered-group
      ,org-balance-valu-ratio-goal-grp-num (regexp ,org-balance-valu-range-regexp))
     (one-or-more whitespace)
     (regexp ,org-balance-ratio-words)
     (one-or-more whitespace)
     (org-balance-numbered-group
      ,org-balance-valu-ratio-goal-grp-denom
      (regexp ,org-balance-valu-regexp))
     (optional
      (seq
       (one-or-more whitespace)
       "+-"
       (zero-or-more whitespace)
       (seq
	(or
	 (seq (org-balance-numbered-group ,org-balance-valu-ratio-goal-grp-margin-percent
					  (regexp ,org-balance-number-regexp))
	      (zero-or-more whitespace)
	      "%")
	 (org-balance-numbered-group
	  ,org-balance-valu-ratio-goal-grp-margin-val
	  (regexp ,org-balance-valu-regexp)))))))))
  
(defun org-balance-parse-valu-ratio-goal (goal-str)
  "Parse a string representing a valu-ratio goal.

The syntax is: [at most]

"
  (save-match-data
    (if (org-balance-full-match org-balance-valu-ratio-goal-regexp goal-str)
	(let ((valu-range (org-balance-parse-valu-range
			   (match-string org-balance-valu-ratio-goal-grp-num goal-str))))
	  (make-org-balance-valu-ratio-goal 
	   :num-min (car valu-range)
	   :num-max (cdr valu-range)
	   :denom (org-balance-parse-valu (match-string org-balance-valu-ratio-goal-grp-denom goal-str))
	   :polarity (match-string org-balance-valu-ratio-goal-grp-polarity goal-str)
	   :margin (or (and (match-string org-balance-valu-ratio-goal-grp-margin-percent goal-str)
			    (org-balance-parse-number
			     (match-string org-balance-valu-ratio-goal-grp-margin-percent goal-str)))
		       (and (match-string org-balance-valu-ratio-goal-grp-margin-val goal-str)
			    (org-balance-parse-valu (match-string org-balance-valu-ratio-goal-grp-margin-val goal-str))))
	   :text goal-str))
      (error "Could not parse %s as a valu ratio goal" goal-str))))


(defun org-balance-parse-duration-into-minutes (duration-str)
  (condition-case err
      (let* ((duration-list (if (stringp duration-str) (split-string duration-str) duration-str))
	     (unit-pos (1- (length duration-list)))
	     (unit (nth unit-pos duration-list))
	     (minutes-in-unit (assoc-val (intern unit) org-balance-unit2minutes))
	     (num-units (if (= unit-pos 0) 1 (string-to-number (first duration-list))))
	     (duration-in-minutes (float (* num-units minutes-in-unit))))
	`((duration-in-minutes . ,duration-in-minutes) (minutes-in-unit . ,minutes-in-unit) (unit . ,unit)))
    (error (signal 'org-balance-parse-error (list duration-str err)))))


(defun org-balance-parse-time-fraction (fraction-str)
  (let* ((parts (split-string fraction-str (regexp-opt (list " per " " every " " each " " / " " a " " in a ")))))
    (/ (assoc-val 'duration-in-minutes (org-balance-parse-duration-into-minutes (first parts)))
       (assoc-val 'duration-in-minutes (org-balance-parse-duration-into-minutes (second parts))))))


(defun org-balance-report-time-fraction (fraction-str minutes-per-day)
  "Given minutes per day, report the equivalent time fraction in the original units"
  (let* ((parts (split-string fraction-str (regexp-opt (list " per " " every " " each " " / " " a " " in a "))))
	 (minutes-in-their-denom (assoc-val 'duration-in-minutes (org-balance-parse-duration-into-minutes (second parts))))
	 (days-in-their-denom (/ minutes-in-their-denom org-balance-minutes-per-day))
	 (parse-their-num (org-balance-parse-duration-into-minutes (first parts)))
	 (minutes-per-their-num-unit (assoc-val 'minutes-in-unit parse-their-num))
	 (fraction-in-their-units (/ (* minutes-per-day days-in-their-denom) minutes-per-their-num-unit))
	 )
    fraction-in-their-units))
    
(defun org-balance-parse-count (s)
  (let ((known-words '((once . 1) (twice . 2) (thrice . 3))))
    (cond ((assoc (intern s) known-words) (assoc-val (intern s) known-words))
	  ((string-match "\\([[:digit:]]+\\) ?\\([xX]\\|times?\\)?" s) (string-to-number (match-string 1 s))))))

(defun org-balance-parse-frequency-per-day (fraction-str)
  (let* ((parts (split-string fraction-str (regexp-opt (list " per " " every " " each " " / " " a ")))))
    (* org-balance-minutes-per-day (/ (float (org-balance-parse-count (first parts)))
	       (assoc-val 'duration-in-minutes (org-balance-parse-duration-into-minutes (second parts)))))))

(provide 'org-balance)


;; test

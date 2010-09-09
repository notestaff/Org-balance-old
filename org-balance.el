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
(eval-when-compile (require 'cl))
(require 'rx)
(require 'rxx)

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
(defgroup org-balance-faces nil
  "Faces in org-balance"
  :tag "Org-balance faces"
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

(defmacro dbg (&rest exprs)
  "Print the values of exprs, so you can write e.g. (dbg a b) to print 'a=1 b=2'.
Returns the value of the last expression."
  `(let ((expr-vals (list ,@exprs)))
     (apply 'message
	    (append (list ,(mapconcat (lambda (expr)
					(concat (format "%s" expr) "=%s "))
				      exprs
				      ""))
		    expr-vals))
     (car-safe (last expr-vals))))

(defun org-balance-groupby (z key-func)
  "Group items in a list by their key, using the specified key extractor.
Return an a-list mapping keys to items with that key.  "
  (dbg z)
  (setq z (copy-sequence z))
  (setq z (sort z (lambda (x y) (< (funcall key-func x) (funcall key-func y)))))
  (dbg "z is now " z)
  (let (result)
    (dolist (x z)
      (dbg "bef" result)
      (let* ((k (funcall key-func x)))
	(when (or (null result) (not (equal k (car (first result)))))
	  (push (cons k nil) result))
	(push x (cdr (first result))))
      (dbg "aft" result)
      )

    (dbg (reverse result))
  ))
      
  
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


(defconst org-balance-goal-prefix "goal_")

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
	       (if (> dt 0) (floor (/ dt 60)) 0))
	   0)))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(while (re-search-forward
		(concat "^[ \t]*" org-clock-string
			"[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\)")
		nil t)
	  (setq ts (match-string 1)
		te (match-string 2)
		ts (org-float-time
		    (apply 'encode-time (org-parse-time-string ts)))
		te (org-float-time
		    (apply 'encode-time (org-parse-time-string te)))
		ts (if tstart (max ts tstart) ts)
		te (if tend (min te tend) te)
		dt (- te ts)
		total-minutes (if (> dt 0) (+ total-minutes (floor (/ dt 60))) total-minutes)))))
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

(defun* org-balance-compute-goal-deltas (&key goals tstart tend callback1 callback2 error-handler)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

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
	      
	      (let* ((line-here (org-current-line))
		     (goal-name-here (org-match-string-no-properties 1))
		     (goal-prop-here (substring goal-name-here (length org-balance-goal-prefix)))
		     (goal-def-here (org-match-string-no-properties 2))
		     (parsed-goal
		      (condition-case err
			  (rxx-parse org-balance-valu-ratio-goal-regexp goal-def-here)
			(error
			 (incf org-balance-num-warnings)
			 (message "Error parsing %s" goal-def-here)
			 nil)))
		     should-call-p
		     ;; values we pass to the callback that renders the
		     ;; goal info
		     cb-goal cb-actual cb-delta-val cb-delta-percent cb-goal-point cb-error)
		(unless parsed-goal (setq cb-error "Could not parse this goal"))
		(when parsed-goal
		  ;;
		  ;; Compute the actual usage under this subtree, and convert to the same
		  ;; units as the goal, so we can compare them.
		  ;;
		  (save-excursion (goto-char (point-at-bol))
				  (dbg (point) (buffer-substring (point) (1+ (point-at-eol))))
				  (setq cb-goal-point (point))
				  
				  )
		  
		  (save-match-data
		    (save-excursion
		      (save-restriction
			(org-narrow-to-subtree)
			(goto-char (point-min))
			(let* ((is-time (equal goal-name-here (concat org-balance-goal-prefix "clockedtime")))
			       (sum-here
				(if is-time (org-balance-clock-sum tstart tend)
				  (org-balance-sum-property
				   goal-prop-here
				   (org-balance-valu-unit
				    (org-balance-valu-ratio-goal-numer-min parsed-goal))
				   tstart tend))))
					; we only really need to set the text for entries where there is a goal.
					; but is it faster just to set it?
			  (let ((actual
				 (org-balance-convert-valu-ratio
				  (make-org-balance-valu-ratio
				   :num
				   (org-balance-make-valu sum-here
							  (if is-time 'minutes
							    (org-balance-valu-unit (org-balance-valu-ratio-goal-numer-min parsed-goal))))
				   :denom days-in-interval)
				  (make-org-balance-valu-ratio
				   :num (org-balance-valu-ratio-goal-numer-min parsed-goal)
				   :denom (org-balance-valu-ratio-goal-denom parsed-goal)))))

			    (let* ( (polarity (or (org-balance-valu-ratio-goal-polarity parsed-goal)
						  (cdr (assoc-string goal-prop-here org-balance-default-polarity))))
				    (margin (or (org-balance-valu-ratio-goal-margin parsed-goal)
						org-balance-default-margin-percent))
				    (goal-min (org-balance-valu-val (org-balance-valu-ratio-goal-numer-min parsed-goal)))
				    (goal-max (org-balance-valu-val (org-balance-valu-ratio-goal-numer-max parsed-goal)))
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
						      (if (eq polarity 'atleast) (- actual-num goal-max) (- goal-max actual-num)))
						     ((< actual-num range-min)
						      (if (eq polarity 'atmost) (- goal-min actual-num) (- actual-num goal-min)))))
				    (delta-percent
				     (* 100 (/ delta-val (if (< range-max actual-num) goal-max goal-min))))
				    )

			      ;(dbg goal-name-here goal-prop-here goal-def-here polarity actual-num range-min range-max goal-min goal-max delta-val delta-percent)

			      ;; so, actually, show delta vs specified range not vs margin;
			      ;; show it in units of the goal; and show both absolute and relative shift.

			      ;; let's make the sparse tree work well first, including robust error reporting, and handling multiple
			      ;; goals per entry, before moving on to the more involved issue of agenda.

			      ;; need a way to filter the goals shown, by priority of goal, as well as by arbitrary criterion.


			      ;(outline-flag-region line-here (1+ line-here) t)
			      (setq should-call-p t cb-goal goal-def-here cb-actual actual cb-delta-val delta-val
				    cb-delta-percent delta-percent)
			      (save-match-data
				(when (functionp callback1) (funcall callback1)))
			    )))))))
		(when (and should-call-p (functionp callback2))
		  (save-excursion
		    (save-match-data
		      (funcall callback2)))))
	      (when (> org-balance-num-warnings 0)
		(message "There were %d warnings; check the *Messages* buffer." org-balance-num-warnings)))))))))

;; struct: org-balance-goal-delta - information about how well one goal is being met.
;;      `org-balance-compute-goal-deltas' gathers this information from various entries and
;;      presents it in a list.
(defstruct org-balance-goal-delta goal entry-buf entry-pos goal-pos actual delta-val delta-percent error-msg)

(defun* org-balance-compute-goal-deltas2 (&key goals tstart tend callback1 callback2 error-handler)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  (let ((days-in-interval (org-balance-make-valu (/ (float (- tend tstart)) 60.0 60.0 24.0) 'days)))
    (save-excursion
      (save-restriction
	(save-match-data
	  (let ((org-balance-num-warnings 0) goal-deltas)
	    
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
	      
	      (let* ((save-goal-pos (point-at-bol))
		     (goal-name-here (org-match-string-no-properties 1))
		     (goal-prop-here (substring goal-name-here (length org-balance-goal-prefix)))
		     (goal-def-here (org-match-string-no-properties 2))
		     (parsed-goal
		      (condition-case err
			  (rxx-parse org-balance-valu-ratio-goal-regexp goal-def-here)
			(error
			 (incf org-balance-num-warnings)
			 (message "Error parsing %s" goal-def-here)
			 (push (make-org-balance-goal-delta :error-msg "Error parsing this goal") goal-deltas)
			 nil)))
		     should-call-p
		     ;; values we pass to the callback that renders the
		     ;; goal info
		     cb-goal cb-actual cb-delta-val cb-delta-percent cb-goal-point cb-error save-entry-pos)
		(unless parsed-goal (setq cb-error "Could not parse this goal"))
		(when parsed-goal
		  ;;
		  ;; Compute the actual usage under this subtree, and convert to the same
		  ;; units as the goal, so we can compare them.
		  ;;
		  (save-excursion (goto-char (point-at-bol))
				  (dbg (point) (buffer-substring (point) (1+ (point-at-eol))))
				  (setq cb-goal-point (point))
				  
				  )
		  
		  (save-match-data
		    (save-excursion
		      (save-restriction
			(org-narrow-to-subtree)
			(goto-char (point-min))
			(setq save-entry-pos (point))
			(let* ((is-time (equal goal-name-here (concat org-balance-goal-prefix "clockedtime")))
			       (sum-here
				(if is-time (org-balance-clock-sum tstart tend)
				  (org-balance-sum-property
				   goal-prop-here
				   (org-balance-valu-unit
				    (org-balance-valu-ratio-goal-numer-min parsed-goal))
				   tstart tend))))
					; we only really need to set the text for entries where there is a goal.
					; but is it faster just to set it?
			  (let ((actual
				 (org-balance-convert-valu-ratio
				  (make-org-balance-valu-ratio
				   :num
				   (org-balance-make-valu sum-here
							  (if is-time 'minutes
							    (org-balance-valu-unit (org-balance-valu-ratio-goal-numer-min parsed-goal))))
				   :denom days-in-interval)
				  (make-org-balance-valu-ratio
				   :num (org-balance-valu-ratio-goal-numer-min parsed-goal)
				   :denom (org-balance-valu-ratio-goal-denom parsed-goal)))))

			    (let* ( (polarity (or (org-balance-valu-ratio-goal-polarity parsed-goal)
						  (cdr (assoc-string goal-prop-here org-balance-default-polarity))))
				    (margin (or (org-balance-valu-ratio-goal-margin parsed-goal)
						org-balance-default-margin-percent))
				    (goal-min (org-balance-valu-val (org-balance-valu-ratio-goal-numer-min parsed-goal)))
				    (goal-max (org-balance-valu-val (org-balance-valu-ratio-goal-numer-max parsed-goal)))
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
						      (if (eq polarity 'atleast) (- actual-num goal-max) (- goal-max actual-num)))
						     ((< actual-num range-min)
						      (if (eq polarity 'atmost) (- goal-min actual-num) (- actual-num goal-min)))))
				    (delta-percent
				     (* 100 (/ delta-val (if (< range-max actual-num) goal-max goal-min))))
				    )

			      ;(dbg goal-name-here goal-prop-here goal-def-here polarity actual-num range-min range-max goal-min goal-max delta-val delta-percent)

			      ;; so, actually, show delta vs specified range not vs margin;
			      ;; show it in units of the goal; and show both absolute and relative shift.

			      ;; let's make the sparse tree work well first, including robust error reporting, and handling multiple
			      ;; goals per entry, before moving on to the more involved issue of agenda.

			      ;; need a way to filter the goals shown, by priority of goal, as well as by arbitrary criterion.


			      ;(outline-flag-region line-here (1+ line-here) t)
			      (setq should-call-p t cb-goal goal-def-here cb-actual actual cb-delta-val delta-val
				    cb-delta-percent delta-percent)
			      (save-match-data
				(when (functionp callback1) (funcall callback1)))
			      (push (make-org-balance-goal-delta :goal parsed-goal :entry-buf (current-buffer) :entry-pos save-entry-pos
								 :goal-pos save-goal-pos
								 :actual actual :delta-val delta-val :delta-percent delta-percent)
				    goal-deltas)
			    )))))))
		(when (and should-call-p (functionp callback2))
		  (save-excursion
		    (save-match-data
		      (funcall callback2))))))
	      (when (> org-balance-num-warnings 0)
		(message "There were %d warnings; check the *Messages* buffer." org-balance-num-warnings))
	      goal-deltas
	      ))))))


(defun org-balance-check-sparsetree ()
  "Show missed goals as sparsetree"
  (interactive)
  (org-balance-remove-overlays)
  (org-overview)
  (org-balance-compute-goal-deltas
   :callback1
   (lambda ()

     ;(goto-char (1+ cb-goal-point))

     (if cb-error
	 (org-balance-put-overlay cb-error 'error)
       ;; so, as the next thing:
       ;;   - if needed, open up the entry. if still needed,
       ;;     open up the properties drawer.
       ;;   - if we do open up the properties drawer, perhaps
       ;;     show goal results next to corresponding goals?
       ;;(forward-line)
       (org-balance-put-overlay
	(list
	 (format "%4d%% actual: \"%20s\" %.2f"
		 (round cb-delta-percent) cb-goal
		 (org-balance-valu-val (org-balance-valu-ratio-num cb-actual)))
	 "second line"
	 )
	)
     ))
   :callback2
   (lambda ()
     (let ((org-show-hierarchy-above t)
	   (org-show-following-heading nil)
	   (org-show-siblings nil))
       (org-back-to-heading 'invis-ok)
       (org-show-context 'default)
       ;(goto-char cb-goal-point)
       (when nil
	 (org-balance-put-overlay (format "%4d%% \"%20s\" actual: %.2f"
					  (round cb-delta-percent)
					  cb-goal (org-balance-valu-val (org-balance-valu-ratio-num cb-actual)))))
       
       ;(forward-line)
       ;;; ** uncomment
       ;(org-show-hidden-entry)
       ;(outline-flag-region (1- (point)) (1+ (point-at-eol)) nil)
       (save-excursion
	 (save-match-data
	   ;(re-search-forward org-drawer-regexp)
	   ;(goto-char (point-at-bol))
	   ;(org-flag-drawer nil)))
       ))))))

(defun org-balance-check2 ()
  "Check gathering of goal deltas"
  (interactive)
  (let* ((goal-deltas-orig (org-balance-compute-goal-deltas2 ))
	 (dummy (dbg (mapcar 'org-balance-goal-delta-entry-pos goal-deltas-orig) goal-deltas-orig))
	 (goal-deltas (org-balance-groupby goal-deltas-orig 'org-balance-goal-delta-entry-pos)))
    (dbg (length goal-deltas) goal-deltas)
    (org-balance-remove-overlays)
    (org-overview)
    (dolist (entry-goal-delta goal-deltas)
      (dbg (car entry-goal-delta) (length (cdr entry-goal-delta)) entry-goal-delta)
      (goto-char (org-balance-goal-delta-entry-pos (cadr entry-goal-delta)))

      (when t
	(org-balance-put-overlay
	 (mapcar
	  (lambda (goal-delta)
	    (format "%4d%% \"%20s\" actual: %.2f"
		    (round (org-balance-goal-delta-delta-percent goal-delta))
		    (org-balance-valu-ratio-goal-text (org-balance-goal-delta-goal goal-delta))
		    (org-balance-valu-val (org-balance-valu-ratio-num (org-balance-goal-delta-actual goal-delta)))))
	  (cdr entry-goal-delta))))
      (org-show-context 'default))))


(defun show-prefix (x)
  (interactive "P")
  (message "%s" x))


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

(defconst org-balance-number-name-regexp
  (rxx (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-balance-number-names))))
       (lambda (match) (cdr-safe (assoc-string match org-balance-number-names)))))

(defconst org-balance-number-regexp
  (rxx
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
    (rxx-parse org-balance-number-regexp s)))

(defalias 'org-balance-parse-number 'org-balance-string-to-number)

(defconst org-balance-number-range-regexp
  (rxx
   (seq
    (org-balance-number-regexp range-start)
    (optional "-" (org-balance-number-regexp range-end)))
   (cons range-start (or range-end range-start))))

(defvar org-balance-parse-valu-hooks nil
  "List of hooks for parsing valu strings (value with units), such as `5 hours'.  Can be used e.g. to parse currency
such as $5 into the canonical form `5 dollars'.  Each hook must take a string as an argument and return either an
`org-balance-valu' struct if it successfully parsed the string, or nil if it didn't.")

(defconst org-balance-unit-regexp
  (rxx (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-balance-unit2dim-alist))))))

(defconst
  org-balance-valu-regexp
  (rxx
   ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   (or (seq (optional (org-balance-number-regexp val))
	    (org-balance-unit-regexp unit))
       (seq val (optional unit)))
   (org-balance-make-valu (or val 1) (or unit "item"))
   "value with unit")
  "regexp for value with a unit, e.g. '1 day'")

  
(defun org-balance-parse-valu (valu-str)
  "Given a string representing a value with units, parse it into an org-balance-valu structure."
  (or
   (run-hook-with-args-until-success 'org-balance-parse-valu-hooks valu-str)
   (rxx-parse org-balance-valu-regexp valu-str)))

(defconst
  org-balance-valu-range-regexp
  (rxx
   ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   (or (seq (optional (seq (org-balance-number-range-regexp range) (one-or-more whitespace)))
	    (org-balance-unit-regexp unit))
       (seq range (optional (one-or-more whitespace) unit)))
   (let ((number-range (or range (cons 1 1)))
	 (unit (or unit "item")))
     (cons (org-balance-make-valu (car number-range) unit)
	   (org-balance-make-valu (cdr number-range) unit))))
   "value range")

(defun org-balance-parse-valu-range (valu-str)
  "Given a string representing a value range with units, parse it into an org-balance-valu structure."
  (rxx-parse org-balance-valu-range-regexp valu-str))

(defconst org-balance-ratio-words (list "per" "every" "each" "/" "a" "in a"))
(defconst org-balance-ratio-words-regexp (rxx (eval-regexp (regexp-opt org-balance-ratio-words 'words))))

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
  numer-min
  numer-max
  denom
  polarity
  margin
  text)

(defconst org-balance-polarity-regexp
  (rxx
   (seq (zero-or-more whitespace)
	(or (named-grp atmost (eval-regexp (regexp-opt (list "at most"))))
	    (named-grp atleast (eval-regexp (regexp-opt (list "at least")))))
	(zero-or-more whitespace))
     (if atmost 'atmost 'atleast)
   "polarity"))

(defconst org-balance-valu-ratio-goal-regexp
  (rxx
   (seq
    (optional (org-balance-polarity-regexp polarity))
    (org-balance-valu-range-regexp numerator)
    (one-or-more whitespace) org-balance-ratio-words-regexp (one-or-more whitespace)
    (org-balance-valu-regexp denominator)
    (optional
     ;; specify margin
     (seq
      (one-or-more whitespace) "+-" (zero-or-more whitespace)
      (or
       (seq (org-balance-number-regexp margin) (zero-or-more whitespace) "%")
       (org-balance-valu-regexp margin)))))
   (lambda (goal-str)
     (make-org-balance-valu-ratio-goal 
      :numer-min (car numerator)
      :numer-max (cdr numerator)
      :denom denominator
      :polarity polarity
      :margin margin
      :text goal-str)))
  "value ratio goal")


(provide 'org-balance)


;; test

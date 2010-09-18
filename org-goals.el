;;; org-goals.el --- Set and check goals for spending your time or other resources
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
;; org-goals: orgmode tools for setting goals for how much time/effort you want to spend on various
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
;;      org-goals-menu - show a menu of org-goals commands
;;
;;   Recording:
;;
;;      org-goals-record-time - record time spent under the current entry.
;;
;;   Reporting:
;;
;;      org-goals-done-in-range - show items completed in given date range (e.g. within last two weeks).
;;      org-goals-show-clocked-time - show things on which you worked recently
;;      org-goals-show-neglected-time - show things on which you spend less time than you want
;;      org-goals-show-non-neglected-time - show things on which you spend enough time
;;      org-goals-show-neglected-val - show things where you get less done than you want
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: external dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-clock)
(require 'org-agenda)
(require 'org-compat)
(require 'org-macs)
(eval-when-compile
  (require 'cl)
  (require 'rxx))

(defvar org-clock-report-include-clocking-task)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: org-goals customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup org-goals nil
  "Options for the org-goals package"
  :tag "Org Balance"
  :group 'org
  :link '(url-link "http://sourceforge.net/projects/org-goals/")
  )

(defcustom org-goals-default-margin-percent 5.0
  "Report entries as neglected if they are neglected by at least percentage"
  :group 'org-goals
  :type 'float)

(defcustom org-goals-default-interval "1 week"
  "By default, report goal compliance averaged over this time interval
before today."
  :group 'org-goals
  :type 'string)   ;; todo: add a validation function to make sure this can be parsed as a duration.


(defcustom org-goals-default-polarity (quote (("clockedtime" . atleast) ("done" . atleast) ("spend" . atmost)))
  "Default polarity to use for specific properties"
  :group 'org-goals
  :type '(alist :key-type string :value-type (radio (const :tag "at least" atleast)
						    (const :tag "at most" atmost))))

(defcustom org-goals-agenda-sorting-strategy
  '(priority-down category-keep user-defined-down)
  "Sorting rules for sorting org-goals agenda views."
  :group 'org-goals
  :link '(variable-link org-agenda-sorting-strategy)
  :type `(repeat ,org-sorting-choice))

(defgroup org-goals-faces nil
  "Faces in org-goals"
  :tag "org-goals faces"
  :group 'org-goals)

(defface org-goals-malformed-goal
  '((((background light)) (:foreground "Firebrick"))
    (((background dark)) (:foreground "black")))
  "Face used for showing malformed goals"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: org-goals overlays
;; 
;; Code for managing org-goals overlays
;;
;; Aadapted from org-clock.el; it might make sense to
;; make a generic overlay facility for use by various org-modules.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-goals-overlays nil
  "We use overlays to display summary information about how much got done
under each category.  Modeled on org-clock-overalys.")
(make-variable-buffer-local 'org-goals-overlays)

(defun org-goals-put-overlay (txt &optional error)
  "Put an overlay on the current line, displaying TXT.
This creates a new overlay and stores it in `org-goals-overlays', so that it
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
			 (list 'face (if error 'org-goals-malformed-goal 'org-clock-overlay)))
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
    (push ov org-goals-overlays)))

(defun org-goals-remove-overlays (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (if (or (not org-goals-overlays) org-inhibit-highlight-removal)
      nil
    (progn
	(mapc 'delete-overlay org-goals-overlays)
	(setq org-goals-overlays nil)
	(unless noremove
	  (remove-hook 'before-change-functions
		       'org-clock-remove-overlays 'local))
	t)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-goals-remove-overlays)

(defun org-goals-reset-overlays ()
  "Reset org-goals overlays if present, preparing to put on new ones"
  (org-overview)
  (org-goals-remove-overlays)
  (org-unhighlight)
  (when org-remove-highlights-with-change
    (org-add-hook 'before-change-functions 'org-goals-remove-overlays
		  nil 'local)))

(defun org-goals-unhighlight ()
  (interactive)
  (org-unhighlight))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Utils
;;
;; General-purpose utility routines used in org-goals module
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
     (car-safe (with-no-warnings (last expr-vals)))))

(defmacro safe-val (x) `(if (boundp (quote ,x)) ,x 'unbound))

(defun org-goals-groupby (z key-func)
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
  
(defun org-goals-get-property (prop &optional default-val)
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

(defun org-goals-set-property (prop val &optional default-val clear-when-default)
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

(defun org-goals-delete-property-globally (prop)
  "Delete either text or org property globally from all entries."
    (if (stringp prop)
	(org-delete-property-globally prop)
      (remove-list-of-text-properties (point-min) (point-max) (list prop))))

(defun org-goals-trim-whitespace (s)
  "Trim trailing and leading whitespace from string"
  (save-match-data
    (replace-regexp-in-string
     (rx (or (seq string-start (zero-or-more whitespace))
	     (seq (zero-or-more whitespace) string-end)))
     "" (if (symbolp s) (symbol-name s) s))))

(defun org-goals-full-match (re s)
  "Do a string match, but fail unless the regexp matches the full string"
  (and (string-match re s)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-goals-done-in-range ()
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

(defun org-goals-sum-property (prop unit tstart tend)
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
				 (org-goals-valu-val
				  (org-goals-convert-valu
				   (org-goals-parse-valu prop-here)
				    unit))
				  (error
				   (message
				    "Warning: at line %d of file %s, could not add %s to sum for goal %s; sum-so-far is %s unit is %s"
					    (line-number-at-pos (point)) (buffer-file-name (current-buffer))
					    prop-here prop prop-sum unit)
				   (setq org-goals-num-warnings (1+ org-goals-num-warnings))
				   0))))))))))
		
	prop-sum))))

(defun org-goals-record-time (&optional hours ago)
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


(defconst org-goals-goal-todo-keyword "GOAL")

(defun org-goals-clock-sum (tstart tend)
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
    

(defun org-goals-read-date (s)
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
    (when (string-match (concat "\\(" org-goals-number-regexp "\\)" "[hH]") s)
      (org-goals-string-to-number (match-string 1 s))
    )
  ))


;; struct: org-goals-goal-delta - information about how well one goal is being met.
;;      `org-goals-compute-goal-deltas' gathers this information from various entries and
;;      presents it in a list.
(defstruct org-goals-goal-delta heading goal entry-buf entry-pos goal-pos actual delta-val delta-percent error-msg)

(defconst org-goals-goal-prefix-regexp
  (rxx (seq bol (1+ "*") (1+ blank) (optional "[#" upper "]" (1+ blank)) (eval org-goals-goal-todo-keyword) (1+ blank)
	    (named-group goal-name (1+ alnum)) (1+ blank)) goal-name))

(defun* org-goals-compute-goal-deltas2 (&key goals tstart tend)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  (let ((num-errors 0) (num-under 0) (num-met 0) (num-over 0)
	(days-in-interval (org-goals-make-valu (/ (float (- tend tstart)) 60.0 60.0 24.0) 'days)))
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(save-match-data
	  (let ((org-goals-num-warnings 0) goal-deltas)
	    ;; FIXME if goals specified, make regexp for them
	    (let (goal-name-here)
	      (while (setq goal-name-here
			   (rxx-search-fwd
			    org-goals-goal-prefix-regexp (not 'bound) 'no-error))
		(let* ((goal-def-here (buffer-substring (point) (point-at-eol)))
		       (parsed-goal
			(condition-case err
			    (org-goals-parse-goal-or-link-at-point)
			(error
			 (incf num-errors)
			 (message "Error parsing %s" goal-def-here)
			 (save-match-data (org-toggle-tag "goal_error" 'on))
			 nil))))
		  (when parsed-goal
		    (org-toggle-tag "goal_error" 'off)
		    ;;
		    ;; Compute the actual usage under this subtree, and convert to the same
		    ;; units as the goal, so we can compare them.
		    ;;
		    (let (delta-val delta-percent)
		      (save-match-data
			(save-excursion
			  (save-restriction
			    (outline-up-heading 1 'invisible-ok)
			    (org-narrow-to-subtree)
			    (goto-char (point-min))
			    (let* ((is-time (equal goal-name-here "clockedtime"))
				   (to-unit (if is-time 'minutes
					      (org-goals-valu-unit
					       (org-goals-goal-numer-min parsed-goal))))
				   (sum-here
				    (if is-time (org-goals-clock-sum tstart tend)
				      (org-goals-sum-property
				       goal-name-here
				       to-unit
				       tstart tend))))
			      (let ((actual
				     (org-goals-convert-valu-ratio
				      (make-org-goals-valu-ratio
				       :num
				       (org-goals-make-valu sum-here to-unit)
				       :denom days-in-interval)
				      (make-org-goals-valu-ratio
				       :num (org-goals-goal-numer-min parsed-goal)
				       :denom (org-goals-goal-denom parsed-goal)))))
				
				(let* ( (polarity (or (org-goals-goal-polarity parsed-goal)
						      (cdr (assoc-string goal-name-here org-goals-default-polarity))))
					(margin (or (org-goals-goal-margin parsed-goal)
						    org-goals-default-margin-percent))
					(goal-min (org-goals-valu-val (org-goals-goal-numer-min parsed-goal)))
					(goal-max (org-goals-valu-val (org-goals-goal-numer-max parsed-goal)))
					(range-min (- goal-min
						      (if (numberp margin)
							  (* (/ (float margin) 100.0) goal-min)
							(org-goals-valu-val margin))))
					
					(range-max (+ goal-max
						      (if (numberp margin)
							  (* (/ (float margin) 100.0) goal-max)
							(org-goals-valu-val margin))))
					
					(actual-num (org-goals-valu-val (org-goals-valu-ratio-num actual))))
				  
				  (setq delta-val (cond ((and (<= range-min actual-num) (<= actual-num range-max)) 0)
							((< range-max actual-num)
							 (if (eq polarity 'atleast)
							     (- actual-num goal-max)
							   (- goal-max actual-num)))
							((< actual-num range-min)
							 (if (eq polarity 'atmost)
							     (- goal-min actual-num)
							   (- actual-num goal-min))))
					delta-percent
					(* 100 (/ delta-val (if (< range-max actual-num) goal-max goal-min)))))))))
			(cond ((< delta-val 0) (incf num-under))
			      ((> delta-val 0) (incf num-over))
			      ((= delta-val 0) (incf num-met)))
			(org-entry-put (point) "goal_delta_val" (format "%s" delta-val))
			(org-entry-put (point) "goal_delta_percent" (format "%s" delta-percent))))))))))))
    (message "err %d under %d met %d over %d" num-errors num-under num-met num-over)))
  

(defun org-goals-check-sparsetree ()
  "Show missed goals as sparsetree"
  (interactive)
  (org-goals-remove-overlays)
  (org-overview)
  (org-goals-compute-goal-deltas
   :callback1
   (lambda ()

     ;(goto-char (1+ cb-goal-point))

     (if cb-error
	 (org-goals-put-overlay cb-error 'error)
       ;; so, as the next thing:
       ;;   - if needed, open up the entry. if still needed,
       ;;     open up the properties drawer.
       ;;   - if we do open up the properties drawer, perhaps
       ;;     show goal results next to corresponding goals?
       ;;(forward-line)
       (org-goals-put-overlay
	(list
	 (format "%4d%% actual: \"%20s\" %.2f"
		 (round cb-delta-percent) cb-goal
		 (org-goals-valu-val (org-goals-valu-ratio-num cb-actual)))
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
	 (org-goals-put-overlay (format "%4d%% \"%20s\" actual: %.2f"
					  (round cb-delta-percent)
					  cb-goal (org-goals-valu-val (org-goals-valu-ratio-num cb-actual)))))
       
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

(defun org-goals-check2 ()
  "Check gathering of goal deltas"
  (interactive)
  (let* ((goal-deltas-orig (org-goals-compute-goal-deltas2 ))
	 (goal-deltas (org-goals-groupby goal-deltas-orig 'org-goals-goal-delta-entry-pos)))
    (org-goals-remove-overlays)
    (org-overview)
    (dolist (entry-goal-delta goal-deltas)
      (goto-char (org-goals-goal-delta-entry-pos (cadr entry-goal-delta)))
      (org-goals-put-overlay
       (mapcar
	(lambda (goal-delta)
	  (format "%4d%% \"%20s\" actual: %.2f"
		  (round (org-goals-goal-delta-delta-percent goal-delta))
		  (org-goals-goal-text (org-goals-goal-delta-goal goal-delta))
		  (org-goals-valu-val (org-goals-valu-ratio-num (org-goals-goal-delta-actual goal-delta)))))
	(cdr entry-goal-delta)))
      (org-show-context 'default))))


(defun org-goals-check3 (&optional tstart tend)
  "Show goal deltas as agenda entries"
  (interactive)
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))
  (let* ((props (list 'face 'default
		      'done-face 'org-agenda-done
		      'undone-face 'default
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 )
    (rxx-flet ((org-make-tags-matcher (match) (cons "org-goals" nil))
	       (org-prepare-agenda (title) (org-prepare-agenda-orig "org-goals"))
	       ;; set redo command
	       (org-scan-tags
		(&rest args)
		(mapcar
		 (lambda (goal-delta)
		   (goto-char (org-goals-goal-delta-entry-pos goal-delta))
		   (let ((txt
			  (org-format-agenda-item
			   nil
			   (or
			    (org-goals-goal-delta-error-msg goal-delta)
			    (format "%-40s %+4d%% %20s actual: %.2f"
				    (org-goals-goal-delta-heading goal-delta)
				    (round (org-goals-goal-delta-delta-percent goal-delta))
				    (org-goals-goal-text (org-goals-goal-delta-goal goal-delta))
				    (org-goals-valu-val (org-goals-valu-ratio-num
							   (org-goals-goal-delta-actual goal-delta)))))
			   ))
			 (marker (org-agenda-new-marker)))
		     (org-add-props txt props 'org-marker marker 'org-hd-marker marker 'org-category (org-get-category)
				    'priority (org-get-priority
					       (or
						(org-goals-goal-priority (org-goals-goal-delta-goal goal-delta))
						(org-get-heading))) 'type "org-goals"
						'org-goals-goal-delta goal-delta)))
		 (org-goals-compute-goal-deltas2 :tstart tstart :tend tend)))
	       )
      (let (org-agenda-before-sorting-filter-function
	    org-agenda-sorting-strategy-selected
	    (org-agenda-cmp-user-defined
		(lambda (a b)
		  (let ((va (org-goals-goal-delta-delta-percent (get-text-property 0 'org-goals-goal-delta a)))
			(vb (org-goals-goal-delta-delta-percent (get-text-property 0 'org-goals-goal-delta b))))
		    (if (< va vb) -1 +1)
		    )))
	    (org-agenda-sorting-strategy `((tags ,@org-goals-agenda-sorting-strategy))))
	(org-tags-view)))))

(defun org-goals-show-neglected-time (&optional tstart tend)
  (interactive)
  (org-goals-reset-overlays)
  (let (org-goals-result-list)
    (message "got %d"
	     (length (delq nil 
			   (org-goals-compute-goal-deltas
			    :goal "time" :tstart tstart :tend tend
			    :callback
			    (lambda (goal-delta total-here per-day-here per-day-goal-here)
			       (when 
				 (org-show-context)
				 (org-goals-put-overlay (format "goal-delta %s total %s per-day %s per-day-goal %s"
								  goal-delta total-here per-day-here per-day-goal-here)))
			       t)
			    :error-handler
			    (lambda (err)
			      (org-show-context)
			      (org-goals-put-overlay (format "Error: %s" (error-message-string err))))
			      
			    ))))))

(defun org-goals-show-neglected-val (&optional tstart tend)
  (interactive)
  (org-goals-reset-overlays)
  (org-goals-compute-goal-deltas :goal "done" :tstart tstart :tend tend
				   :callback
				   (lambda (goal-delta total-here per-day-here per-day-goal-here)
				     (when (< goal-delta 0)
				       (org-show-context)
				       (org-goals-put-overlay (format "goal-delta %s total %s per-day %s per-day-goal %s" goal-delta total-here per-day-here per-day-goal-here))))))


(defun org-goals-show-non-neglected-val (&optional tstart tend)
  (interactive)
  (org-goals-reset-overlays)
  (org-goals-compute-goal-deltas :goal "done" :tstart tstart :tend tend
				   :callback
				   (lambda (goal-delta total-here per-day-here per-day-goal-here)
				     (when (>= goal-delta 0)
				       (org-show-context)
				       (org-goals-put-overlay
					(format "goal-delta %s total %s per-day %s per-day-goal %s" goal-delta total-here per-day-here per-day-goal-here))))))



(defun org-goals-show-non-neglected-time (&optional tstart tend)
  (interactive)
  (org-goals-check-goals :goal "time" :tstart tstart :tend tend
			   :neglect-tolerance 30 :show-if-too-much-p t :show-if-just-right t))

(defun org-goals-show-clocked-time (&optional tstart tend)
  "Show entries that have at least some clocked time in the given interval"
  (interactive)

  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Start date: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))
  
  (org-clock-sum tstart tend)
  (message "finished clock sum")

  (org-overview)
  (org-goals-remove-overlays)
  (when org-remove-highlights-with-change
    (org-add-hook 'before-change-functions 'org-goals-remove-overlays
		  nil 'local))
  
  (org-map-entries
   (lambda ()
     (let* ((total-minutes-here (get-text-property (point) :org-clock-minutes)))
       (when (and total-minutes-here (> total-minutes-here 0))
	 (org-show-context)
	 (org-goals-put-overlay (format "tot: %s" (org-minutes-to-hh:mm-string (round total-minutes-here))))
	 )))))


(defun org-goals-show-val (&optional tstart tend)
  "Show entries that have at least some clocked time in the given interval"
  (interactive)

  (when (not tstart) (setq tstart (org-float-time (org-read-date nil 'to-time nil "Start date: "))))
  (when (not tend) (setq tend (org-float-time (org-current-time))))
  
  (org-goals-sum-property "val" :val-sum 1 tstart tend)
  (message "finished clock sum")

  (org-overview)
  (org-goals-remove-overlays)
  (when org-remove-highlights-with-change
    (org-add-hook 'before-change-functions 'org-goals-remove-overlays
		  nil 'local))
  
  (org-map-entries
   (lambda ()
     (let* ((total-minutes-here (get-text-property (point) :val-sum)))
       (when (and total-minutes-here (> total-minutes-here 0))
	 (org-show-context)
	 (org-goals-put-overlay (format "tot: %.1f" total-minutes-here))
	 )))))


(defmacro org-goals-replacing-function (fname new-func body)
  ;; Execute code, temporarily replacing a given function with a new one
  `(let ((old-func (symbol-function ,fname)))
     (unwind-protect
	 (progn
	   (fset ,fname ,new-func)
	   ,body)
       (fset ,fname old-func))))

(defsubst org-goals-cmp (a b)
  "Compare agenda entries by the amount of neglect, with the most-neglected at the top."
  (let ((pa (or (get-text-property 0 :org-goals-neglect a) 0))
	(pb (or (get-text-property 0 :org-goals-neglect b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))

(defun org-goals-save-amt-neglected (agenda-line)
  "Given an agenda line, save the 'neglect amount' value of the corresponding org entry
as a text property, for later use by org-goals-cmp.  Also, add the neglect amount value
to the agenda line.
"
  (let ((orig-entry (get-text-property 0 'org-hd-marker agenda-line)) cur-buf)
    (if orig-entry
	(save-excursion
	  (setq cur-buf (current-buffer))
	  (switch-to-buffer (marker-buffer orig-entry))
	  (goto-char (marker-position orig-entry))
	  (let ((org-goals-neglect-val (get-text-property (point) :org-goals-neglect)))
	    (if (and org-goals-neglect-val (> org-goals-neglect-val 0.01)) 
		(progn
		  (put-text-property 0 1 :org-goals-neglect org-goals-neglect-val agenda-line)
		  (let ((r (concat agenda-line "::: " (number-to-string org-goals-neglect-val))))
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

(defun org-goals-set-goal ()
  "Set a goal property on the current entry.
Offer completion for the goal.  Optionally show help screen with goal examples.
"
  (interactive)
  )

(defun org-goals-check-goals ()
  "Run a check of compliance with goals."
  (interactive)
  )


(defun* org-goals-dispatcher-api (&key (action 'tree) tstart tend (goal "time")
					 (goal-delta-relative-p t) (goal-delta-max .05)
					 (show-goals-under t) show-goals-over show-goals-within
					 )
  "Top-level dispatcher for the org-goals package: all user functions can be invoked by calling this routine with
appropriate parameters.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  (cond ((eq action 'tree)
	 (org-goals-reset-overlays)
	 (org-goals-compute-goal-deltas
	  :goal goal :goal-delta-relative-p goal-delta-relative-p :tstart tstart :tend tend
	  :callback
	  (lambda (goal-delta total-here per-day-here per-day-goal-here per-day-here-in-their-units
			      per-day-goal-in-their-units their-units)
	    (when (or (and show-goals-under (< goal-delta (- goal-delta-max)))
		      (and show-goals-over (> goal-delta goal-delta-max))
		      (and show-goals-within (<= (- goal-delta-max) goal-delta goal-delta-max)))
	      (org-show-context)
	      (org-goals-put-overlay (format "%s%d%%: %.2f (not %s) %s"
					       (if (>= goal-delta 0) "+" "")
					       (round (* goal-delta 100.0)) 
					       per-day-here-in-their-units per-day-goal-in-their-units
					       their-units
					       ))))
	  :error-handler
	  (lambda (err)
	    (org-show-context)
	    (org-goals-put-overlay (format "Error: %s" (error-message-string err))))	  
	  ))))


(defun orgb-test ()
  (interactive)
  (let ((displayed-month 8)
	(displayed-year 2010))
    (org-goals-dispatcher-api :show-goals-within nil :show-goals-over nil )
  ))


(defun org-goals-make-agenda-custom-commands ()
       '(("b" "org-goals: neglected items" tags "val_goal>=0"
	  ((org-agenda-sorting-strategy '(user-defined-down))
	   (org-agenda-cmp-user-defined 'org-goals-cmp)
	   (org-agenda-before-sorting-filter-function 'org-goals-save-amt-neglected)))))

(setq org-agenda-custom-commands (org-goals-make-agenda-custom-commands))
	
(require 'widget)

(eval-when-compile
  (require 'wid-edit)
  (require 'cl)
  (require 'cl-19))

(defun org-goals-hide-menu ()
  "Hides the org-goals menu"
  (interactive)
  (org-unhighlight)
  (when (string= (buffer-name (current-buffer))
		 "*Org-Goals Menu*")
    (org-goals-remove-overlays)
    (kill-buffer (current-buffer))
    (delete-window)
    ))



(defun org-goals-menu ()
  "Show a menu of org-goals functions"
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "This function only works in org-mode buffers")
    (let* ((orig-window-config (current-window-configuration))
	   (restore-func `(lambda (&rest ignore)
			    (org-goals-hide-menu)
			    (set-window-configuration ,orig-window-config)
			    (org-unhighlight))))
      (delete-other-windows)
      (let ((org-buf (current-buffer))
	    (org-window (selected-window)))
	(org-highlight (point-at-bol) (point-at-eol))
	(recenter)
	(split-window)
	(switch-to-buffer "*Org-Goals Menu*")
	(kill-all-local-variables)
	(let ((inhibit-read-only t))
	  (erase-buffer))
	(let ((all (overlay-lists)))
	  ;; Delete all the overlays.
	  (mapcar 'delete-overlay (car all))
	  (mapcar 'delete-overlay (cdr all)))
	(widget-insert "Choose an org-goals command:\n\n")
	(widget-create 'push-button
		       :notify `(lambda (&rest ignore)
				  (delete-other-windows)
				  (switch-to-buffer ,org-buf)
				  (org-goals-record-time)
				  (,restore-func))
		       "Record time")
	(widget-insert "\n")
	(widget-create 'push-button
		       :notify `(lambda (&rest ignore)
				  (delete-other-windows)
				  (switch-to-buffer ,org-buf)
				  (org-goals-record-time)
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
				  (org-goals-show-neglected-time)
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
			  (org-goals-hide-menu)))
	
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
;; and we don't want to mess up other calculations by adding org-goals units to emacs standard units.)
;;
;; notes
;;
;; an alternative would be to set calc.el units only temporarily, while org-goals code runs,
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


(defconst org-goals-units
  '((time . ((second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
	     (workweek . 7200)
	     (month . 43200) (year . 525600) (bluemoon 1e12)))
    (money . ((dollar . 1) (cent . .01) (k . 1000)))
    (count . ((item . 1) (time . 1)))))

;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defconst org-goals-units
      (mapcar
       (lambda (dimension-info)
	 (cons (car dimension-info)
	       (reduce 'append
		       (mapcar
			(lambda (unit-info)
			  (list unit-info (cons (intern (concat (symbol-name (car unit-info)) "s")) (cdr unit-info))))
			(cdr dimension-info)))))
       org-goals-units))

;; var: org-goals-unit2dim-alist - assoc list mapping each unit to its dimension (time, money, count, ...)
(defconst org-goals-unit2dim-alist
  (reduce 'append
	  (mapcar
	   (lambda (dimension-info)
	     (mapcar (lambda (unit-info) (cons (car unit-info) (car dimension-info))) (cdr dimension-info)))
	   org-goals-units)))

(put 'org-goals-error 'error-conditions '(error org-goals-errors org-goals-error))
(put 'org-goals-error 'error-message "org-goals error")

(defun org-goals-assoc-val (key alist &optional error-message)
  "Looks up KEY in association list ALIST.  Unlike `assoc', returns the associated value rather than the associated pair.
Also, converts key to a symbol if it is a string.
If ERROR-MESSAGE is given, and the key is not in the list, throws an error with this message.
"
  (let ((assoc-result (assoc (if (stringp key) (intern key) key) alist)))
    (if assoc-result (cdr assoc-result)
      (if (eq error-message 'nil-ok) nil
	(signal 'org-goals-error
		(list (if error-message error-message (format "key %s not in alist %s" key alist))))))))

(defun org-goals-is-unit (unit)
  (org-goals-assoc-val unit org-goals-unit2dim-alist 'nil-ok))

(defun org-goals-unit2dim (unit)
  "Given a unit, return the dimension that its measures"
  (org-goals-assoc-val unit org-goals-unit2dim-alist))

;; var: org-goals-unit2base-alist - assoc list mapping each unit to how many base units are in it
(defconst org-goals-unit2base-alist (reduce 'append (mapcar 'cdr org-goals-units)))

(defun org-goals-unit2base (unit)
  "Return the number of base units in the given unit.  For each dimension we have a base unit in terms of which all other
units are measured; for example, for time we use minutes."
  (org-goals-assoc-val unit org-goals-unit2base-alist))

;; Struct: org-goals-valu - a value together with a given unit of measurement, e.g., 5 hours. 
(defstruct (org-goals-valu
	    (:constructor new-org-goals-valu
			  (val unit-name
			       &aux (unit
				     (if (stringp unit-name) (intern unit-name) unit-name)))))
	    val unit)

(defun org-goals-scale-valu (factor valu)
  "Return the value scaled by the factor"
  (new-org-goals-valu (* factor (org-goals-valu-val valu)) (org-goals-valu-unit valu)) 
  )

(defun org-goals-make-valu (val unit)
  (if (and (numberp val) (org-goals-is-unit unit))
      (new-org-goals-valu val unit)
    (error "Invalid valu: %s %s" val unit)))

(defun org-goals-convert-valu (valu new-unit &optional multiples-of-new-unit)
  "Convert a valu to new units in the same dimension, e.g. 1 hour to 1/24 of a day.  If MULTIPLES-OF-NEW-UNIT is given,
we convert to the specified multiples of new unit."
  (when (stringp new-unit) (setq new-unit (intern new-unit)))
  (unless (eq (org-goals-unit2dim (org-goals-valu-unit valu)) (org-goals-unit2dim new-unit))
    (error "Cannot convert between incompatible units: %s and %s" (org-goals-valu-unit valu) new-unit))
  (unless multiples-of-new-unit (setq multiples-of-new-unit 1))
  (make-org-goals-valu :val (/ (/ (float (* (org-goals-valu-val valu)
					      (org-goals-unit2base (org-goals-valu-unit valu))))
				    (float (org-goals-unit2base new-unit)))
				 multiples-of-new-unit) :unit new-unit))

(defun org-goals-add-valu (valu1 valu2)
  "Add two values with units, converting them to a common unit"
  (let* ((unit1 (org-goals-valu-unit valu1))
	 (unit2 (org-goals-valu-unit valu2))
	 (smaller-unit (if (< (org-goals-unit2base unit1) (org-goals-unit2base unit2))
			   unit1 unit2))
	 (conv1 (org-goals-convert-valu valu1 smaller-unit))
	 (conv2 (org-goals-convert-valu valu2 smaller-unit)))
    (org-goals-make-valu (+ (org-goals-valu-val conv1)
			      (org-goals-valu-val conv2))
			   smaller-unit)))

(put 'org-goals-parse-error 'error-conditions '(error org-goals-errors org-goals-parse-error))
(put 'org-goals-parse-error 'error-message "org-goals: Could not parse")

(defconst org-goals-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(defconst org-goals-number-name-regexp
  (rxx (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-goals-number-names))))
       (lambda (match) (cdr-safe (assoc-string match org-goals-number-names)))))

(defconst org-goals-number-regexp
  (rxx
   (seq
     (zero-or-more whitespace)
     
     (or
      ;; either an english number name
      (org-goals-number-name-regexp named-number)
      
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
			

(defun org-goals-is-valid-number-p (s)
  "Test if s is a number or a string representing a valid number (ignoring leading or trailing whitespace).
The whole string must match a floating-point number optionally surrounded by whitespace; extraneous characters in string
are not allowed.
"
  (if (numberp s) s
    (save-match-data
      (org-goals-full-match org-goals-number-regexp s))))

(defun org-goals-string-to-number (s)
  "Convert a string to a number, recognizing some number names for readability.  If s is already a number, just return it.
Unlike the standard `string-to-number', if the string as a whole cannot be interpreted as a valid number possibly surrounded
by whitespace, it throws an error rather than silently returning zero.
"
  (if (numberp s) s
    (rxx-parse org-goals-number-regexp s)))

(defalias 'org-goals-parse-number 'org-goals-string-to-number)

(defconst org-goals-number-range-regexp
  (rxx
   (seq
    (org-goals-number-regexp range-start)
    (optional "-" (org-goals-number-regexp range-end)))
   (cons range-start (or range-end range-start))))

(defvar org-goals-parse-valu-hooks nil
  "List of hooks for parsing valu strings (value with units), such as `5 hours'.  Can be used e.g. to parse currency
such as $5 into the canonical form `5 dollars'.  Each hook must take a string as an argument and return either an
`org-goals-valu' struct if it successfully parsed the string, or nil if it didn't.")

(defconst org-goals-unit-regexp
  (rxx (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-goals-unit2dim-alist))))))

(defconst
  org-goals-valu-regexp
  (rxx
   ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   (or (seq (optional (org-goals-number-regexp val))
	    (org-goals-unit-regexp unit))
       (seq val (optional unit)))
   (org-goals-make-valu (or val 1) (or unit "item"))
   "value with unit")
  "regexp for value with a unit, e.g. '1 day'")

  
(defun org-goals-parse-valu (valu-str)
  "Given a string representing a value with units, parse it into an org-goals-valu structure."
  (or
   (run-hook-with-args-until-success 'org-goals-parse-valu-hooks valu-str)
   (rxx-parse org-goals-valu-regexp valu-str)))

(defconst
  org-goals-valu-range-regexp
  (rxx
   ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   (or (seq (optional (seq (org-goals-number-range-regexp range) (one-or-more whitespace)))
	    (org-goals-unit-regexp unit))
       (seq range (optional (one-or-more whitespace) unit)))
   (let ((number-range (or range (cons 1 1)))
	 (unit (or unit "item")))
     (cons (org-goals-make-valu (car number-range) unit)
	   (org-goals-make-valu (cdr number-range) unit))))
   "value range")

(defun org-goals-parse-valu-range (valu-str)
  "Given a string representing a value range with units, parse it into an org-goals-valu structure."
  (rxx-parse org-goals-valu-range-regexp valu-str))

(defconst org-goals-ratio-words (list "per" "every" "each" "/" "a" "in a"))
(defconst org-goals-ratio-words-regexp (rxx (eval-regexp (regexp-opt org-goals-ratio-words 'words))))

;; Struct: org-goals-valu-ratio - a ratio of two valu's.
(defstruct org-goals-valu-ratio num denom
  ;; ratio- - the word from org-goals-ratio-words to use when printing the ratio.
  ratio-word)

(defun org-goals-convert-valu-ratio (old-valu-ratio new-valu-ratio)
  "Convert a valu ratio to new units, e.g. minutes per day to hours per week.  We keep the denominator of the new ratio,
changing only the numerator."
  (let ((new-num (org-goals-valu-ratio-num new-valu-ratio))
	(new-denom (org-goals-valu-ratio-denom new-valu-ratio)))
    (make-org-goals-valu-ratio
     :num (org-goals-make-valu (/ (org-goals-valu-val (org-goals-convert-valu
							   (org-goals-valu-ratio-num old-valu-ratio)
							   (org-goals-valu-unit new-num)))
				    (org-goals-valu-val (org-goals-convert-valu
							   (org-goals-valu-ratio-denom old-valu-ratio)
							   (org-goals-valu-unit new-denom)
							   (org-goals-valu-val new-denom))))
				 (org-goals-valu-unit new-num))
     :denom new-denom :ratio-word (org-goals-valu-ratio-ratio-word new-valu-ratio)))) 


;; Struct: org-goals-goal - the user-specified goal you have for a ratio.
(defstruct org-goals-goal
  numer-min
  numer-max
  denom
  polarity
  margin
  priority
  ratio-word
  text)

(defun org-goals-scale-goal (factor goal)
  (let ((result (copy-org-goals-goal goal)))
    (setf (org-goals-goal-numer-min result)
	  (org-goals-scale-valu factor (org-goals-goal-numer-min result)))
    (setf (org-goals-goal-numer-max result)
	  (org-goals-scale-valu factor (org-goals-goal-numer-max result)))
    (setf (org-goals-goal-text result) (format "%.2f * (%s)" factor (org-goals-goal-text goal)))
    result
  ))

(defconst org-goals-polarity-regexp
  (rxx
   (seq (zero-or-more whitespace)
	(or (named-grp atmost (eval-regexp (regexp-opt (list "at most"))))
	    (named-grp atleast (eval-regexp (regexp-opt (list "at least")))))
	(zero-or-more whitespace))
     (if atmost 'atmost 'atleast)
   "polarity"))

(defconst org-goals-goal-regexp
  (rxx
   (seq
    (0+ blank)
    (optional
     (seq (named-grp priority (regexp "\\[#[A-Z0-9]\\]")) (0+ blank)))
    (optional (org-goals-polarity-regexp polarity))
    (org-goals-valu-range-regexp numerator)
    (1+ blank) (org-goals-ratio-words-regexp ratio-word) (1+ blank)
    (org-goals-valu-regexp denominator)
    (optional
     ;; specify margin
     (seq
      (1+ blank) "+-" (0+ blank)
      (or
       (seq (org-goals-number-regexp margin) (0+ blank) "%")
       (org-goals-valu-regexp margin))))
    (0+ blank)
    )
   
   (lambda (goal-str)
     (make-org-goals-goal 
      :numer-min (car numerator)
      :numer-max (cdr numerator)
      :denom denominator
      :polarity polarity
      :margin margin
      :priority priority
      :text goal-str
      :ratio-word ratio-word)))
  "value ratio goal")

(defconst org-goals-goal-link-regexp
  (rxx (seq (org-goals-number-regexp factor)
	    (1+ blank)
	    "of"
	    (1+ blank)
	    (eval-regexp org-any-link-re))
       factor))

(defconst org-goals-goal-or-link-regexp
  (rxx (or (org-goals-goal-regexp goal)
	   (org-goals-goal-link-regexp link))
       (or goal link)))

(defun org-goals-parse-goal-or-link-at-point ()
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
    (let ((result (rxx-parse-fwd org-goals-goal-or-link-regexp (point-at-eol) 'partial-match-ok)))
      (dbg result)
      (if (org-goals-goal-p result) result
	(save-excursion
	  (save-window-excursion
	    (org-open-at-point 'in-emacs)
	    (unless (rxx-search-fwd org-goals-goal-prefix-regexp (point-at-eol))
	      (error "No goal found at link taget"))
	    (org-goals-scale-goal result (org-goals-parse-goal-or-link-at-point)))
  )))))

(provide 'org-goals)


;; test

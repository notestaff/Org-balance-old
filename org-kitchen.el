;;
;; Random bits of code -- not meant to be evaluatable as a buffer.
;;

(defun org-balance-show-here ()
  (interactive)
  (let ((org-balance-neglect-val (get-text-property (point) :org-balance-neglect)))
    (message "%s" org-balance-neglect-val)
    org-balance-neglect-val))

(defun test-org-sum ()
  (interactive)
  (org-balance-sum-property "val" :val-sum)
  (message "done summing"))

(defun org-balance-show-here ()
  (interactive)
  (let* ((time1 (org-float-time
                 (org-read-date nil 'to-time nil "Starting date: ")))
         (time2 (org-float-time (org-current-time))))
    (org-balance-sum-property "val" :val-sum 0 time1 time2)
    (message "done summing"))
  )

(defun org-do-sum ()
  "Call org-balance-sum-property with given args"
  (interactive)
  (org-balance-sum-property  :mysum "pnew"))


(defun org-try ()
  (interactive)

  (let* (

	 (time1 (org-float-time
                 (org-read-date nil 'to-time nil "Start date: ")))
         (time2 (org-float-time (org-current-time)))

	 (dummy (org-balance-sum-local time1 time2))

	 (org-balance-remove-overlays)
	 
	 (check-entry (lambda ()
			(let* ((time-str (org-entry-get nil "CLOSED"))
			       (clocksum (get-text-property (point) :org-clock-minutes-local)))

			  (when (and clocksum (> clocksum 0.0))
			    (org-show-context)
			    (org-balance-put-overlay clocksum (org-outline-level))
			    )
			  
			  (when (and nil time-str)

			    (let* ((time
				    (org-float-time
				     (apply 'encode-time
					    (org-parse-time-string time-str))))
				   (does-match (and (>= time time1) (<= time time2))))
			      (when does-match
				(org-show-context)
				(message "Got closed %s at %s, level %s, line %s"
					 time-str (point) (org-reduced-level (org-outline-level))
					 (line-number-at-pos))))))
			t)))
    (org-overview)
    (org-map-entries check-entry)))


(defun org-balance-check-neglected (&optional tstart tend)
     "Find neglected subtrees"
     (interactive)

     (when (not tstart) (setq tstart (org-float-time (org-read-date nil 'to-time nil "Start date: "))))
     (when (not tend) (setq tend (org-float-time (org-current-time))))
     
     (org-balance-calc-done tstart tend)

     (org-overview)
     (org-balance-remove-overlays)
     (when org-remove-highlights-with-change
       (org-add-hook 'before-change-functions 'org-balance-remove-overlays
		     nil 'local))
     
     (let ((days-in-interval (/ (- tend tstart) 60 60 24)))
       (org-map-entries
	(lambda ()
	  (let ((one-every (org-entry-get nil "one_every")))
	    (when one-every
	      (let ((sum-here (or (get-text-property (point) :balance-sum) 0)))
		(let ((need-here (* days-in-interval (/ 1.0 (float (string-to-number one-every))))))
		  (when (< sum-here need-here)
		    (org-show-context)
		    (org-balance-put-overlay (format "need: %.0f have: %.1f" need-here sum-here))
		    )))))))))


(defun org-balance-calc-done (&optional tstart tend)
     "Find amount of work done for each subtree"
     (interactive)
     (unless (eq major-mode 'org-mode) (error "need org-mode"))
     (message "Checking neglected")
     (org-balance-sum-local tstart tend)
     (org-balance-compute-entry-values (/ 1.0 60.0) "value" tstart tend :balance-local)
     (org-balance-sum-property :balance-local :balance-sum)
     (message "done sum-property"))

(defun org-balance-show-work (&optional tstart tend)
     "Show done and ongoing work for an interval"
     (interactive)
     (when (not tstart) (setq tstart (org-float-time (org-read-date nil 'to-time nil "Start date: "))))
     (when (not tend) (setq tend (org-float-time (org-current-time))))
     
     (org-balance-calc-done tstart tend)
     (org-overview)
     (org-balance-remove-overlays)

     (when org-remove-highlights-with-change
       (org-add-hook 'before-change-functions 'org-balance-remove-overlays
		     nil 'local))
     
     (org-map-entries
      (lambda ()
	(let ((sum-here (org-balance-get-property :balance-sum)))
	  (when (> sum-here 0)
	    (org-show-context)
	    (org-balance-put-overlay (format "%.1f" sum-here)))))))


(defun org-balance-compute-entry-values (time-factor closed-value-prop tstart tend store-as-prop)
  "For each entry, compute its contribution as"
  (interactive)
  
  (let* ((bmp (buffer-modified-p))
	 (check-entry
	  (lambda ()
	    (let* ((time-str (org-entry-get nil "CLOSED"))
		   (time (if (not time-str) -1
			   (org-float-time
			    (apply 'encode-time
				   (org-parse-time-string time-str)))))
		   (is-closed-todo (and time-str (>= time tstart) (<= time tend)))
		   (closed-todo-value (or (and closed-value-prop
					       (if (org-entry-get nil closed-value-prop)
						   (string-to-number
						    (org-entry-get nil closed-value-prop))
						 nil))
					  1))
		   (entry-value (+ (* time-factor (or (get-text-property (point) :org-clock-minutes-local) 0))
				   (* closed-todo-value (if is-closed-todo 1 0)))))
	      (put-text-property (point) (point-at-eol) store-as-prop entry-value)))))
    (org-map-entries check-entry)
    (set-buffer-modified-p bmp)))

(defun org-balance-sum-local (&optional tstart tend)
  "Sum the clock times for each entry.
Puts the resulting times in minutes as the text property :org-clock-minutes-local on each headline.
TSTART and TEND can mark a time range to be considered.

Adapted from `org-clock-sum'.
"
  (interactive)
  (when (not tstart) (setq tstart (org-read-date nil 'to-time nil "Start date: ")))
  (when (not tend) (setq tend (org-current-time)))
  (let* ((bmp (buffer-modified-p))
	 (re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		     org-clock-string
		     "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	 (t1 0)
	 ts te dt
	 time)
    (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
    (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
    (if (consp tstart) (setq tstart (org-float-time tstart)))
    (if (consp tend) (setq tend (org-float-time tend)))
    (remove-text-properties (point-min) (point-max) '(:org-clock-minutes-local t))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward re nil t)
	(cond
	 ((match-end 2)
	  ;; Two time stamps
	  (setq ts (match-string 2)
		te (match-string 3)
		ts (org-float-time
		    (apply 'encode-time (org-parse-time-string ts)))
		te (org-float-time
		    (apply 'encode-time (org-parse-time-string te)))
		ts (if tstart (max ts tstart) ts)
		te (if tend (min te tend) te)
		dt (- te ts)
		t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1)))
	 ((match-end 4)
	  ;; A naked time
	  (setq t1 (+ t1 (string-to-number (match-string 5))
		      (* 60 (string-to-number (match-string 4))))))
	 (t ;; A headline
	  (goto-char (match-beginning 0))
	  (when (> t1 0)
	    (put-text-property (point) (point-at-eol) :org-clock-minutes-local t1)
	    (message "Set t1=%s at %s" t1 (line-number-at-pos)))
	  (setq t1 0)))))
    (set-buffer-modified-p bmp)))

(defun org-balance-sum (&optional tstart tend headline-filter)
  "Sum the values for each subtree.
Puts the resulting values in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.  HEADLINE-FILTER is a
zero-arg function that, if specified, is called for each headline in the time
range with point at the headline.  Headlines for which HEADLINE-FILTER returns
nil are excluded from the clock summation.

Adapted from `org-clock-sum'.
"
  (interactive)
  (let* ((bmp (buffer-modified-p))
	 (re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		     org-clock-string
		     "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	 (lmax 30)
	 (ltimes (make-vector lmax 0))
	 (t1 0)
	 (level 0)
	 ts te dt
	 time)
    (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
    (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
    (if (consp tstart) (setq tstart (org-float-time tstart)))
    (if (consp tend) (setq tend (org-float-time tend)))
    (remove-text-properties (point-min) (point-max)
                            '(:org-clock-minutes t
                              :org-clock-force-headline-inclusion t))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward re nil t)
	(cond
	 ((match-end 2)
	  ;; Two time stamps
	  (setq ts (match-string 2)
		te (match-string 3)
		ts (org-float-time
		    (apply 'encode-time (org-parse-time-string ts)))
		te (org-float-time
		    (apply 'encode-time (org-parse-time-string te)))
		ts (if tstart (max ts tstart) ts)
		te (if tend (min te tend) te)
		dt (- te ts)
		t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1)))
	 ((match-end 4)
	  ;; A naked time
	  (setq t1 (+ t1 (string-to-number (match-string 5))
		      (* 60 (string-to-number (match-string 4))))))
	 (t ;; A headline
	  ;; Add the currently clocking item time to the total
	  (when (and org-clock-report-include-clocking-task
		     (equal (org-clocking-buffer) (current-buffer))
		     (equal (marker-position org-clock-hd-marker) (point))
		     tstart
		     tend
		     (>= (org-float-time org-clock-start-time) tstart)
		     (<= (org-float-time org-clock-start-time) tend))
	    (let ((time (floor (- (org-float-time)
				  (org-float-time org-clock-start-time)) 60)))
	      (setq t1 (+ t1 time))))
	  (let* ((headline-forced
		  (get-text-property (point)
                                     :org-clock-force-headline-inclusion))
                 (headline-included
                  (or (null headline-filter)
                      (save-excursion
                        (save-match-data (funcall headline-filter))))))
	    (setq level (- (match-end 1) (match-beginning 1)))
	    (when (or (> t1 0) (> (aref ltimes level) 0))
	      (when (or headline-included headline-forced)
                (if headline-included
                    (loop for l from 0 to level do
                          (aset ltimes l (+ (aref ltimes l) t1))))
		(setq time (aref ltimes level))
		(goto-char (match-beginning 0))
		(put-text-property (point) (point-at-eol) :org-clock-minutes time)
                (if headline-filter
                    (save-excursion
                      (save-match-data
                        (while
                            (> (funcall outline-level) 1)
                          (outline-up-heading 1 t)
                          (put-text-property
                           (point) (point-at-eol)
                           :org-clock-force-headline-inclusion t))))))
	      (setq t1 0)
	      (loop for l from level to (1- lmax) do
		    (aset ltimes l 0)))))))
      (setq org-clock-file-total-minutes (aref ltimes 0)))
    (set-buffer-modified-p bmp)))




(defun* org-balance-compute-goal-deltas2 (&key goals tstart tend callback error-handler
					       match scope skip)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  ;; For each goal, make sure we have the sum of that goal under each node, stored as a text property.
  ;; (with an orgb- prefix).
  ;; (better to: regexp search for the goals, then for each of them narrow the file to that subtree
  ;; (using save-restriction) and compute the sums only there; also, when one goal is under another goal's subtree,
  ;; do only the outer subtree.  then this summing happens only on the relevant parts of the file.
  ;; note that orgmode already has commands to restrict to subtree etc.
  (mapc (lambda (goal)
	  (let* ((is-time (equal goal "time"))
		 (sum-goal (if is-time :org-clock-minutes (intern (concat goal "_sum")))))
					; For each subtree, compute the sum of GOAL under that subtree.
	    (if is-time
					; BUG: need to go into any archive files associated with the current file.
		(org-clock-sum tstart tend)
	      (org-balance-sum-property goal sum-goal tstart tend))))
	goals)

    (let ((days-in-interval (/ (float (- tend tstart)) 60.0 60.0 24.0)))
      (apply
       'org-map-entries
       (append
	(list 
	 (lambda ()
	   (condition-case err
	       (let* ((org-trust-scanner-tags t)
		      
		      (the-goal (org-entry-get nil (concat "goal_" goal))))
		 (when the-goal
		   (let* ((parsed-goal (org-balance-parse-valu-ratio the-goal))
			  (per-day-goal-here (float
					      (if is-time
						  (* org-balance-minutes-per-day
						     (org-balance-parse-time-fraction daily-goal))
						(org-balance-parse-frequency-per-day daily-goal))))
			  (total-here (or (get-text-property (point) sum-goal) 0))
			  (per-day-here (/ (float total-here) (float days-in-interval)))
			  (goal-delta (- per-day-here per-day-goal-here))
			  (goal-delta (if goal-delta-relative-p
					  (/ goal-delta (if (> per-day-goal-here 0) per-day-goal-here 1e-6))
					goal-delta))
			  
			  (per-day-here-in-their-units (org-balance-report-time-fraction daily-goal per-day-here))
			  (per-day-goal-in-their-units (substring daily-goal 0 (string-match "[[:space:]]" daily-goal)))
			  (their-units (substring daily-goal (string-match "[[:space:]]" daily-goal)))
			  )
		     (apply callback))))
	     (error
	      (if error-handler (apply error-handler (list err))
		(progn
		  (message "Error processing entry at %s: %s" (point) (error-message-string err))
		  (signal (car err) (cdr err ))
		  nil))))) match scope) skip)))))



(defmacro org-balance-replacing-function (fname new-func body)
  ;; Execute code, temporarily replacing a given function with a new one
  `(let ((old-func (symbol-function ,fname)))
     (fset ,fname ,new-func)
     ,body
     (fset ,fname old-func)))


(defun my-func ()
  1)

(org-balance-replacing-function 'my-func (lambda () 2) (message "got %s" (my-func)))



(let ((old-f (symbol-function 'my-func)))
  (fset 'my-func (lambda () 2))
  (message "here is %s" (my-func))
  (fset 'my-func old-f))



(defun org-balance-sum-property (prop sum-prop unit tstart tend)
  "Sum the specified text or org property PROP under each subtree, for entries
closed within given time range (TSTART to TEND),
and store nonzero results as a specified text or org property SUM-PROP.
The results are stored in terms of the unit UNIT.
"
  (let* ((bmp (buffer-modified-p))
	 (lmax 30)
	 (ltimes (make-vector lmax 0))
	 (t1 0)
	 (time 0)
	 (prop-default (concat prop "_default")))
    
    (org-balance-delete-property-globally sum-prop)
    ;; Scan backwards from the end of the file, stopping at each headline,
    ;; and accumulating the total under each subtree
    (save-excursion
      (goto-char (point-max))
      (save-match-data
	(while (re-search-backward "^\\(\\*+\\)[ \t]" nil t)
	  (let* ((level (- (match-end 1) (match-beginning 1)))
		 ;; Determine whether the value of prop at this entry
		 ;; should be added to the total: was this entry closed
		 ;; within the time range we're considering?
		 (closedStr (org-entry-get (point) "CLOSED"))
		 (closedTime (if closedStr (org-float-time (apply 'encode-time (org-parse-time-string closedStr))) nil))
		 (closedMatches (and closedStr (>= closedTime tstart) (<= closedTime tend)))
		 (t0 (if (not closedMatches) nil
		       (or (org-balance-get-property prop)
			   (org-entry-get nil prop-default 'inherit)
			   "1"))))
	    
	    (when t0 (setq t1 (+ t1 (org-balance-valu-val (org-balance-convert-valu (org-balance-parse-valu t0) unit)))))
	    (when (or (> t1 0) (> (aref ltimes level) 0))
	      (loop for l from 0 to level do
		    (aset ltimes l (+ (aref ltimes l) t1)))
	      (setq t1 0 time (aref ltimes level))
	      (loop for l from level to (1- lmax) do
		    (aset ltimes l 0))
	      (org-balance-set-property sum-prop time 0))))))
    (set-buffer-modified-p bmp)))


(defun* org-balance-compute-goal-deltas (&key goal goal-delta-relative-p tstart tend callback error-handler)
  "For each goal, determine the difference between the actual and desired average daily expenditure of
resource GOAL toward that goal in the period between TSTART and TEND.  Call the callback with the value.
"
  (unless tstart (setq tstart (org-float-time (org-read-date nil 'to-time nil "Interval start: "))))
  (unless tend (setq tend (org-float-time (org-current-time))))

  (let* ((is-time (equal goal "time"))
 	 (sum-goal (if is-time :org-clock-minutes (intern (concat goal "_sum")))))
    ; For each subtree, compute the sum of GOAL under that subtree.
    (if is-time
	; BUG: need to go into any archive files associated with the current file.
	(org-clock-sum tstart tend)
      (org-balance-sum-property goal sum-goal 'item tstart tend))

    (let ((days-in-interval (/ (float (- tend tstart)) 60.0 60.0 24.0)))
      (org-map-entries
       (lambda ()
	 (condition-case err
	     (let ((daily-goal (org-entry-get nil (concat "goal_" goal))))
	       (when daily-goal
		 (let* ((per-day-goal-here (float
					    (if is-time
						(* org-balance-minutes-per-day
						   (org-balance-parse-time-fraction daily-goal))
					      (org-balance-parse-frequency-per-day daily-goal))))
			(total-here (or (get-text-property (point) sum-goal) 0))
			(per-day-here (/ (float total-here) (float days-in-interval)))
			(goal-delta (- per-day-here per-day-goal-here))
			(goal-delta (if goal-delta-relative-p
					(/ goal-delta (if (> per-day-goal-here 0) per-day-goal-here 1e-6))
				      goal-delta))
			
			(per-day-here-in-their-units (org-balance-report-time-fraction daily-goal per-day-here))
			(per-day-goal-in-their-units (substring daily-goal 0 (string-match "[[:space:]]" daily-goal)))
			(their-units (substring daily-goal (string-match "[[:space:]]" daily-goal)))
			)
		   (apply callback (list goal-delta total-here per-day-here per-day-goal-here
					 per-day-here-in-their-units per-day-goal-in-their-units their-units)))))
	   (error
	    (if error-handler (apply error-handler (list err))
	      (progn
		(message "Error processing entry at %s: %s" (point) (error-message-string err))
		(signal (car err) (cdr err ))
		nil)))))))))



(defun org-balance-re-shy-group (&rest args)
  "Wrap a shy group around the given regexp, and return the result."
  (concat "\\(?:" (apply 'concat args) "\\)"))

(defun org-balance-re-group (&rest args)
  "Wrap a group around the given regexp, and return the result."
  (concat "\\(" (apply 'concat args) "\\)"))

(defun org-balance-re-or (&rest args)
  "Construct a regular expression for matching any of the given regexps.  Wraps each of them, and the entire
expression, in a shy group for safety."
  (org-balance-re-shy-group (mapconcat 'org-balance-re-shy-group args "\\|")))

(defconst org-balance-number-regexp0
  (org-balance-re-shy-group
   "\\s-*"   ; allow for leading whitespace
   "[+\\-]?" ; optional sign
   ; either NNN.NNN or NNN. or .NNN
   (org-balance-re-or
    (concat "[[:digit:]]+" "\\.?[[:digit:]]*")
    (concat "[[:digit:]]*\\.?" "[[:digit:]]+"))
   ;; optional exponent
   (org-balance-re-shy-group
    (org-balance-re-shy-group
     "[eE]"
     "[+\\-]?"
     "[[:digit:]]+")
    "?")
   "\\s-*"   ; allow for trailing whitespace
   )
  "Regular expression for an arbitrary floating-point number, possibly surrounded by whitespace."
  )

(defconst
  org-balance-valu-regexp
  (org-balance-re-or
   (org-balance-re-or
    (concat "\\(?1:" org-balance-number-regexp "\\)?" "\\s-*"
	    "\\(?2:" org-balance-unit-regexp "\\)"  ))
   (org-balance-re-or
    (concat "\\(?1:" org-balance-number-regexp "\\)" "\\s-*"
	    "\\(?2:" org-balance-unit-regexp "\\)?"  ))))

(defconst org-balance-valu-ratio-regexp
  (concat "\\(?1:" (org-balance-re-make-shy org-balance-valu-regexp) "\\)" org-balance-ratio-words
	  "\\(?2:" (org-balance-re-make-shy org-balance-valu-regexp) "\\)"))

(defun org-balance-parse-valu-ratio3 (valu-ratio-str)
  (save-match-data
    (when (string-match org-balance-valu-ratio-regexp valu-ratio-str)
      (cons (org-balance-parse-valu (match-string 1 valu-ratio-str))
	    (org-balance-parse-valu (match-string 2 valu-ratio-str))))))


(defun org-balance-re-make-shy-aux-1 (re)
  "Make all groups in regexp RE shy"
  (save-match-data
    (replace-regexp-in-string
     (rx
      (seq "\\(" (not (any "?")) (not (any ":"))))
     (lambda (match)
       (concat  "\\(?:"
		(substring match
			   (- (length match) 2))))
     re
     'fixedcase 'literal)))

(defun org-balance-re-make-shy-aux-2 (re)
  "Make all groups in regexp RE shy"
  (save-match-data
     (replace-regexp-in-string
      (rx
       (seq "\\(?" (one-or-more (any digit)) ":"))
      "\\(?" re 'fixedcase 'literal)))

(defun org-balance-re-make-shy (re)
  "Make all groups in regexp RE shy"
   (org-balance-re-make-shy-aux-2 re))


(defun org-balance-parse-valu-ratio (ratio-str)
  "Parse a string describing a ratio of two valu's: e.g. '3 hours a day' or '100 dollars per month'."
  (let ((parts (split-string ratio-str org-balance-ratio-words)))
    (make-org-balance-valu-ratio
     :num (org-balance-parse-valu (first parts)) :denom (org-balance-parse-valu (second parts))
     :ratio-word (save-match-data (string-match org-balance-ratio-words ratio-str) (match-string 0 ratio-str)))))

(defconst org-balance-minutes-per-day (* 60 24))
(defconst org-balance-unit2minutes
  `((second . 0.0166666666667) (seconds . 0.0166666666667) (minute . 1) (minutes . 1)
    (min . 1) (mins . 1)
    (hour . 60) (hours . 60) (hr . 60) (hrs . 60) (day . ,org-balance-minutes-per-day)
    (days . ,org-balance-minutes-per-day) (week . 10080) (weeks . 10080)
    (month . 43200) (months . 43200) (year . 525600) (years . 525600)
    (bluemoon 1e12))
 "Number of minutes in each unit" )

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

;;
;; so, i can take the regexp form and call rx-to-string on it
;; (after first binding rx-constituents to our extensions in a let).
;; during that time, there will be callbacks to our rxx-named-xregexp forms.
;;
;; our routine for handling this form will:
;;   - generate a group number for wrapping what matched this form
;;   - add 
;;
;;
;;

(defconst org-balance-clock-time-xregexp
  (rxx
   '(seq "[" (one-or-more anything) "]")
   (lambda (time-str)
     (org-float-time (apply 'encode-time (org-parse-time-string time-str)))))
  "A clock time, e.g. [2010-03-17 Wed 16:27]")
	       
(defconst org-balance-clock-range-xregexp
  (rxx
   `(seq
     (zero-or-more whitespace)
     ,org-clock-string (one-or-more whitespace)
     
     (named-grp clock-range-start ,org-balance-clock-time-xregexp) "--"
     (named-grp clock-range-end ,org-balance-clock-time-xregexp)
     
     ;(one-or-more whitespace) "=>" (one-or-more whitespace)
     ;(shy-grp ,time-duration-xregexp)
     )
   
   (lambda (clock-range-str)
     (list (rxx-match-val 'clock-range-start)
	   (rxx-match-val 'clock-range-end))))
  "A clock time range, e.g. [2010-03-17 Wed 16:27]--[2010-03-18 Thu 12:42] => 20:15")

(defconst rxx-int-xregexp (rxx '(one-or-more digit) 'string-to-number))

(setq z "123")

(defconst rxx-interval-xregexp (rxx `(seq
				      (named-grp beg ,rxx-int-xregexp)
				      "-"
				      (named-grp end ,rxx-int-xregexp))))

(message "%s" rxx-interval-xregexp)

(when nil
  (progn
    (setq z "CLOCK: [2009-11-01 Sun 14:34]--[2009-11-02 Mon 11:51]")
    (string-match org-balance-clock-range-xregexp z)
    (rxx-match-val 'clock-range-end z org-balance-clock-range-xregexp)))

(string-match org-balance-clock-time-xregexp "[to]")
		  
(defun rxx-match-string (grp-name &optional object xregexp)

  ;; so, if this group stands for just a group, then, just return a string.
  ;; but, if this group stands for 

  (save-match-data
    (let* ((rxx-obj (or object (when (boundp 'rxx-obj) rxx-obj)))
	   (rxx-env (if xregexp (rxx-info-env (get-rxx-info xregexp))
		      rxx-env))
	   (grp-info (cdr (assq grp-name rxx-env))))
      (match-string (rxx-info-num grp-info) rxx-obj))))

;; so, what needs to happen is that when we make this recursive call,
;; we need to make 
;;
;; so, the macro would save the regexp as a string,
;; but would also save the original form, so that if we need to construct
;; a regexp with this subexpr, we can.
;; 


;; also make a macro that just constructs an expression for local use,
;; e.g. within a let, rather than necessarily defining a global constant.


;; so, what needs to happen is that when we make this recursive call,
;; we need to make 
;;
;; so, the macro would save the regexp as a string,
;; but would also save the original form, so that if we need to construct
;; a regexp with this subexpr, we can.
;; 


;; also make a macro that just constructs an expression for local use,
;; e.g. within a let, rather than necessarily defining a global constant.

	 ;; var: rxx-grp-num-counts - for each explicit group number
	 ;;   allocated to a named group, how many times was it used?
	 ;;   we use this to detect collisions between explicit group
	 ;;   numbers generated by us in `rxx-process-named-grp',
	 ;;   and any explicit group numbers used in other parts of
	 ;;   the expression.
	 (rxx-grp-num-counts (make-rxx-dyn-vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils for working with dynamically sized vectors. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* make-rxx-dyn-vec (&optional (init-size 16) (init-elem 0))
  "Allocate a (conceptually) infinite-size vector with default value
of elements set to INIT-ELEM.  Physically, a vector of size INIT-SIZE
is initially allocated, and grown as needed.  Any elements not explicitly
set with `rxx-dyn-vec-aset' (defined below) have the value INIT-ELEM when
read by `rxx-dyn-vec-aref'."
  (cons (make-vector init-size init-elem) init-elem))

(defun rxx-dyn-vec-aref (rxx-dyn-vec index)
  "Read the value from the dynamic vector RXX-DYN-VEC at the index INDEX.
Any elements not explicitly set by `rxx-dyn-vec-aset' return the INIT-ELEM value
given at array construction."
  (if (>= index (length (car rxx-dyn-vec))) (cdr rxx-dyn-vec)
    (aref (car rxx-dyn-vec) index)))

(defun rxx-dyn-vec-aset (rxx-dyn-vec index object)
  "Set the INDEX'th element of dynamic vector RXX-DYN-VEC to OBJECT,
growing the physical representation of the vector as needed."
  (let ((cur-len (length (car rxx-dyn-vec))))
    (when (>= index cur-len)
      (setcar rxx-dyn-vec (vconcat (car rxx-dyn-vec)
				   (make-vector (- (round (expt 2 (fceiling (log (1+ index) 2))))
						   cur-len)
						(cdr rxx-dyn-vec)))))
    (aset (car rxx-dyn-vec) index object)))

(defsetf rxx-dyn-vec-aref rxx-dyn-vec-aset)

(defconst brk ")")

(let* ((number-regexp (rxxm (one-or-more digit) string-to-number))
       (fraction-regexp (rxx '(seq (named-grp num number-regexp) "/" (named-grp denom number-regexp))
			     '(cons num denom)))
       (paren-regexp (rxxm (seq "(" (named-grp val fraction-regexp) (eval brk)) val))
       (range-regexp (rxx '(seq "[" (named-grp rmin paren-regexp) "]--[" (named-grp rmax paren-regexp) "]") '(list rmin rmax))))
  (rxx-parse range-regexp "[(1/2)]--[(3/4)]"))

(rxxlet* ((number-regexp (one-or-more digit) string-to-number)
	  (fraction-regexp (seq (named-grp num number-regexp) "/" (named-grp denom number-regexp))
			   (cons num denom))
	  (paren-regexp (seq "(" (named-grp val fraction-regexp) ")") val)
	  (range-regexp (seq "[" (named-grp rmin paren-regexp) "]--[" (named-grp rmax paren-regexp) "]")
			(list rmin rmax)))
	 (rxx-parse range-regexp "[(1/2)]--[(3/4)]"))


(defmacro rxxlet* (bindings forms)
  (list 'let* (mapcar (lambda (binding) (list (first binding) (list 'rxx (second binding) (third binding) (symbol-name (first binding)))))
		      bindings)
	forms))

(let ((rxx-disable-grps '(polarity atleast atmost)))
  (assert (member 'polarity rxx-disable-grps))
  (rxx-parse (rxx-to-string (rxx-info-form (get-rxx-info rxx-valu-ratio-goal-regexp))
			    (rxx-info-parser (get-rxx-info rxx-valu-ratio-goal-regexp))) "at leastttt once a month"))


(defconst rxx-clock-range-regexp2
  (rxx (seq (0+ whitespace) (eval rxx-clock-string) (0+ whitespace) (named-grp from rxx-clock-regexp) (1+ "-")
	    (named-grp to rxx-clock-regexp :replace left-bracket (named-backref (.. from left-bracket)))) (cons from to)))


(defun rxx-parse-recurs (aregexp s max-recurs-depth &optional partial-match-ok)
  (let* ((rxx-recurs-depth max-recurs-depth)
	 (unwound-aregexp (rxx-to-string `(named-grp top-grp
						     ,aregexp))))
    (rxx-parse (rxx-to-string unwound-aregexp) s partial-match-ok)
  ))


(defun show-next () (interactive) (let ((point-now (point))) (save-excursion (forward-line) (outline-flag-region point-now (point-at-eol) nil))))

(defconst org-balance-goal-option-regexp (rxx (seq (named-grp name (1+ alphanumeric)) ":" (0+ whitespace) (named-grp val (1+ not-newline))) (cons name val)))

(optional
 (seq-separated-by (0+ ws)
  "("
  (1+-separated-by (seq ";" (0+ ws))
		   (org-balance-goal-option-regexp props)
		   )
  ")"))

so, as long as prop is just priority, can specify that there's optional priority.
but in general, these would be useful additions: seq-separated-by, and 1+-separated-by (or repeat-separated-by).

or, more generally (separated-by (0+ ws) ... ) where ... is either seq or a repeat command.
also, would be good if ws denoted whitespace.

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
	      
	      (let* (
		     (goal-name-here (org-match-string-no-properties 1))
		     (goal-prop-here (substring goal-name-here (length org-balance-goal-prefix)))
		     (goal-def-here (org-match-string-no-properties 2))
		     (parsed-goal
		      (condition-case err
			  ;(rxx-parse org-balance-valu-ratio-goal-regexp goal-def-here)
			  (org-balance-parse-goal-or-link-at-point goal-name-here)
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



(let* ((exp (rxx (or (recurse (seq (named-grp op (or "+" "-" "*" "/")) (exp e1) (exp e2)))
		     (named-grp d digit) ) (list (safe-val op) (safe-val e1) (safe-val e2) (safe-val d))))
       (rxx-recurs-depth 3)
       (dummy (message "now the recurs part"))
       (expr (rxx exp (list (safe-val op) (safe-val e1) (safe-val e2) (safe-val d)))))
(rxx-parse expr "*3+12")
;(message "%s" expr)
;(rxx-info-form (get-rxx-info exp))
)
(rxx (or "hi" (eval-regexp rxx-never-match)))
(string-match "\\(?:[[:digit:]]\\|\\(?:\\(?:[[:digit:]]\\)[*+/-]\\(?:[[:digit:]]\\)\\)\\)" "")

(string-match "\(?:[[:digit:]]\|\(?:\(?:[[:digit:]]\)[*+/-]\(?:[[:digit:]]\)\)\)\)\)\)[*+/-]\(?:\(?:\(?:\(?:[[:digit:]]\|\(?:\(?:[[:digit:]]\)[*+/-]\(?:[[:digit:]]\)\)\)" 


(defadvice rx-to-string (around rx-show-args last (form &optional no-group) activate compile)
  (dbg "in  rx-to-string" form (safe-val rxx-recurs-depth) (safe-val rxx-env))
  ad-do-it
  (dbg "out rx-to-string" ad-return-value)) 

(defadvice rxx-process-named-grp (around rx-show-proc-named last (form) activate compile)
  (dbg "in  rxx-process-named-grp" form (safe-val rxx-recurs-depth) (safe-val rxx-env))
  ad-do-it
  (dbg "out rxx-process-named-grp" ad-return-value (safe-val rxx-recurs-depth) (safe-val rxx-env))) 

(defadvice rxx-to-string (around rxx-show (form &optional parser descr) activate compile)
  (dbg "in  rxx-to-string" form)
  ad-do-it
  (dbg "out rxx-to-string" ad-return-value))
(ad-deactivate 'rx-to-string)


(let* ((exp (rxx (or (named-grp bb (recurse (named-grp sub (seq "(" (named-grp op (or "+" "-" "*" "/")) " " (exp e1) " " (exp e2) ")"))))
		     (named-grp d digit) ) (list (safe-val sub) (safe-val op) (safe-val e1) (safe-val e2) (safe-val d))))
       (rxx-recurs-depth 1)
       (dummy (message "now the recurs part"))
       (expr (rxx exp (lambda (full) (list full (safe-val bb) (safe-val sub) (safe-val op) (safe-val e1) (safe-val e2) (safe-val d)))))
       (s "(* 3 4)"))
(rxx-parse expr s 'part-ok)
;(message "%s" expr)
;(rxx-info-form (get-rxx-info exp))
;expr
)

(let* ((exp (rxx (or (named-grp sub (recurse (seq "(" exp (named-grp op (or "+" "-" "*" "/")) exp ")"))) (named-grp d digit)) (cons sub d)))
       (rxx-recurs-depth 2)
       (expr (rxx exp (lambda (full) (list full sub d (rxx-match-val '(sub sub op)) (rxx-match-val '(sub op))))))
       (s "(3+(4*5))"))
  (rxx-parse expr s 'part-ok))

(let* ((num (rxx (1+ digit) string-to-number))
       (exp (rxx (or (named-grp sub (recurse (seq "(" (exp a) (named-grp op (or "+" "-" "*" "/")) (exp b) ")"))) (num d)) (if (boundp 'a) (list a b d) (list d))))
       (rxx-recurs-depth 2)
       (expr (rxx exp (lambda (full) (list full sub d  (rxx-match-val '(sub op))))))
       (s "(30+(42*57))"))
  (rxx-parse expr s 'part-ok))


(let* ((num (rxx (1+ digit) string-to-number))
       (exp (rxx (or (named-grp sub (recurse (seq "(" (exp a) (named-grp op (or "+" "-" "*" "/")) (exp b) ")"))) (num d)) (if (boundp 'a) (list a b d) (list d))))
       (rxx-recurs-depth 3)
       (expr (rxx exp (lambda (full) (list full sub d  (rxx-match-val '(sub op))))))
       (s "(1/(30+(42*57)))"))
  (rxx-parse expr s 'part-ok))


(defun ppp (x) "my func" (message "hi: %s" x))

(let ((orig-pp (symbol-function 'ppp)))
  (flet ((ppp (x) (funcall orig-pp (format "bye -- %s" x))))
    (ppp 3))
  (ppp 4))

(bindings &rest body)

(defmacro rxx-flet (bindings &rest body)
  "Temporarily replace functions, making previous definitions available."
  `(let 
       ,(mapcar (lambda (binding) (list (intern (concat (symbol-name (first binding)) "-orig")) (list 'symbol-function (list 'quote (first binding))))) bindings)
     (flet ,(append bindings (mapcar
			      (lambda (binding)
				(let ((orig-fn (intern (concat (symbol-name (first binding)) "-orig"))))
				  (list orig-fn '(&rest args) `(apply (symbol-value (quote ,orig-fn)) args))))
			      bindings))
       ,@body)))



(rxx-flet ((ppp (x) (+ x x)))
  (ppp-orig 4)
  (ppp 4)
  )

(let ((x 1))
  (message "%s" x)
  )

(defadvice org-entries-lessp (around rxx-lessp (a b) activate compile)
  (dbg "LESSP" a b org-agenda-sorting-strategy-selected)
  ad-do-it
  (dbg "LESSP" ad-return-value))

(defadvice org-cmp-priority (around rxx-lessp (a b) activate compile)
  (dbg "cmp-priority" a b)
  ad-do-it
  (dbg "cmp-priority" ad-return-value))



(let (org-agenda-sorting-strategy-selected
      (org-agenda-sorting-strategy '((tags user-defined-up))))
  (org-set-sorting-strategy 'tags)
  org-agenda-sorting-strategy-selected


(ad-disable-advice 'org-entries-lessp 'around 'rxx-lessp)

(defun show-prefix (x)
  (interactive "P")
  (message "%s" x))

)

(defun org-make-tags-matcher2 (match)
  (rxx-let*
   ((tagname-re (1+ (any alnum "_")))
    (tagcond-re (seq (optional "+-") tagname-re))
    (propname-re (1+ (any alnum "_-")))
    (prop-cond-re (or (seq propname-re op-re val-re)
		      (seq propname-re "={" regexp-re "}")))
    (matcher-re (separated-by "|" (0+ or-clause-re)))
  )
   ))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(delete-selection-mode nil nil (delsel))
 '(doc-view-cache-directory "/wga/scr1/ilya/tmp/doc-view/")
 '(edebug-all-defs t)
 '(edebug-all-forms t)
 '(fill-column 123)
 '(message-log-max 10000)
 '(org-agenda-custom-commands (quote (("g" . "org-balance commands") ("gn" "Neglected items" tags "goal_delta_val<0/!GOAL" ((org-agenda-sorting-strategy (quote (priority-down category-keep user-defined-up))) (org-agenda-cmp-user-defined (quote org-balance-cmp)) (org-agenda-before-sorting-filter-function (quote org-balance-save-amt-neglected)) (org-show-hierarchy-above (quote ((agenda . t)))))))))
 '(org-agenda-dim-blocked-tasks (quote invisible))
 '(org-agenda-files (quote ("/cvar/sabeti-dav/ilya/ilya/new/mythings.org")))
 '(org-agenda-start-with-follow-mode t)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.6)))
 '(org-balance-agenda-sorting-strategy (quote (priority-down category-keep user-defined-down)))
 '(org-balance-default-polarity (quote (("clockedtime" . atleast) ("done" . atleast) ("spend" . atmost))))
 '(org-clock-into-drawer 2)
 '(org-clock-report-include-clocking-task t)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "NOTES" "LOGBOOK")))
 '(org-log-done (quote time))
 '(org-sparse-tree-open-archived-trees t)
 '(org-todo-keywords (quote ((sequence "TODO" "DONE") (sequence "GOAL" "|" "DONE"))))
 '(scroll-bar-mode (quote right))
 '(tab-width 3)
 '(visible-bell t)
 '(w3m-content-type-alist (quote (("application/x-gzip" "\\.gz\\'" ("/usr/bin/zcat" file) nil) ("text/plain" "\\.\\(txt\\|tex\\|el\\)\\'" nil nil) ("text/html" "\\.s?html?\\'" browse-url-default-browser nil) ("text/xml" "\\.xml\\'" nil "text/plain") ("image/jpeg" "\\.jpe?g\\'" ("/usr/bin/display" file) nil) ("image/png" "\\.png\\'" ("/usr/bin/display" file) nil) ("image/gif" "\\.gif\\'" ("/usr/bin/display" file) nil) ("image/tiff" "\\.tif?f\\'" ("/usr/bin/display" file) nil) ("image/x-xwd" "\\.xwd\\'" ("/usr/bin/display" file) nil) ("image/x-xbm" "\\.xbm\\'" ("/usr/bin/display" file) nil) ("image/x-xpm" "\\.xpm\\'" ("/usr/bin/display" file) nil) ("image/x-bmp" "\\.bmp\\'" ("/usr/bin/display" file) nil) ("video/mpeg" "\\.mpe?g\\'" nil nil) ("video/quicktime" "\\.mov\\'" nil nil) ("application/postscript" "\\.e?ps\\'" ("gv" file) nil) ("application/pdf" "\\.pdf\\'" ("acroread" file) nil) ("application/xml" "\\.xml\\'" nil "text/plain") ("application/rdf+xml" "\\.rdf\\'" nil "text/plain") ("application/rss+xml" "\\.rss\\'" nil "text/plain") ("application/xhtml+xml" nil nil "text/html"))))
 '(w3m-cookie-accept-domains (quote ("livejournal.com"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((nil (:foreground "yellow"))))
 '(font-lock-string-face ((nil (:foreground "MediumSeaGreen"))))
 '(org-hide ((t (:foreground "white")))))

(let ((s (make-string 100 ?.)))
  (put-text-property 2 15 'a 1 s)
  (put-text-property 9 20 'b 3 s)
  (text-properties-at 10 s)
  )


(defstruct org-balance-intervals intervals sums str seen seen-id imin imax chunk-size)

(defun new-org-balance-interval-set (intervals)
  "Construct an interval set"
  (let* ((str (make-string 1024 ?.))
	 (imin (apply 'min (mapcar 'car intervals)))
	 (imax (apply 'max (mapcar 'cdr intervals)))
	 (chunk-size (/ (- imax imin) (length str)))
	 (interval-id 0))
    (dolist (interval intervals)
      (put-text-property (floor (- (car interval) imin) chunk-size)
			 (ceiling (- (cdr interval) imin) chunk-size)
			 (intern (format "i%d" interval-id))
			 interval-id str)
			 
      )
    (make-org-balance-intervals :intervals intervals :sums (make-vector (length intervals) 0) :str str
			      :seen (make-vector (length intervals) 0) :seen-id 0 :imin imin :imax imax
			      :chunk-size chunk-size)
  ))

(defun org-balance-add-to-intervals (intervals tmin tmax val)
  "Add value VAL to running sums for intervals intersecting the range [TMIN,TMAX] in INTERVAL-SET"
  (incf (seen-id interval-set))
  (let* ((pos-min (floor (- tmin (org-balance-intervals-imin intervals) (org-balance-intervals-chunk-size intervals))))
	 (pos-max (ceiling (- tmax (org-balance-intervals-imin intervals) (org-balance-intervals-chunk-size intervals))))
	 (tpos tmin)
	 (tps (text-properties-at tmin (org-balance-intervals-str intervals))))
    (while (<= tpos tmax)
      (
      )
  ))


(defun bsearch-impl (vec target left right)
  (let ((mid (/ (+ left right) 2)))
    (cond ((eq left right)
           (if (eq target (elt vec left))
               left
             -1))
          ((<= target (elt vec mid))
           (bsearch-impl vec target left mid))
          (t ; target > (elt vec mid)
           (bsearch-impl vec target (+ 1 mid) right)))))

(defun bsearch (vec elt)
  "binary search VEC for ELT, returning index, or -1 if not found"
  (bsearch-impl vec elt 0 (1- (length vec))))

(let ((v (make-vector 10 0)))
  (aset v 5 3)
  (sort* v '<)
  (message "%s" v)
  (bsearch v 3)
  )


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
	 (goal-deltas (org-balance-groupby goal-deltas-orig 'org-balance-goal-delta-entry-pos)))
    (org-balance-remove-overlays)
    (org-overview)
    (dolist (entry-goal-delta goal-deltas)
      (goto-char (org-balance-goal-delta-entry-pos (cadr entry-goal-delta)))
      (org-balance-put-overlay
       (mapcar
	(lambda (goal-delta)
	  (format "%4d%% \"%20s\" actual: %.2f"
		  (round (org-balance-goal-delta-delta-percent goal-delta))
		  (org-balance-goal-text (org-balance-goal-delta-goal goal-delta))
		  (org-balance-valu-val (org-balance-valu-ratio-num (org-balance-goal-delta-actual goal-delta)))))
	(cdr entry-goal-delta)))
      (org-show-context 'default))))


(defun org-balance-check3 (&optional tstart tend)
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
    (rxx-flet ((org-make-tags-matcher (match) (cons "org-balance" nil))
	       (org-prepare-agenda (title) (org-prepare-agenda-orig "org-balance"))
	       ;; set redo command
	       (org-scan-tags
		(&rest args)
		(mapcar
		 (lambda (goal-delta)
		   (goto-char (org-balance-goal-delta-entry-pos goal-delta))
		   (let ((txt
			  (org-format-agenda-item
			   nil
			   (or
			    (org-balance-goal-delta-error-msg goal-delta)
			    (format "%-40s %+4d%% %20s actual: %.2f"
				    (org-balance-goal-delta-heading goal-delta)
				    (round (org-balance-goal-delta-delta-percent goal-delta))
				    (org-balance-goal-text (org-balance-goal-delta-goal goal-delta))
				    (org-balance-valu-val (org-balance-valu-ratio-num
							   (org-balance-goal-delta-actual goal-delta)))))
			   ))
			 (marker (org-agenda-new-marker)))
		     (org-add-props txt props 'org-marker marker 'org-hd-marker marker 'org-category (org-get-category)
				    'priority (org-get-priority
					       (or
						(org-balance-goal-priority (org-balance-goal-delta-goal goal-delta))
						(org-get-heading))) 'type "org-balance"
						'org-balance-goal-delta goal-delta)))
		 (org-balance-compute-goal-deltas2 :tstart tstart :tend tend)))
	       )
      (let (org-agenda-before-sorting-filter-function
	    org-agenda-sorting-strategy-selected
	    (org-agenda-cmp-user-defined
		(lambda (a b)
		  (let ((va (org-balance-goal-delta-delta-percent (get-text-property 0 'org-balance-goal-delta a)))
			(vb (org-balance-goal-delta-delta-percent (get-text-property 0 'org-balance-goal-delta b))))
		    (if (< va vb) -1 +1)
		    )))
	    (org-agenda-sorting-strategy `((tags ,@org-balance-agenda-sorting-strategy))))
	(org-tags-view)))))

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

(defun* org-balance-compute-goal-deltas (&key goals tstart tend)
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
	  (let ((org-balance-num-warnings 0) goal-deltas)
	    ;; FIXME if goals specified, make regexp for them
	    (let (goal-name-here)
	      (while (setq goal-name-here
			   (rxx-search-fwd
			    org-balance-goal-prefix-regexp (not 'bound) 'no-error))
		(let* ((goal-def-here (buffer-substring (point) (point-at-eol)))
		       (parsed-goal
			(condition-case err
			    (org-balance-parse-goal-or-link-at-point)
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
			    (when (string= (upcase (org-get-heading)) "GOALS")
			      (outline-up-heading 1 'invisible-ok))
			    (org-narrow-to-subtree)
			    (goto-char (point-min))
			    (let* ((is-time (equal goal-name-here "clockedtime"))
				   (to-unit (if is-time 'minutes
					      (org-balance-valu-unit
					       (org-balance-goal-numer-min parsed-goal))))
				   (sum-here
				    (if is-time (org-balance-clock-sum tstart tend)
				      (org-balance-sum-property
				       goal-name-here
				       to-unit
				       tstart tend))))
			      (let ((actual
				     (org-balance-convert-valu-ratio
				      (make-org-balance-valu-ratio
				       :num
				       (org-balance-make-valu sum-here to-unit)
				       :denom days-in-interval)
				      (make-org-balance-valu-ratio
				       :num (org-balance-goal-numer-min parsed-goal)
				       :denom (org-balance-goal-denom parsed-goal)))))
				
				(let* ( (polarity (or (org-balance-goal-polarity parsed-goal)
						      (cdr (assoc-string goal-name-here org-balance-default-polarity))))
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
					
					(actual-num (org-balance-valu-val (org-balance-valu-ratio-num actual))))
				  
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
  
;; struct: org-balance-goal-delta - information about how well one goal is being met.
;;      `org-balance-compute-goal-deltas' gathers this information from various entries and
;;      presents it in a list.
(defstruct org-balance-goal-delta heading goal entry-buf entry-pos goal-pos actual delta-val delta-percent error-msg)

(defmacro org-balance-replacing-function (fname new-func body)
  ;; Execute code, temporarily replacing a given function with a new one
  `(let ((old-func (symbol-function ,fname)))
     (unwind-protect
	 (progn
	   (fset ,fname ,new-func)
	   ,body)
       (fset ,fname old-func))))


(defun org-balance-make-agenda-custom-commands ()
       '(("b" "org-balance: neglected items" tags "val_goal>=0"
	  ((org-agenda-sorting-strategy '(user-defined-down))
	   (org-agenda-cmp-user-defined 'org-balance-cmp)
	   (org-agenda-before-sorting-filter-function 'org-balance-save-amt-neglected)))))

(setq org-agenda-custom-commands (org-balance-make-agenda-custom-commands))
	
(rxx-parse org-balance-clock-regexp "			 CLOCK: [2010-09-21 Tue 11:38]--[2010-09-22 Wed 00:13] => 12:35")

(defun rxx-qqq (x) (message "y=%s" x))


(let ((rxx-ppp-orig (when (fboundp (quote rxx-ppp)) (symbol-function (quote rxx-ppp)))))
  (flet ((rxx-ppp (&rest args) (apply rxx-qqq args))
	 (rxx-ppp-orig (&rest args) (apply (symbol-value (quote rxx-ppp-orig)) args))) (rxx-ppp 44)))


(let ((rxx-ppp-orig
       (when (fboundp (quote
		       rxx-ppp))
	 (symbol-function (quote rxx-ppp)))))
  (flet ((rxx-ppp
	  rxx-qqq)
	 (rxx-ppp-orig (&rest args) (apply (symbol-value (quote
							  rxx-ppp-orig)) args))) (rxx-ppp 44)))


(let ((rxx-ppp-orig (when (fboundp (quote rxx-ppp)) (symbol-function (quote rxx-ppp))))) (flet ((rxx-ppp (&rest args) (apply (quote rxx-qqq) args)) (rxx-ppp-orig (&rest args) (apply (symbol-value (quote rxx-ppp-orig)) args))) (rxx-ppp 44)))


(defun rx-check-any (arg)
   "Check arg ARG for Rx `any'."
   (cond
    ((integerp arg) (list arg))
    ((symbolp arg)
     (let ((translation (condition-case nil
			    (rx-form arg)
			  (error nil))))
       (message "translation is %s" translation)

       (if (or (null translation)
	       (null (string-match "\\`\\[\\[:[-a-z]+:\\]\\]\\'" translation)))
	   (error "Invalid char class `%s' in Rx `any'" arg))
       (list (substring translation 1 -1)))) ; strip outer brackets
    ((and (integerp (car-safe arg)) (integerp (cdr-safe arg)))
     (list arg))
    ((stringp arg) (rx-check-any-string arg))
    ((error
      "rx `any' requires string, character, char pair or char class args"))))





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
	 (goal-deltas (org-balance-groupby goal-deltas-orig 'org-balance-goal-delta-entry-pos)))
    (org-balance-remove-overlays)
    (org-overview)
    (dolist (entry-goal-delta goal-deltas)
      (goto-char (org-balance-goal-delta-entry-pos (cadr entry-goal-delta)))
      (org-balance-put-overlay
       (mapcar
	(lambda (goal-delta)
	  (format "%4d%% \"%20s\" actual: %.2f"
		  (round (org-balance-goal-delta-delta-percent goal-delta))
		  (org-balance-goal-text (org-balance-goal-delta-goal goal-delta))
		  (org-balance-valu-val (org-balance-valu-ratio-num (org-balance-goal-delta-actual goal-delta)))))
	(cdr entry-goal-delta)))
      (org-show-context 'default))))


(defun org-balance-check3 (&optional tstart tend)
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
    (rxx-flet ((org-make-tags-matcher (match) (cons "org-balance" nil))
	       (org-prepare-agenda (title) (org-prepare-agenda-orig "org-balance"))
	       ;; set redo command
	       (org-scan-tags
		(&rest args)
		(mapcar
		 (lambda (goal-delta)
		   (goto-char (org-balance-goal-delta-entry-pos goal-delta))
		   (let ((txt
			  (org-format-agenda-item
			   nil
			   (or
			    (org-balance-goal-delta-error-msg goal-delta)
			    (format "%-40s %+4d%% %20s actual: %.2f"
				    (org-balance-goal-delta-heading goal-delta)
				    (round (org-balance-goal-delta-delta-percent goal-delta))
				    (org-balance-goal-text (org-balance-goal-delta-goal goal-delta))
				    (org-balance-valu-val (org-balance-valu-ratio-num
							   (org-balance-goal-delta-actual goal-delta)))))
			   ))
			 (marker (org-agenda-new-marker)))
		     (org-add-props txt props 'org-marker marker 'org-hd-marker marker 'org-category (org-get-category)
				    'priority (org-get-priority
					       (or
						(org-balance-goal-priority (org-balance-goal-delta-goal goal-delta))
						(org-get-heading))) 'type "org-balance"
						'org-balance-goal-delta goal-delta)))
		 (org-balance-compute-goal-deltas2 :tstart tstart :tend tend)))
	       )
      (let (org-agenda-before-sorting-filter-function
	    org-agenda-sorting-strategy-selected
	    (org-agenda-cmp-user-defined
		(lambda (a b)
		  (let ((va (org-balance-goal-delta-delta-percent (get-text-property 0 'org-balance-goal-delta a)))
			(vb (org-balance-goal-delta-delta-percent (get-text-property 0 'org-balance-goal-delta b))))
		    (if (< va vb) -1 +1)
		    )))
	    (org-agenda-sorting-strategy `((tags ,@org-balance-agenda-sorting-strategy))))
	(org-tags-view)))))

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


(progn (or (equal (rxxlet* ((number-regexp (one-or-more digit) string-to-number) (fraction-regexp (seq (number-regexp numerator) / (number-regexp denominator)) (cons numerator denominator)) (paren-regexp (seq ( (fraction-regexp val) )) val) (range-regexp (seq [ (paren-regexp rmin) ]--[ (paren-regexp rmax) ]) (list rmin rmax))) (rxx-parse range-regexp [(1/2)]--[(3/4)])) (quote ((1 . 2) (3 . 4)))) (signal (quote cl-assertion-failed) (list (quote (equal (rxxlet* ((number-regexp (one-or-more digit) string-to-number) (fraction-regexp (seq (number-regexp numerator) / (number-regexp denominator)) (cons numerator denominator)) (paren-regexp (seq ( (fraction-regexp val) )) val) (range-regexp (seq [ (paren-regexp rmin) ]--[ (paren-regexp rmax) ]) (list rmin rmax))) (rxx-parse range-regexp [(1/2)]--[(3/4)])) (quote ((1 . 2) (3 . 4)))))))) nil)


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
	  (mapc 'delete-overlay (car all))
	  (mapc 'delete-overlay (cdr all)))
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

(message "VVVVVVV%sVVVVVVVVV" (rxx-env-lookup 'num (rxx-info-env (get-rxx-info org-balance-prop-ratio-regexp))))

(string-match org-any-link-re "[[#privet]]")
(car (rxx-parse org-balance-prop-ratio-regexp "done at [[#here]] / spend at [[#there][see]]"))
(equal (cons "hi" nil) (list "hi"))
(defun rxx-add-font-lock-keywords ()
  (when (featurep 'font-lock)
    (put 'defrxxconst 'doc-string-elt 3)
    (put 'defrxx 'doc-string-elt 4)
    (font-lock-add-keywords
     nil
     '(("\\<\\(?1:defrxx\\(?:const\\)?\\)\\>\\(?:[[:blank:]]+\\<\\(?2:[^[:blank:]]+\\)\\>\\)"
	(1  font-lock-keyword-face) (2  font-lock-variable-face))))))

(rxx-parse org-balance-link-regexp "[[http://mit.edu][thislink]]")

(defun try () (interactive)
  (rxx-do-search-fwd org-balance-link-regexp link
			 (message "found link at %s isint %s" link (integerp link))))

(let ((s "********* [#A] GOAL clockedtime: at least 2 hours per day"))
  (string-match org-balance-goal-prefix-regexp s)
  (match-string 0 s))

(defrxx org-balance-goal-regexp
  (sep-by
   blanks
   (opt polarity)
   (valu-range numerator)
   ratio-word
   (valu denominator)
   (opt margin))
  
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


(cl-prettyprint '(lambda (match-str) (let ((repeat-form (quote (seq))) repeat-grp-names parse-result) (while (not parse-result) (when repeat-grp-names (rxx-push-end (quote blanks) repeat-form)) (let ((new-grp-name (make-symbol new-grp))) (rxx-push-end new-grp-name repeat-grp-names) (rxx-push-end repeat-form (list (quote named-grp) new-grp-name (list (quote seq) ((named-grp dgrp digit))))) (save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form (` (lambda (match-str) (mapcar (lambda (repeat-grp-name) (rxx-match-val (list repeat-grp-name (, grp-name)))) (, repeat-grp-names)))) (not (quote partial-ok)) (quote error-ok)) match-str))))) parse-result)) )
(lambda (match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (when repeat-grp-names (rxx-push-end 'blanks repeat-form))
      (let ((new-grp-name (make-symbol new-grp)))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end repeat-form
		      (list 'named-grp
			    new-grp-name
			    (list 'seq ((named-grp dgrp digit)))))
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (\` (lambda (match-str)
									    (mapcar (lambda (repeat-grp-name)
										      (rxx-match-val (list repeat-grp-name
													   (\, grp-name))))
										    (\, repeat-grp-names))))
								      (not 'partial-ok)
								      'error-ok)
						       match-str)))))
    parse-result))

(cl-prettyprint '(lambda (match-str) (let ((repeat-form (quote (seq))) repeat-grp-names parse-result) (while (not parse-result) (when repeat-grp-names (rxx-push-end (quote blanks) repeat-form)) (let ((new-grp-name (make-symbol new-grp))) (rxx-push-end new-grp-name repeat-grp-names) (rxx-push-end repeat-form (list (quote named-grp) new-grp-name (list (quote seq) ((named-grp dgrp digit))))) (save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form (lambda (match-str) (mapcar (lambda (repeat-grp-name) (rxx-match-val (list repeat-grp-name (dgrp [cl-struct-rxx-info digit \(?2:[[:digit:]]\) identity ((nil (nil (nil)) strange-13)) 2 nil])))) nil)) (not (quote partial-ok)) (quote error-ok)) match-str))))) parse-result)) )
(lambda (match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (when repeat-grp-names (rxx-push-end 'blanks repeat-form))
      (let ((new-grp-name (make-symbol new-grp)))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end repeat-form
		      (list 'named-grp
			    new-grp-name
			    (list 'seq ((named-grp dgrp digit)))))
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       (dgrp [cl-struct-rxx-info digit \(\?2: [[:digit:]] \) identity ((nil (nil (nil)) strange-13)) 2 nil]))))
										nil))
								      (not 'partial-ok)
								      'error-ok)
						       match-str)))))
    parse-result))

(cl-prettyprint (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))))
(lambda (match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end repeat-form
		      (list 'named-grp
			    new-grp-name
			    (list 'seq ((named-grp dgrp digit)))))
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       dgrp)))
										nil))
								      (not 'partial-ok)
								      'error-ok)
						       match-str)))))
    parse-result))


(cl-prettyprint (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))))
(lambda (match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end repeat-form
		      (list 'named-grp
			    new-grp-name
			    (list 'seq (named-grp dgrp digit))))
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       'dgrp)))
										nil))
								      (not 'partial-ok)
								      'error-ok)
						       match-str)))))
    parse-result))

(lambda (match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end repeat-form
		      (list 'named-grp
			    new-grp-name
			    (list 'seq (named-grp dgrp digit))))
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       'dgrp)))
										nil)))
						       (not 'partial-ok)
						       'error-ok)
						       match-str))))
    parse-result))
(cl-prettyprint (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))))
(lambda (match-str)

  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end repeat-form
		      (list 'named-grp
			    new-grp-name
			    (list 'seq '(named-grp dgrp digit))))
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       'dgrp)))
										(new-grp)))
								      (not 'partial-ok)
								      'error-ok)
						       match-str)))))
    parse-result))

(cl-prettyprint (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))))
(lambda (match-str)
  (rxx-dbg match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end (list 'named-grp
			    new-grp-name
			    (list 'seq '(named-grp dgrp digit)))
		      repeat-form)
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       'dgrp)))
										nil)))
						       match-str
						       (not 'partial-ok)
						       'error-ok)))))
    parse-result))
 


(cl-prettyprint (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))))
(lambda (match-str)
  (rxx-dbg match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end (list 'named-grp
			    new-grp-name
			    (list 'seq '(named-grp dgrp digit)))
		      repeat-form)
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       'dgrp)))
										nil)))
						       match-str
						       (not 'partial-ok)
						       'error-ok)))))
    parse-result))

(cl-prettyprint (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))))
(lambda (match-str)
  (rxx-dbg match-str)
  (let ((repeat-form '(seq))
	repeat-grp-names
	parse-result)
    (while (not parse-result)
      (let ((new-grp-name (make-symbol "new-grp")))
	(rxx-push-end new-grp-name repeat-grp-names)
	(rxx-push-end (list 'named-grp
			    new-grp-name
			    (list 'seq '(named-grp dgrp digit)))
		      repeat-form)
	(rxx-dbg repeat-form new-grp-name repeat-grp-names)
	(save-match-data (setq parse-result (rxx-parse (rxx-to-string repeat-form
								      (lambda (match-str)
									(mapcar (lambda (repeat-grp-name)
										  (rxx-match-val (list repeat-grp-name
												       'dgrp)))
										(when (boundp 'repeat-grp-names)
										  repeat-grp-names))))
						       match-str
						       (not 'partial-ok)
						       'error-ok)))))
    parse-result))

(funcall (rxx-info-parser (first (rxx-env-lookup 'dgrp-list (rxx-info-env (get-rxx-info (rxx (one-or-more (named-grp dgrp digit)) dgrp)))))) "123")
(let ((s "12"))
  (string-match (rxx (one-or-more (named-grp dgrp digit))) s)
  (match-string 0 s))

(let* ((re2 (rxx (seq "zz" (zero-or-more (seq (named-grp areg rxx-number-regexp) whitespace))) areg-list)))
  (rxx-parse re2 "zz1 2 3 "))

(rxx-parse (rxx (zero-or-more (seq (named-grp areg rxx-number-regexp) whitespace)) areg-list) "1. .2    \t3.4 ")

(cl-prettyprint (rxx-info-parser (get-rxx-info (rxx (zero-or-more (seq (named-grp areg rxx-number-regexp) whitespace)) areg-list) )))

(let ((rxx-prefix "org-balance"))
  (rxx-parse (rxx (1+ number blanks unit blanks)
		  (list number-list unit-list)) "1 dollar 50 cents 30 k "))
(require 'cl)

(cl-prettyexpand (let ((rxx-prefix "org-balance"))
  (rxx-info-parser (first (rxx-env-lookup 'number-list (rxx-info-env (get-rxx-info (rxx (1+ number blanks unit blanks)))))))))
(function
 (lambda (match-str)
   (let ((expr-vals (list match-str)))
     (if nil (apply 'message (append (list "match-str=%s ") expr-vals)))
     (car-safe (with-no-warnings (last expr-vals))))
   (let ((repeat-form '(seq))
	 repeat-grp-names
	 parse-result)
     (while (not parse-result)
       (let ((new-grp-name (make-symbol "new-grp")))
	 (progn
	   (setq repeat-grp-names (append repeat-grp-names
					  (list new-grp-name)))
	   new-grp-name)
	 (progn
	   (setq repeat-form (append repeat-form
				     (list (list 'named-grp
						 new-grp-name
						 '(seq number blanks unit blanks)))))
	   (list 'named-grp new-grp-name '(seq number blanks unit blanks)))
	 (let ((expr-vals (list repeat-form new-grp-name repeat-grp-names)))
	   (if nil
	       (apply 'message
		      (append (list "repeat-form=%s new-grp-name=%s repeat-grp-names=%s ")
			      expr-vals)))
	   (car-safe (with-no-warnings (last expr-vals))))
	 (let ((save-match-data-internal (match-data)))
	   (unwind-protect
	       (setq parse-result (rxx-parse (rxx-to-string repeat-form
							    (function
							     (lambda (match-str)
							       (mapcar (function
									(lambda (repeat-grp-name)
									  (rxx-match-val (list repeat-grp-name
											       'number))))
								       (if (boundp 'repeat-grp-names)
									   repeat-grp-names)))))
					     match-str
					     (not 'partial-ok)
					     'error-ok))
	     (set-match-data save-match-data-internal 'evaporate)))))
     parse-result)))
    (when
	(and nil (symbolp form) (rxx-ends-with (symbol-name form) "?"))
      (setq form (list 'opt (intern (substring (symbol-name form) 0 -1)))))


(defun rxx-add-font-lock-keywords ()
  (when (and nil (featurep 'font-lock))
    (put 'defrxxconst 'doc-string-elt 3)
    (put 'defrxx 'doc-string-elt 4)
    (put 'defrxxrecurse 'doc-string-elt 5)
    (font-lock-add-keywords
     nil
     `((,(rxx (seq bow (group (or "defrxx" "defrxxconst")) blanks (group (1+ (not space))))) .
	((1 font-lock-keyword-face) (2 font-lock-variable-name-face)))
       (,(rxx (seq bow (group "defrxxrecurse") blanks (1+ digit) blanks (group (1+ (not space))))) .
	((1 font-lock-keyword-face) (2 font-lock-variable-name-face)))))))
  
(add-hook 'emacs-lisp-mode-hook 'rxx-add-font-lock-keywords)

(defun rxx-unload-function ()
  "Remove any hooks pointing to rxx functions"
  (ad-disable-regexp "rxx")
  (remove-hook 'emacs-lisp-mode-hook 'rxx-add-font-lock-keywords))

		      ;; while not matched:
		      ;;   -- if repeat-regexp is non-empty and we have a separator, append it to repeat-regexp
		      ;;   -- take a shy-grped version of ad-return-value (or, call it with the option to make it shy-grped)
		      ;;      (or, start with the highest grp number in it, plus one)
		      ;;    optimization: if we have a separator, first try splitting the total match on the separator and
					;      matching each part.   if fail, go to the fallback/default route.
		      ;;   -- append this expr, wrapped in a known-numbered group.
		      ;;   -- try matching
		      ;;   -- if greedy, keep trying and save last match that worked; then re-run the match to fill the match data.
		    ;;   -- so, we either have a non-trivial parser for the repeated subexpr itself or we don't.
		      ;;      if we don't, the most we can give is
		      
		      ;; so, the default parser here might be: (sep-by blanks (1+ valu))
		      ;;    say we have a parser for value
		      ;; (defrxx values (seq first-valu (0+ blanks valu)) (lambda (match-str) (mapcar (lambda (x) (rxx-parse-valu x) (split-string match-str)))))
		      ;; vs
		      ;; (defrxx values (sep-by blanks (1+ valu)))

      ;; now for each name in rxx-env,
      ;; put a name into parent-rxx-env with a parser that would:
      ;;   - for zero-or-one, just leave it.
      ;;   - determine the number of repetitions that matched
      ;;     - take the shy version of ad-return
      ;;     - wrap it into groups, and keep increasing number of copies until matched
      ;;     - if greedy, keep increasing the number of copies until it stops matching
      ;;     - now, take each match string, match the regexp against it, and call
      ;;       
      ;;
      ;; notes:
      ;;   - what exactly happens if zero repetitions matched?
(defrxx nums (sep-by blanks "(" (1+ number) ")"))
(defrxx nums (sep-by blanks "(" (sep-by (seq "," blanks?) (1+ number)) ")") number-list)

	   (body-repeat-regexp
	    ;; remove sep-by if present
	    ;; call the original with the body as (regexp ,body-regexp).
	    ;; probably also later, factor out the common parts so that advice to
	    ;; rx-kleene, rx-repeat etc can call this common code.
	    (let ((form (list (first form) (list 'regexp body-regexp))))
	      ad-do-it
	      ad-return-value
	      ))


;; can define e.g. a grammar for an org entry, or a drawer, etc.

(progn
  (ad-disable-advice 'rx-kleene 'around 'rxx-kleene)
  (ad-activate 'rx-kleene))

(progn
  (ad-enable-advice 'rx-kleene 'around 'rxx-kleene)
  (ad-activate 'rx-kleene))

(rxx-parse (rxx (sep-by blanks (1+ (named-grp d digit))) d-list) "1 2 3")

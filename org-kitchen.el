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


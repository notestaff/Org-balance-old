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


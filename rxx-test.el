
(require 'rxx)
(eval-when-compile (require 'cl))

(assert
 (equal (rxxlet* ((number-regexp (one-or-more digit) string-to-number)
		  (fraction-regexp (seq (named-grp numerator number-regexp) "/" (named-grp denominator number-regexp))
				   (cons numerator denominator))
		  (paren-regexp (seq "(" (named-grp val fraction-regexp) ")") val)
		  (range-regexp (seq "[" (named-grp rmin paren-regexp) "]--[" (named-grp rmax paren-regexp) "]")
				(list rmin rmax)))
		 (rxx-parse range-regexp "[(1/2)]--[(3/4)]")) '((1 . 2) (3 . 4))))


(defconst rxx-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(defconst rxx-number-regexp
  (rxx
   (seq
     (zero-or-more whitespace)
     
     (or
      ;; either an english number name
      (named-grp number-name (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car rxx-number-names)))))
      
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
     (if number-name (cdr (assoc-string number-name rxx-number-names))
       (string-to-number match)))
   "number")
   "Regular expression for a floating-point number")
			
(defconst rxx-number-range-regexp
  (rxx
   (seq
    (named-grp range-start rxx-number-regexp)
    (optional "-" (named-grp range-end rxx-number-regexp)))
   (cons range-start (or range-end range-start))))

(defconst rxx-units
  '((time . ((second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
	     (workweek . 7200)
	     (month . 43200) (year . 525600) (bluemoon 1e12)))
    (money . ((dollar . 1) (cent . .01) (k . 1000)))
    (count . ((item . 1) (time . 1)))))

;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defconst rxx-units
      (mapcar
       (lambda (dimension-info)
	 (cons (car dimension-info)
	       (reduce 'append
		       (mapcar
			(lambda (unit-info)
			  (list unit-info (cons (intern (concat (symbol-name (car unit-info)) "s")) (cdr unit-info))))
			(cdr dimension-info)))))
       rxx-units))

;; var: rxx-unit2dim-alist - assoc list mapping each unit to its dimension (time, money, count, ...)
(defconst rxx-unit2dim-alist
  (reduce 'append
	  (mapcar
	   (lambda (dimension-info)
	     (mapcar (lambda (unit-info) (cons (car unit-info) (car dimension-info))) (cdr dimension-info)))
	   rxx-units)))

(defconst rxx-unit-regexp
  (rxx (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car rxx-unit2dim-alist))))))

(defconst
  rxx-valu-regexp
  (rxx
   ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   (or (seq (optional (named-grp val rxx-number-regexp))
	    (named-grp unit rxx-unit-regexp))
	(seq (named-grp val rxx-number-regexp) (optional (named-grp unit rxx-unit-regexp))))
   (cons (or val 1) (or unit "item"))
   "value with unit")
  "regexp for value with a unit, e.g. '1 day'")

(assert (equal (rxx-parse rxx-valu-regexp "1 day") '(1 . "day")))


(defconst
  rxx-valu-range-regexp
  (rxx
   ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
   ;; or an optional number (assumed to be 1 if not given) followed by a unit.
   ;; But either a number or a unit must be given.
   (or (seq (optional (seq (named-grp range rxx-number-range-regexp) (one-or-more whitespace)))
	     (named-grp unit rxx-unit-regexp))
       (seq (named-grp range rxx-number-range-regexp) (optional (one-or-more whitespace)
								(named-grp unit rxx-unit-regexp))))
   (let ((number-range (or range (cons 1 1)))
	 (unit (or unit "item")))
     (cons (cons (car number-range) unit)
	   (cons (cdr number-range) unit))))
  "value range")

(defconst rxx-ratio-words (list "per" "every" "each" "/" "a" "in a"))

(defconst rxx-polarity-regexp
  (rxx
   (seq (zero-or-more whitespace)
	(seq
	 (or (named-grp atmost (eval-regexp (regexp-opt (list "at most"))))
	     (named-grp atleast (eval-regexp (regexp-opt (list "at least"))))))
	 (zero-or-more whitespace))
     (if atmost 'atmost 'atleast)
   "polarity"))

(defconst rxx-valu-ratio-goal-regexp
  (rxx
   (seq
    (optional (named-grp polarity rxx-polarity-regexp))
    (named-grp num rxx-valu-range-regexp)
    (one-or-more whitespace) (eval-regexp (regexp-opt rxx-ratio-words)) (one-or-more whitespace)
    (named-grp denom rxx-valu-regexp)
    (optional
     ;; specify margin
     (seq
      (one-or-more whitespace)
      "+-"
      (zero-or-more whitespace)
      (seq
       (or
	(seq (named-grp margin-percent rxx-number-regexp)
	     (zero-or-more whitespace)
	     "%")
	(named-grp margin-val rxx-valu-regexp))))))
   (lambda (goal-str)
     (list
      :num-min (car num)
      :num-max (cdr num)
      :denom denom
      :polarity polarity
      :margin (or margin-percent margin-val)
      :text goal-str)))
  "value ratio goal")

(assert (equal (rxx-parse rxx-valu-ratio-goal-regexp "at least once a day +- 5%")
	       '(:num-min (1 . "item") :num-max (1 . "item") :denom (1 . "day") :polarity atleast :margin 5 :text "at least once a day +- 5%")))

(assert (equal (rxx-parse (rxx (seq (or (named-grp num (one-or-more digit)) (seq "hithere" (named-grp num)))  (named-backref num))) "1212") "1212"))


(assert (equal (rxx-parse (rxx (seq (or (named-grp num (one-or-more digit)) (seq "hithere" (named-grp num)))  (named-backref num)) (string-to-number num)) "hithere1212") 12))







	



;;; org-balance.el --- Handle values with units
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
;; Code to handle values with units.


(eval-when-compile (require 'cl))
(require 'elu)
(require 'rxx)

(rxx-start-module org-balance)

(defrxxcustom org-balance-units
  (quote ((time (second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
		(workweek . 7200) (month . 43200) (year . 525600))
	  (money (dollar . 1) ($ . 1) (cent . 0.01) (k . 1000))
	  (count (item . 1) (time . 1))
	  (unpleasantness (frog . 1))))
  "Units and their relative values"
  :group 'org-balance
  :type '(alist :tag "Units used in org-balance"
		:key-type (symbol :tag "Dimension")
		:value-type
		(alist :key-type (symbol :tag "Unit name")
		       :value-type (number :tag "Relative value"))))


(defun org-balance-do-sum (start-value seq)
  "Sum a sequence SEQ of valus, starting with START-VALUE."
  (let ((result
	 (dolist (v seq start-value)
	   (setq start-value (org-balance-add-valu start-value v)))))
    result))
    
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


;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defrxxconst org-balance-units-with-plurals
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
    org-balance-units-with-plurals)))

(put 'org-balance-error 'error-conditions '(error org-balance-errors org-balance-error))
(put 'org-balance-error 'error-message "org-balance error")


(defun org-balance-is-unit (unit)
  (elu-assoc-val unit org-balance-unit2dim-alist 'nil-ok))

(defun org-balance-unit2dim (unit)
  "Given a unit, return the dimension that its measures"
  (elu-assoc-val unit org-balance-unit2dim-alist))

;; var: org-balance-unit2base-alist - assoc list mapping each unit to how many base units are in it
(defconst org-balance-unit2base-alist (apply 'append (mapcar 'cdr org-balance-units-with-plurals)))

(defun org-balance-unit2base (unit)
  "Return the number of base units in the given unit.  For each dimension we have a base unit in terms of which all other
units are measured; for example, for time we use minutes."
  (elu-assoc-val unit org-balance-unit2base-alist))

;; Struct: org-balance-valu - a value together with a given unit of measurement, e.g., 5 hours. 
(defstruct (org-balance-valu
	    (:constructor new-org-balance-valu
			  (val unit-name
			       &aux (unit
				     (if (stringp unit-name) (intern unit-name) unit-name)))))
	    val unit)

(defun org-balance-scale-valu (factor valu)
  "Return the value scaled by the factor"
  (elu-modified-struct org-balance-valu valu
    :val (* factor val)))

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
  "Add two values with units, converting them to a common unit.  Returns
a newly created valu representing the sum of VALU1 and VALU2."
  (let* ((unit1 (org-balance-valu-unit valu1))
	 (unit2 (org-balance-valu-unit valu2))
	 (smaller-unit (if (< (org-balance-unit2base unit1) (org-balance-unit2base unit2))
			   unit1 unit2))
	 (conv1 (org-balance-convert-valu valu1 smaller-unit))
	 (conv2 (org-balance-convert-valu valu2 smaller-unit)))
    (org-balance-make-valu (+ (org-balance-valu-val conv1)
			      (org-balance-valu-val conv2))
			   smaller-unit)))

(defun org-balance-sub-valu (valu1 valu2)
  "Subtract two values with units, converting them to a common unit.  Returns
a newly created valu representing the difference of VALU1 and VALU2."
  (org-balance-add-valu valu1 (org-balance-scale-valu -1 valu2)))

(defun org-balance-add-valu-vec (valu-vec-1 valu-vec-2)
  "Add two vectors of valus"
  (elu-map-vectors 'org-balance-add-valu valu-vec-1 valu-vec-2))

(defun org-balance-scale-valu-vec (factor valu-vec)
  "Scale all valus in VALU-VEC by FACTOR"
  (message "scaling by %s: %s" factor valu-vec)
  (elu-map-vectors (apply-partially 'org-balance-scale-valu factor) valu-vec))

(put 'org-balance-parse-error 'error-conditions '(error org-balance-errors org-balance-parse-error))
(put 'org-balance-parse-error 'error-message "org-balance: Could not parse")

(defrxxconst org-balance-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(defrxx number-name
  "The string name of a number, for the few numbers often written as words.
Parsed as the numeric value of the number."
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-balance-number-names))))
  (lambda (match) (cdr-safe (assoc-string match org-balance-number-names))))

(defrxx number
  "A general number -- floating-point or integer.
Some frequently-used numbers can also be written in English;
see variable `org-balance-number-names'.
Parsed as the numeric value of the number."
  (or
   number-name
   (seq
    (opt (any "+-"))
    (or (seq digits (opt ".") digits?)
	(seq "." digits))
    (opt (any "eE") (opt (any "+-")) digits)))
  (lambda (match)
    (or number-name (string-to-number match))))

(defun org-balance-is-valid-number-p (s)
  "Test if s is a number or a string representing a valid number (ignoring leading or trailing whitespace).
The whole string must match a floating-point number optionally surrounded by whitespace; extraneous characters in string
are not allowed.
"
  (if (numberp s) s
    (save-match-data
      (elu-full-match org-balance-number-regexp s))))

(defun org-balance-string-to-number (s)
  "Convert a string to a number, recognizing some number names for readability.  If s is already a number, just return it.
Unlike the standard `string-to-number', if the string as a whole cannot be interpreted as a valid number possibly surrounded
by whitespace, it throws an error rather than silently returning zero.
"
  (if (numberp s) s
    (rxx-parse org-balance-number-regexp s)))

(defalias 'org-balance-parse-number 'org-balance-string-to-number)

(defrxx number-range
  "A range of two numbers separated by a dash; or a single number,
in which case the range contains just that number.   
Parsed as a cons of range start and end."
  (seq (number range-start) (opt "-" (number range-end)))
  (cons range-start (or range-end range-start)))

(defvar org-balance-parse-valu-hooks nil
  "List of hooks for parsing valu strings (value with units), such as `5 hours'.  Can be used e.g. to parse currency
such as $5 into the canonical form `5 dollars'.  Each hook must take a string as an argument and return either an
`org-balance-valu' struct if it successfully parsed the string, or nil if it didn't.")
;; FIXME: such hooks should also provide the regexp to much this.  so, an aregexp.

(defrxx unit
  "A unit name.   See customization variable "
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car org-balance-unit2dim-alist)))))

(defrxx valu
  ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
  ;; But either a number or a unit must be given.
  (or (sep-by blanks? (named-grp unit "$") number)
      (sep-by blanks number? unit)
      (sep-by blanks number unit?))
  (org-balance-make-valu (or number 1) (or unit "item"))
  "value with unit")

(defun org-balance-parse-valu (valu-str)
  "Given a string representing a value with units, parse it into an org-balance-valu structure."
  (or
   (run-hook-with-args-until-success 'org-balance-parse-valu-hooks valu-str)
   (rxx-parse org-balance-valu-regexp valu-str)))

(defrxx valu-range
  ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
  ;; But either a number or a unit must be given.
  (or (sep-by blanks number-range? unit)
      (sep-by blanks number-range  unit?))
  (let ((number-range (or number-range (cons 1 1)))
	(unit (or unit "item")))
    (cons (org-balance-make-valu (car number-range) unit)
	  (org-balance-make-valu (cdr number-range) unit)))
  "value range")

(defun org-balance-parse-valu-range (valu-str)
  "Given a string representing a value range with units, parse it into an org-balance-valu structure."
  (rxx-parse org-balance-valu-range-regexp valu-str))


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

(rxx-end-module org-balance)

(provide 'org-valu)

;;; org-valu.el ends here


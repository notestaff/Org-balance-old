;;; rxx.el --- Extension to the rx macro for writing readable regexps; adds parsing of named subexpressions.
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
;; Extension to the rx macro for writing readable regexps; adds parsing of named subexpressions.
;; In effect lets you define and parse grammars.

;; explain the issue with modularity and group numbers in subexprs.

;;
;; todo:
;;
;;    - need a simple facility like string-match where you can just give an anonymous pattern and then parse
;;      the results, without constructing an object.  (but ok e.g. to have a special form within which to do it.)
;;
;;      but then also need an analog of save-match-data form.
;; 
;;    - flag duplicately-named groups within regexp
;;
;;    - consider what can be done at compile-time with rx as opposed to rx-to-string.
;;    - should we just add our extra info as a string property of the regexp?  then you can keep
;;      storing it as a string, and using it as regexp.
;;
;;  so, rxx-string-match would call string-match on the string.
;;  then, it would get the data from the string as text prop.

;;
;;  or, you could just have a working org-make-shy routine, and then you can include
;;  subexprs.  you'd just need to call the correct parse routines for them.
;;

(require 'rx)
(eval-when-compile (require 'cl))

(defstruct rxx-info form parser regexp env num)

(defun rxx-process-named-grp (form)
  "Process the (named-grp grp-name grp-def) form."
  (let* ((grp-name (second form))
	 (dummy (when (assq grp-name rxx-env )
		  (error "Duplicate group name %s" grp-name)))
	 (grp-def-xregexp (third form))
	 (grp-num (incf rxx-next-grp-num))
	 (old-rxx-env rxx-env)

	 ;; start a fresh mapping for group names encountered inside
	 ;; this named group
	 (rxx-env '((nil)))
	 (grp-def (get-text-property 0 'rxx grp-def-xregexp))
	 (regexp-here (format "\\(%d?:%s\\)" grp-num
			      (rx-to-string (rxx-info-form grp-def)))))
    (nconc old-rxx-env (list (cons grp-name (make-rxx-info
					     :num grp-num
					     :parser (rxx-info-parser grp-def)
					     :env rxx-env
					     :form (rxx-info-form grp-def)))))
    regexp-here))

(defun rxx-string-match (xregexp s)
    ;
    ; temporarily add our syntax to 
    ;
    (when (string-match (rxx-regexp xregexp) s)
      (let ((name2num (rxx-xregexp-name2num xregexp)))
	(funcall (rxx-xregexp-parser xregexp) (match-string 0))
      )))

(defun rxx-match-val (grp-name &optional xregexp)

  ;; so, if this group stands for just a group, then, just return a string.
  ;; but, if this group stands for 
  
  (let* ((rxx-env (if xregexp (rxx-info-env (get-text-property 0 'rxx xregexp))
		    rxx-env))
	 (grp-info (cdr (assq grp-name rxx-env)))
	 (match-here (match-string (rxx-grp-num grp-info))))
    (when match-here
      (let ((rxx-env (rxx-grp-env grp-info))) 
	(funcall (rxx-grp-parser grp-info) match-here)))))

;; so, what needs to happen is that when we make this recursive call,
;; we need to make 
;;
;; so, the macro would save the regexp as a string,
;; but would also save the original form, so that if we need to construct
;; a regexp with this subexpr, we can.
;; 


;; also make a macro that just constructs an expression for local use,
;; e.g. within a let, rather than necessarily defining a global constant.

(defun rxx (form &optional parser)
  "Construct a regexp from the FORM, using the syntax of `rx-to-string' with some
extensions.  The extensions include (named-grp name forms); in the returned regexp, 
named groups are represented as numbered groups, and a mapping of group names to
group numbers is attached to the returned regexp as a text property.
When interpreting the match result, you can use (rxx-match-string grp-name regexp)
to get the text that matched.  Additionally, if the list of forms in the named group
consists of one xregexp, you can call (rxx-match-val grp-name regexp) to get
the matched subgroup as a parsed object rather than as a string.

The PARSER, if given, is a function that parses the match of this expression
into an object.  The PARSER function is 
passed one argument, the matched string, and may also call rxx-match-val
and rxx-match-string with name of named groups in the form to get their values.
It does not need to pass the regexp to these functions.
"
  ;
  ; cache the result of rxx-xregexp on the whole form.
  ; but also save it for use in subexpressions.
  ;

  (let* (;; var: rxx-env - map from group name used in FORM
	 ;; to group info in the returned regexp.
	 ;; Only includes mappings for named groups at top leve of 
	 ;; FORM, not for any nested once.
	 (rxx-env '((nil)))

	 ;; var: rxx-next-grp-num - the next group number to generate
	 ;;    as we generate group numbers for named groups.
	 (rxx-next-grp-num 20)

	 ;; extend the language understood by rx-to-string with named groups
	 (rx-constituents (cons '(named-grp . (rxx-process-named-grp
					       2  ; at least two args: grp name and def
					       nil))
				rx-constituents))
	 ;; var: regexp - the string regexp for the form.
	 (regexp
	  ;; whenever the rx-to-string call below encounters a (named-grp ) construct
	  ;; in the form, it calls back to rxx-process-named-grp, which will
	  ;; add a mapping from the group's name to rxx-grp structure
	  ;; to rxx-name2grp.
	  (rx-to-string form))
	 (rxx-info (make-rxx-info
		    :form form :parser (if parser parser 'identity)
		    :regexp regexp :env rxx-env
		    )))
    (put-text-property 0 (length regexp) 'rxx rxx-info regexp)
    regexp
  ))

(defun rxx-parse (xregexp s)
  "Match the string against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - require that the full string match
  ;;   - work with re-search-forward and re-search-bwd.
  ;;     
  (when (string-match (rxx-xregexp-regexp xregexp) s)
    (let ((name2num (rxx-xregexp-name2num xregexp)))
      (apply (rxx-xregexp-parser xregexp) (match-string 0)))))

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
     line-start (zero-or-more whitespace)
     ,org-clock-string (one-or-more whitespace)
     
     (named-grp clock-range-start ,org-balance-clock-time-xregexp) "--"
     (named-grp clock-range-end ,org-balance-clock-time-xregexp)
     
     ;(one-or-more whitespace) "=>" (one-or-more whitespace)
     ;(shy-grp ,time-duration-xregexp)
     )
   
   (lambda (clock-range-str)
     (cons (rxx-match-val 'clock-range-start)
	   (rxx-match-val 'clock-range-end))))
  "A clock time range, e.g. [2010-03-17 Wed 16:27]--[2010-03-18 Thu 12:42] => 20:15")
		  
(provide 'rxx)


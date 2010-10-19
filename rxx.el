;;; rxx.el --- Tools for building complex regexps from simpler ones
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://ilya.cc/rxx
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
;; Tools for building complex regular expressions from simpler building
;; blocks.  Implemented as an extension of the `rx' macro that translates
;; regular expressions represented in sexp form to the string representation
;; passed to `string-match'.   Tools in the `rxx' module lets you define
;; a regexp in sexp form, associate with it a parser that constructs a
;; programmatic object from a match to the regexp, and use it as a building
;; block in larger regexps.
;;
;; See the docstring of `rxx' for detailed documentation.
;;

;;
;; Implementation notes:
;;
;; This module defines advice for a number of functions in `rx.el'.
;; This advice only changes the behavior of these functions when
;; called through `rxx-to-string'; if not, these pieces of advice
;; do nothing.   So no behavior is changed for code that just uses
;; the original `rx' macro, even when `rxx' module is loaded.
;; 

;; Extension to the `rx' macro for writing readable regexps; adds parsing of named subexpressions.
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

;; user-callable functions: rxx-named-grp-num, rxx, rxx-match-val, rxx-match-string, rxx-parse

(require 'rx)
(require 'elu)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: general utils
;;
;; General-purpose utility routines used in the rxx module.
;; Also, for portability, reimplementation of some routines from the
;; cl (Common Lisp) module, and some routines available in GNU Emacs but not
;; in XEmacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; struct: rxx-info - information about a regexp or one named group within it.  when the former, it is attached to a regexp
;;    as a text property by `put-rxx-info'; the resulting annotated regexp is referred to as an aregexp in this module.
(defstruct rxx-info
  "Information about a regexp.  Describes both a general template
for creating instances of this regexp, and a particular
instantiation of that template.  When `rxx-to-string' analyzes an
sexp defining a regexp, it creates one `rxx-info' for the overall
regexp and one for each named subgroup within the regexp.

The `rxx-info' for the overall regexp is attached to the regexp
string as a text property, creating an _annotated_ regexp, or
aregexp for short.  This extended information enables us to parse
matches of this regexp into programmatic objects, and to use this
regexp as a building block in larger regexps.

Fields:

   Fields describing the reusable regexp template:

     FORM - the sexp defining this regexp, in the syntax accepted by `rxx-to-string'.
     PARSER - form or function that parses matches of this regexp into programmatic objects.   It can refer to parsed
        values of named direct subgroups simply by subgroup name (they're dynamically scoped in whenever the parser
        is invoked).  If a function, it takes one argument: the string representing the full match to this regexp
        (as returned by (match-string 0)).
     DESCR - a human-readable description of the entity matched by this regexp.   Used in error messages.

   Fields describing the particular instantiation of the template:

     REGEXP - the regexp string returned by `rx-to-string'.   This is a standard regexp as passed to `string-match' etc.
        Note though, that this is only one concrete instantiation of FORM.  Saving FORM with the regexp lets us
        instantiate new instances of the regexp, with new mappings of symbolic group names to numbered group numbers.
     ENV - environment for resolving references to named subgroups of this regexp.  Maps subgroup name to
       `rxx-info' for the subgroup.
     NUM - the numbered group corresponding to to matches of this regexp (as would be passed to `match-string').
   
"
  form regexp parser env num descr)

(defun put-rxx-info (regexp rxx-info)
  "Put rxx-info on a regexp string, replacing any already there.  This creates an aregexp (annotated regexp).
Return the annotated regexp."
  (put-text-property 0 (length regexp) 'rxx rxx-info regexp)
  (when (featurep 'xemacs)
      (put regexp 'rxx rxx-info))
  regexp)

(defun get-rxx-info (aregexp)
  "Extract rxx-info from regexp string, if there, otherwise return nil."
  (when (stringp aregexp)
    (or (get-text-property 0 'rxx aregexp)
	(and (featurep 'xemacs)
	     (get aregexp 'rxx)))))

(defun rxx-new-env (&optional parent-env)
  "Create a fresh environment mapping group names to rxx-infos.  There is an environment for the top-level regexp, and
also a separate one within each named group (for nested named groups).  We represent the environment as an alist.
The alist always has at least one cell; this lets us add entries to an environment that is bound (pointed to) by
several lisp symbols, without having to find and rebind all the symbols."
  (list (cons nil parent-env)))

(defun rxx-parent-env (rxx-env) (cdr (first rxx-env)))

(defun in-rxx ()
  "Test if we're in rxx, i.e. if we got here through a call
to `rxx-to-string'.  Used by pieces of advice to avoid changing
any behavior unless called through this module."
  (declare (special rxx-env))
  (boundp 'rxx-env))


(defun rxx-add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `equal',
or with COMPARE-FN if that's non-nil.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

The return value is the new value of LIST-VAR.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job.

Taken from `add-to-list'.
"
  (if (cond
       ((null compare-fn)
	(member element (symbol-value list-var)))
       ((eq compare-fn 'eq)
	(memq element (symbol-value list-var)))
       ((eq compare-fn 'eql)
	(memql element (symbol-value list-var)))
       (t
	(let ((lst (symbol-value list-var)))
	  (while (and lst
		      (not (funcall compare-fn element (car lst))))
	    (setq lst (cdr lst)))
          lst)))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))

(defun rxx-env-lookup (grp-name rxx-env)
  "Lookup the rxx-info for the named group GRP-NAME in the environment RXX-ENV, or return nil if not found.  GRP-NAME is
either a symbol, or a list of symbols indicating a path through nested named groups.  Since multiple groups may be
bound to the same name in an environment, this returns a list."
  (when (symbolp grp-name) (setq grp-name (list grp-name)))
  (if (eq (first grp-name) (intern ".."))
      (rxx-env-lookup (cdr grp-name) (rxx-parent-env rxx-env))
    (let ((grp-infos (cdr-safe (assq (first grp-name) (cdr rxx-env)))))
      (elu-uniquify
       (apply 'append
	      (mapcar
	       (lambda (grp-info)
		 (if (cdr grp-name)
		     (rxx-env-lookup (cdr grp-name)
				     (rxx-info-env grp-info))
		   (list grp-info)))
	       grp-infos)) 'eq))))

(defun rxx-env-bind (grp-name rxx-info rxx-env)
  "Bind group name GRP-NAME to group annotation RXX-INFO in the
environment RXX-ENV.  If already bound, add to the binding."
  (let ((entry (or (assq grp-name rxx-env)
		   (let ((new-entry (list (cons grp-name nil))))
		     (nconc rxx-env new-entry)
		     (first new-entry)))))
    (setcdr entry (cons rxx-info (cdr entry)))
    rxx-env))

(defmacro do-rxx-env (grp-name rxx-infos rxx-env &rest forms)
  "Execute forms for each (grp-name, rxx-info) binding in this env"
  (declare (indent 3))
  (let ((cur-var (make-symbol "cur")))
    `(let ((,cur-var (cdr ,rxx-env)))
       (while ,cur-var
	 (let ((,grp-name (car (car ,cur-var)))
	       (,rxx-infos (cdr (car ,cur-var))))
	   ,@forms)
	 (setq ,cur-var (cdr ,cur-var))))))

(defun rxx-env-groups (rxx-env)
  (let (all-grps)
    (do-rxx-env grp-name rxx-infos rxx-env
      (push grp-name all-grps))
    all-grps))

(defun rxx-env-empty-p (rxx-env)
  "Return true if there are no bindings"
  (null (cdr rxx-env)))

(defun rxx-named-grp-num (grp-name &optional aregexp)
  "Look up the explicitly numbered group number assigned to the given named group, for passing as the SUBEXP argument
to routines such as `replace-match', `match-substitute-replacement' or `replace-regexp-in-string'.
The annotated regexp must either be passed in as AREGEXP or scoped in as RXX-AREGEXP. "
  (declare (special rxx-aregexp))
  (mapcar 'rxx-info-num
   (rxx-env-lookup
    grp-name
    (rxx-info-env
     (get-rxx-info
      (or aregexp (when (boundp 'rxx-aregexp) rxx-aregexp)
	  (error "The annotated regexp must be either passed in explicitly, or scoped in as `rxx-aregexp'.")))))))


(defun rxx-subregexp-context-p (regexp pos &optional start)
  "Return non-nil if POS is in a normal subregexp context in REGEXP.
A subregexp context is one where a sub-regexp can appear.
A non-subregexp context is for example within brackets, or within a
repetition bounds operator `\\=\\{...\\}', or right after a `\\'.
If START is non-nil, it should be a position in REGEXP, smaller
than POS, and known to be in a subregexp context.

Taken from `subregexp-context-p'."
  ;; Here's one possible implementation, with the great benefit that it
  ;; reuses the regexp-matcher's own parser, so it understands all the
  ;; details of the syntax.  A disadvantage is that it needs to match the
  ;; error string.
  (save-match-data
    (condition-case err
	(progn
	  (string-match (substring regexp (or start 0) pos) "")
	  t)
      (invalid-regexp
       (not (member (cadr err) '("Unmatched [ or [^"
				 "Unmatched \\{"
				 "Trailing backslash")))))))


(defun rxx-make-shy (regexp)
  "Make all groups in re shy; and wrap a shy group around the re.  WARNING: this will kill any backrefs!
Adapted from `regexp-opt-depth'."
  ;; FIXME: check for backrefs, throw error if any present
  (save-match-data
    ;; Hack to signal an error if REGEXP does not have balanced parentheses.
    (string-match regexp "")
    ;; Count the number of open parentheses in REGEXP.
    
      ;; so, you need to replace non-shy groups.
      ;; under emacs, you also need to replace numbered groups.
    (unless (featurep 'xemacs)
      ;; remove explicitly numbered groups
      (while (and (string-match "\\\\(\\?\\([0-9]+\\):" regexp)
		  (rxx-subregexp-context-p regexp (match-beginning 0)))
	(setq regexp (replace-match "" 'fixedcase 'literal regexp 1))))
    ;; remove unnumbered, non-shy groups
    (if (featurep 'xemacs)
	(while (and (string-match "\\\\(\\([^?]\\)" regexp)
		    (rxx-subregexp-context-p regexp (match-beginning 0))
		    )
	  (setq regexp (replace-match "\\(?:\\1" 'fixedcase (not 'literal) regexp)))
    (while (and (string-match "\\\\(\\(\\)[^?]" regexp)
		(rxx-subregexp-context-p regexp (match-beginning 0)))
      (setq regexp (replace-match "?:" 'fixedcase 'literal regexp 1))))
    regexp))
      

(defun rxx-process-named-grp (form)
  "Process the (named-grp GRP-NAME GRP-DEF) form, when called from `rx-to-string'.  GRP-DEF can be an annotated regexp,
a plain regexp, or a form to be recursively interpreted by `rxx'.  If it is an annotated regexp, you can call 
`rxx-match-val' after doing a match to get the parsed object matched by this named group."

  ;
  ; if the form is a symbol, and not one of the reserved ones in rx,
  ; evaluate it as a variable.
  ;
  (declare (special rxx-num-grps rxx-env))
  (rx-check form)
  (or
   (and (boundp 'rxx-replace-named-grps)
	   (cdr-safe (assoc (second form) rxx-replace-named-grps)))
  (let* ((grp-name (second form))
	 (grp-def-raw (third form))
	 (old-grp-defs (rxx-env-lookup grp-name rxx-env))
	 (equiv-old-grp-defs
	  (delq nil
		(mapcar (lambda (old-grp-def)
			  (when (or (null grp-def-raw)  (equal (rxx-info-form old-grp-def) grp-def-raw))
			    old-grp-def))
			old-grp-defs))))
    (if (and nil equiv-old-grp-defs) (rxx-info-regexp (first equiv-old-grp-defs))
      (let* ((grp-num (incf rxx-num-grps))
	     (old-rxx-env rxx-env)
	     (rxx-env (rxx-new-env old-rxx-env))  ;; within each named group, a new environment for group names
	     (grp-def
	      (or (and (symbolp grp-def-raw)
		       (boundp (rxx-symbol grp-def-raw))
		       (get-rxx-info (symbol-value (rxx-symbol grp-def-raw))))
		  (and (eq (car-safe grp-def-raw) 'regexp)
		       (get-rxx-info (second grp-def-raw)))

		  ;;
		  ;; change handlers for eval-regexp and regexp
		  ;; to call regexp-opt-depth and increment
		  ;; our "next grp number" as needed?
		  ;;
		  ;; (though, what if it's )
		  ;;

		  ;;
		  ;; change form processing code to pass along
		  ;; any rxx-infos found on inner code.
		  ;; 
		  ;; 
		  ;;

		  ;;
		  ;; if underlying form has a parser -- i.e. is 
		  ;; a namegrp or a regexp
		  ;;
		  
		  (and (eq (car-safe grp-def-raw) 'eval-regexp)
		       (get-rxx-info (eval (second grp-def-raw))))

		  ;; first recursively analyze the repeated form(s)
		  ;; 
		  
		  (and nil (eq (car-safe grp-def-raw) 'zero-or-more)
		       (make-rxx-info
			:env (rxx-new-env) :form grp-def-raw
			:parser
			`(lambda (match)
			   ;; so, you need to 
			  (let (found result
				(num-repeats 0)
				(one-copy
				 (format "\\(%s\\)"(rxx-make-shy (rxx ,(second grp-def-raw)))))
				(re ""))
			    (save-match-data
			      (while (not found)
				(if (and (string-match re match)
					 (= (match-beginning 0) 0)
					 (= (match-end 0) (length match)))
				    (progn
				      (setq found t)
				      (dotimes (i num-repeats)
					(let ((ms (match-string (1+ i) match)))   ;; pass the string if search was on string
					  (setq result (append result (list ms)))
					)
				      ))
				  (setq re (concat re one-copy))
				  (incf num-repeats)
				  )
			    ))
			    result
			    ))))
		  (make-rxx-info :parser 'identity :env (rxx-new-env)
				 :form (or grp-def-raw (error "Missing named group definition: %s" form)))))
	     ;; generate the string representation of the regexp inside this named group.   note that any named
	     ;; groups _inside_ this regexp will result in recursive calls from `rxx-to-string' back to
	     ;; `rxx-process-named-grp', which will generate new unique numbered group numbers for these nested named
	     ;; groups, and will record the group-name-to-group-number mapping in the rxx-env environment attached
	     ;; to this named group.   Any parser function attached to this named group will be able to refer
	     ;; to the parsed objects matched by these nested named groups using `rxx-match-val', which will look up
	     ;; the mapping of nested group names to group numbers in the rxx-env environment created above for this
	     ;; named group.
	     (regexp-here (format "\\(%s\\)" 
				  (if (and (boundp 'rxx-disable-grps) (member grp-name rxx-disable-grps))
				      (progn
					(message "DISABLING %s" grp-name)
					(rx-to-string (rxx-info-form grp-def))
					".*")
				    ;; here remove -any- shy groups around the whole thing.
				    (rxx-remove-outer-shy-grps (rx-to-string (rxx-info-form grp-def) 'no-group))))))
	(rxx-env-bind grp-name (make-rxx-info
				:num grp-num
				:parser (rxx-info-parser grp-def)
				:env rxx-env
				:regexp regexp-here
				:form (rxx-info-form grp-def)) old-rxx-env)
	regexp-here)))))


(defun rxx-process-named-backref (form)
  "Process the (named-backref GRP-NAME) form, when called from `rx-to-string'."
  (declare (special rxx-env))
  (rx-check form)
  (let* ((grp-name (second form))
	 (prev-grp-defs (rxx-env-lookup grp-name rxx-env)))
    (unless prev-grp-defs (error "Group in backref not yet seen: %s" grp-name))
    (unless (= (length prev-grp-defs) 1) (error "Ambiguous backref to group %s" grp-name))
    (rx-backref `(backref ,(rxx-info-num (first prev-grp-defs))))))




(defun rxx-call-parser (rxx-info match-str)
  (let ((rxx-env (rxx-info-env rxx-info)))
    (let* ((symbols (delq nil (mapcar 'car (rxx-info-env rxx-info))))
	   (symbol-vals (mapcar
			 (lambda (symbol)
			   (rxx-match-val symbol))
			 symbols))
	   (parser (rxx-info-parser rxx-info)))
      (elu-progv symbols symbol-vals
	(save-match-data
	  (if (functionp parser)
	      (funcall parser match-str)
	    (eval parser)))))))

(defun rxx-match-aux (code)
  "Common code of `rxx-match-val', `rxx-match-string', `rxx-match-beginning' and `rxx-match-end'.  Looks up the rxx-info
for the relevant named group, so that we can get the corresponding group explicitly numbered group number and pass it
to `match-string', `match-beginning' or `match-end'."
  (declare (special grp-name object aregexp rxx-object rxx-aregexp rxx-env))
  (save-match-data
    (let* ((rxx-env
	    (if (boundp 'rxx-env) rxx-env
	      (let ((aregexp (or aregexp (when (boundp 'rxx-aregexp) rxx-aregexp))))
		(rxx-info-env
		 (or
		  (get-rxx-info aregexp)
		  (error "Annotated regexp created by `rxx' must either be passed in, or scoped in via RXX-AREGEXP."))))))
	   (grp-infos (or (rxx-env-lookup grp-name rxx-env) (error "Named group %s not found" grp-name)))
	   (matches-here
	    (delq nil
		  (mapcar
		   (lambda (grp-info)
		     (let ((match (match-string (rxx-info-num grp-info)
						(or object
						    (when (boundp 'rxx-object) rxx-object)))))
		       (when match (cons match grp-info))))
		   grp-infos))))
      (when matches-here
	(unless (= (length matches-here) 1) (error "More than one match to group %s: %s" grp-name matches-here))
	(let* ((match-info-here (first matches-here))
	       (match-here (car match-info-here))
	       (grp-info (cdr match-info-here))
	       (grp-num (rxx-info-num grp-info)))
	  (funcall code))))))

(defun rxx-match-val (grp-name &optional object aregexp)
  "Return the parsed object matched by named group GRP-NAME.  OBJECT, if given, is the string or buffer we last searched;
may be scoped in via RXX-OBJECT.  The annotated regexp must either be passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-object rxx-aregexp grp-info match-here))
  (rxx-match-aux
   (lambda ()
       (let ((rxx-env (rxx-info-env grp-info))) 
	 (rxx-call-parser grp-info match-here)))))

(defun rxx-match-string (grp-name &optional object aregexp)
  "Return the substring matched by named group GRP-NAME.  OBJECT, if given, is the string or buffer we last searched;
may be scoped in via RXX-OBJECT.  The annotated regexp must either be passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-object rxx-aregexp match-here))
  (rxx-match-aux (lambda () match-here)))

(defun rxx-match-beginning (grp-name &optional object aregexp)
  "Return the beginning position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special grp-num))
  (rxx-match-aux (lambda () (match-beginning grp-num))))

(defun rxx-match-end (grp-name &optional object aregexp)
  "Return the end position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special grp-num))
  (rxx-match-aux (lambda () (match-end grp-num))))

(defun rxx-process-eval-regexp (form &optional rx-parent)
  "Parse and produce code from FORM, which is `(eval-regexp FORM)'."
  (declare (special rxx-num-grps))
  (rx-check form)
  (let ((regexp (eval (second form))))
    (incf rxx-num-grps (regexp-opt-depth regexp))
    (concat "\\(?:" (rx-group-if regexp rx-parent) "\\)")))

(defun rxx-replace-posix (s)
  "Replace posix classes in regular expression, for xemacs compatibility.  Adapted from `org-re'."
  (when (featurep 'xemacs)
    (save-match-data
      (dolist (repl '((alnum "a-zA-Z0-9") (word "a-zA-Z0-9") (alpha "a-zA-Z") (digit "0-9")
		      (lower "a-z") (upper "A-Z")
		      (blank " \t") (ascii "\000-\127") (punct "\001-@[-`{-~")))
	(while (string-match (concat "\\[:" (symbol-name (first repl)) ":\\]") s)
	  (setq s (replace-match (second repl) 'fixedcase 'literal s))))))
  s)


(defun rxx-symbol (symbol &optional no-regexp)
  "If rxx prefix is defined (see `rxx-set-prefix'), and SYMBOL does not
end with -regexp or -re, return the full name of the symbol (prefix-SYMBOL-regexp).
If NO-REGEXP is non-nil, do not append the -regexp part and just prepend the prefix."
  (if (and (boundp 'rxx-prefix) rxx-prefix
	   (not (elu-ends-with (symbol-name symbol) "-regexp"))
	   (not (elu-ends-with (symbol-name symbol) "-re")))
      (intern (concat rxx-prefix "-" (symbol-name symbol) (if no-regexp "" "-regexp")))
    symbol))

(defadvice rx-form (around rxx-form first (form &optional rx-parent) activate compile)
  "Handle named subexpressions.  Any symbol whose variable value
is an aregexp (e.g.  one defined by `defrxx') can be used as a
form.  More specifically: if R is a symbol whose variable value
is an aregexp, the form (R name) creates a named group (see
`rxx-process-named-grp') with the name R matching that aregexp.
The form R by itself is equivalent to (R R) i.e. a named group
named R with the definition of R.  The latter only works if a
group named R is not yet defined in the current environment.  R
can be abbreviated (see `rxx-symbol').

Also, R? is translated to (opt R) for a slight reduction in verbosity.
"
  (declare (special rxx-env))
  (if (not (boundp 'rxx-env))
      ad-do-it

    (when (and (symbolp form) (elu-ends-with (symbol-name form) "?"))
      (setq form (list 'opt (intern (substring (symbol-name form) 0 -1)))))
    (cond ((and (consp form) (symbolp (first form)) (boundp (rxx-symbol (first form)))
		(get-rxx-info (symbol-value (rxx-symbol (first form)))))
	   (setq ad-return-value (rxx-process-named-grp (list 'named-grp (second form) (rxx-symbol (first form))))))
	  ((and (symbolp form) (boundp (rxx-symbol form)) (boundp 'rxx-env) (not (rxx-env-lookup (rxx-symbol form) rxx-env))
		(get-rxx-info (symbol-value (rxx-symbol form))))
	   (setq ad-return-value
		 (rxx-process-named-grp (list 'named-grp form form))))
	  (t ad-do-it))))

(defadvice rx-submatch (before rxx-submatch first (form) activate compile)
  "Keep a count of the number of non-shy subgroups, so that when a named
group is created (see `rxx-process-named-grp'), we will know the regexp group
number to which it corresponds."
  (when (boundp 'rxx-num-grps) (incf rxx-num-grps)))

(defadvice rx-regexp (after rxx-regexp first (form) activate compile)
  "Update our count of non-shy subgroups to include any subgroups of the
regexp inserted here."
  (when (boundp 'rxx-num-grps) (incf rxx-num-grps (regexp-opt-depth (second form)))))

(defun rxx-check-regexp-valid (regexp)
  "Throw an error unless REGEXP is a valid regexp."
  (string-match regexp ""))

(defun rxx-to-string (form &optional parser descr)
  "Construct a regexp from its readable representation as a lisp FORM, using the syntax of `rx-to-string' with some
extensions.  The extensions, taken together, allow specifying simple grammars
in a modular fashion using regular expressions.

For detailed description, see `rxx'.
"
  (rxx-remove-unneeded-shy-grps
   (let* ((rxx-env (rxx-new-env))
	  (rxx-num-grps 0)
	  rxx-or-branch
	  rxx-or-child
	  (rxx-or-num 0)
	  ;; extend the syntax understood by `rx-to-string' with named groups and backrefs
	  (rx-constituents (append '((named-grp . (rxx-process-named-grp 1 nil))
				     (eval-regexp . (rxx-process-eval-regexp 1 1))
				     (shy-grp . seq)
				     (& . seq)
				     (blanks . "\\(?:[[:blank:]]+\\)")
				     (digits . "\\(?:[[:digit:]]+\\)")
				     (space . "\\s-")
				     (sep-by . (rxx-process-sep-by 1 nil))
				     (recurse . (rxx-process-recurse 1 nil))
				     (named-group . named-grp) (shy-group . shy-grp)
				     (named-backref . (rxx-process-named-backref 1 1)))
				   rx-constituents))
	  
	  ;; also allow named-group or ngrp or other names
	  ;; var: regexp - the string regexp for the form.
	  (regexp
	   ;; whenever the rx-to-string call below encounters a (named-grp ) construct
	   ;; in the form, it calls back to rxx-process-named-grp, which will
	   ;; add a mapping from the group's name to rxx-grp structure
	   ;; to rxx-name2grp.
	   (rx-to-string form 'no-group))
	  (rxx-info (make-rxx-info
		     :form form :parser (or parser
					    'identity)
		     :env rxx-env :descr descr :regexp regexp
		     )))
     (put-rxx-info regexp rxx-info)
     (setq regexp (rxx-replace-posix regexp))
     (rxx-check-regexp-valid regexp)
     (assert (= rxx-num-grps (regexp-opt-depth regexp)))
     regexp)))

(defun rxx-process-sep-by (form)
  "Process the sep-by form, which looks like (sep-by separator ....)"
  (let ((separator (second form))
	(seq-elems (cddr form)))
    (rx-form
     (let ((form-with-separators '(seq)) seen-non-optional)
       (dolist (seq-elem seq-elems form-with-separators)

	 (when (and (symbolp seq-elem) (elu-ends-with (symbol-name seq-elem) "?"))
	   (setq seq-elem (list 'opt (intern (substring (symbol-name seq-elem) 0 -1)))))

	 (when (and (consp seq-elem) (memq (first seq-elem) '(0+ zero-or-more 1+ one-or-more * *? + +?)))
	   (setq seq-elem (append (list (first seq-elem) :sep-by separator) (cdr seq-elem)))) 
	 
	 (let ((is-optional (and (consp seq-elem) (memq (first seq-elem) '(opt optional zero-or-one ? ??)))))
	   (setq form-with-separators
		 (append form-with-separators
			 (if is-optional
			     (list
			      (if (not seen-non-optional)
				  (append seq-elem (when (> (length form) 3) (list separator)))
				  (append (list (first seq-elem) separator) (cdr seq-elem))))
			   (if (not seen-non-optional) (list seq-elem)
			     (list separator seq-elem)))))
	   (unless is-optional (setq seen-non-optional t))
	   form-with-separators))))))

(put 'sep-by lisp-indent-function 1)

(defconst rxx-never-match
  (if (featurep 'xemacs) "[^\000-\127]"
    (rx (not (any ascii nonascii))))
  "A regexp that never matches anything.
Used for bottoming out bounded recursion (see `rxx-process-recurse').")

(defun rxx-process-recurse (form)
  "Process recurse"
  (declare (special rxx-recurs-depth))
  (if (or (not (boundp 'rxx-recurs-depth))
	  (< rxx-recurs-depth 1))
      rxx-never-match
    (let ((rxx-recurs-depth (1- rxx-recurs-depth)))
      (rx-group-if (rxx-remove-unneeded-shy-grps (rx-to-string (second form) 'no-group)) '*)))) 

(defmacro rxx (form &optional parser descr)
  "Construct a regexp from its readable representation as a lisp FORM, using the syntax of `rx-to-string' with some
extensions.  The extensions, taken together, allow specifying simple grammars
in a modular fashion using regular expressions.

The main extension is named groups, specified in FORM as (named-grp NAME FORMS...).
A named group is analogous to an explicitly numbered group (and is in fact converted
to one), except that it is referenced by name rather than by number.

As a simple example, you might write things like:


; in the returned regexp, 
named groups are represented as explicitly numbered groups, and a mapping of group names to
group numbers is attached to the returned regexp (as a text property).
When interpreting the match result, you can use (rxx-match-string GRP-NAME REGEXP)
to get the text that matched.  Additionally, if the list of forms in the named group
consists of one aregexp, you can call (rxx-match-val grp-name regexp) to get
the matched subgroup as a parsed object rather than as a string.


to explain:
   that because we save the form and the parser, we can use this as a sub-regexp.
the saved form lets us generate new explicit group numbers, and
the fact that the parser gets its subgroups by name within an environment lets us
make the parser work with new set of group numbers.


The PARSER, if given, is a function that parses the match of this expression
into an object.  The PARSER function is 
passed one argument, the matched string, and may also call rxx-match-val
and rxx-match-string with name of named groups in the form to get their values.
It does not need to pass the regexp to these functions.

DESCR, if given, is used in error messages by `rxx-parse'.
"

  (let ((r (rxx-to-string form parser descr)))
    r
  ))


(defmacro rxxlet* (bindings &rest forms)
  (list 'let* (mapcar (lambda (binding) (list (first binding)
					      (list 'rxx (second binding) (third binding) (symbol-name (first binding)))))
		      bindings)
	(or (car-safe forms) (and bindings (car-safe (car-safe (last bindings)))))))



(defun* rxx-parse (aregexp s &optional partial-match-ok error-ok)
  "Match the string against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
  
  (unless s (error "rxx-parse: nil string"))
  (save-match-data
      (let* ((rxx-info (or (get-rxx-info aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp)))
	     (error-msg (format "Error parsing \`%s\' as %s" s
				(or (rxx-info-descr rxx-info) (rxx-info-form rxx-info)))))
	
	;; so, what you need here is just:
	;;   -- if full match, then parse and be done.
	;;   -- if no match, go to the next.
	;; if partial match ok, longer partial match is generally better.
	;; so, possibly, match each string.  (or only when rxx-longest-match is true?)
	
      (if (not (string-match aregexp s))
	  (unless error-ok (error "%s" error-msg))
	(let (no-parse)
	  (unless partial-match-ok
	    (unless (= (match-beginning 0) 0)
	      (if error-ok (setq no-parse t) (error "%s: match starts at %d" error-msg (match-beginning 0))))
	    (unless (= (match-end 0) (length s))
	      (if error-ok (setq no-parse t) (error "%s: match ends at %d" error-msg (match-end 0)))))
	  (unless no-parse
	    (let* ((rxx-env (rxx-info-env rxx-info))
		   (rxx-object s))
	      (rxx-call-parser rxx-info (match-string 0 s)))))))))

(defun* rxx-search-fwd (aregexp &optional bound noerror (partial-match-ok t))
  "Match the current buffer against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
      (let* ((old-point (point))
	     (rxx-info (or (get-rxx-info aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp)))
	     (error-msg (format "Error parsing \`%s\' as %s"
				(if (and bound (>= bound old-point) (< (- bound old-point) 100))
				    (buffer-substring old-point bound)
				  "buffer text") (rxx-info-form rxx-info))))
	(if (not (re-search-forward aregexp bound 'noerror))
	    (unless noerror (error "%s" error-msg))
	  (unless (or noerror partial-match-ok)
	    (unless (= (match-beginning 0) old-point) (error "%s: match starts at %d" error-msg (match-beginning 0)))
	    (unless (= (match-end 0) bound) (error "%s: match ends at %d" error-msg (match-end 0))))
	  (let* ((rxx-env (rxx-info-env rxx-info))
		 rxx-object)
	    (rxx-call-parser rxx-info (match-string 0))))))



(defun* rxx-search-bwd (aregexp &optional bound noerror (partial-match-ok t))
  "Match the current buffer against the given aregexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;   - and with posix searches
  ;;
      (let* ((old-point (point))
	    (rxx-info (or (get-rxx-info aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp)))
	    (error-msg (format "Error parsing \`%s\' as %s"
			       (if (and bound (>= bound old-point) (< (- bound old-point) 100))
				   (buffer-substring old-point bound)
				 "buffer text") aregexp)))
	(if (not (re-search-backward aregexp bound 'noerror))
	    (unless noerror (error "%s" error-msg))
	  (unless (or noerror partial-match-ok)
	    (unless (= (match-beginning 0) old-point) (error "%s: match starts at %d" error-msg (match-beginning 0)))
	    (unless (= (match-end 0) bound) (error "%s: match ends at %d" error-msg (match-end 0))))
	  (let* ((rxx-env (rxx-info-env rxx-info))
		 rxx-object)
	    (rxx-call-parser rxx-info (match-string 0))))))

(defmacro rxx-do-search-fwd (aregexp var &rest forms)
  "Search forward while matches"
  (declare (indent 2))
  (unless var (setq var (make-symbol "dummy-var")))
  `(let (,var) (while (setq ,var (rxx-search-fwd ,(rxx-symbol aregexp) (not 'boundary) 'no-error))
		 ,@forms)))

(defun rxx-parse-fwd (aregexp &optional bound partial-match-ok)
  (save-match-data
    (save-excursion
      (rxx-search-fwd aregexp bound (not 'noerror) partial-match-ok))))


(defun rxx-parse-bwd (aregexp &optional bound partial-match-ok)
  "Match the current buffer against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
  (save-match-data
    (save-excursion
      (let ((old-point (point))
	    (rxx-info (or (get-rxx-info aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp))))
	(if (and (re-search-backward aregexp bound 'noerror)
		 (or partial-match-ok
		     (and (= (match-beginning 0) bound)
			  (= (match-end 0) old-point))))
	    (let* ((rxx-env (rxx-info-env rxx-info))
		   rxx-object)
	      (rxx-call-parser rxx-info (match-string 0)))
	  (error "Error parsing \`%s\' as %s" (if (and bound (>= bound old-point) (< (- bound old-point) 100))
						  (buffer-substring old-point bound)
						"buffer text")
		 (or (rxx-info-descr rxx-info) (rxx-info-form rxx-info))))))))


(defun rxx-parse-recurs (aregexp s max-recurs-depth &optional partial-match-ok)
  (let* ((rxx-recurs-depth max-recurs-depth)
	 (unwound-aregexp (rxx-to-string `(named-grp top-grp
						     ,aregexp))))
    (rxx-parse (rxx-to-string unwound-aregexp) s partial-match-ok)))

(defmacro rxx-push-end (elt lst)
  "Push elt onto end of list, and return the elt"
  `(progn
     (setq ,lst (append ,lst (list ,elt)))
     ,elt))

(defvar rx-greedy-flag)

(defadvice rx-kleene (around rxx-kleene first (form) activate)
  "When processing repeat constructs such as `zero-or-more' and `one-or-more',
turn each named group GRP inside the repeat into a corresponding named group GRP-LIST
whose parser returns the list of parsed individual matches.   So, a construct
such as (1+ num) containing the named group `num' that parses into a number
will have a named group `num-list' that parses into the list of numbered matched
by the repeat.   Note that this is in contrast to `match-string' which returns
only the last copy of the repeated expression.

Also, extend the syntax to allow specifying a separator regexp, e.g. 
\(zero-or-more :sep-by REGEXP-FORM ...).  Then the copies of the repeat
are required to be separated by strings matching REGEXP-FORM.
The parsers that we construct for each named group return parsed matches
of the repeat contents only, not of the separators.
So, for the construct (1+ :sep-by blanks num), the parser for `num-list' would
return the list of parsed numbers, omitting the blanks.   See also
`rxx-process-sep-by'.
"
  (declare (special rxx-env rxx-num-grps))
  (if (or (not (boundp 'rxx-env)) (not (boundp 'rxx-num-grps))
	  (memq (first form) '(optional opt zero-or-one ? ??)))
      ad-do-it
    (let* ((wrap-grp-num (when (boundp 'rxx-num-grps) (incf rxx-num-grps)))
	   (rxx-num-grps (when (boundp 'rxx-num-grps) rxx-num-grps))
	   (parent-rxx-env (when (boundp 'rxx-env) rxx-env))
	   (rxx-env (rxx-new-env parent-rxx-env))
	   (sep-by-form (when (eq (car-safe (cdr-safe form)) :sep-by) (third form)))
	   (have-sep-by (not (null sep-by-form)))
	   (body (cons 'seq (nthcdr (if sep-by-form 3 1) form)))
	   (form-sans-sep-by (list (car form) body))
	   (body-regexp (rx-to-string body))
	   (body-repeat-regexp
	    ;; remove sep-by if present
	    ;; call the original with the body as (regexp ,body-regexp).
	    ;; probably also later, factor out the common parts so that advice to
	    ;; rx-kleene, rx-repeat etc can call this common code.
	    (let ((form (delq
			 nil
			 (list
			  (if sep-by-form
			      (ecase (first form) ((1+ one-or-more 0+ zero-or-more) '0+) ((+ *) '*) ((+? *?) '*?))
			    (first form))
			  sep-by-form
			  (list 'regexp body-regexp)))))
	      ad-do-it
	      ad-return-value))
	   (greedy-p (or (memq (first form) '(* + ?\s))
			 (and rx-greedy-flag (memq (first form) '(zero-or-more 0+ one-or-more 1+ >= repeat **))))))

      (when sep-by-form
	(setq
	 ad-return-value
	 (rx-to-string
	   (ecase (first form)
	     ((1+ one-or-more + +?)
	      `(seq (regexp ,body-regexp) (regexp ,body-repeat-regexp)))
	     ((0+ zero-or-more * *?)
	      `(,(first form) (regexp ,body-regexp) (regexp ,body-repeat-regexp)))))))

      (setq ad-return-value (format "\\(%s\\)" (rxx-make-shy ad-return-value)))
      (progn
	(do-rxx-env grp-name rxx-infos rxx-env
	  (elu-dbg grp-name rxx-infos)
	  (let ((new-parser
		 `(lambda (match-str)
		    (let ((rxx-prefix ,(when (boundp 'rxx-prefix) rxx-prefix)))
		      (elu-dbg match-str)
		      (let ((repeat-form '(seq)) repeat-grp-names parse-result )
			(while (not parse-result)
			  (when (and repeat-grp-names ,have-sep-by)
			    (rxx-push-end (quote ,sep-by-form) repeat-form))
			  (let ((new-grp-name (make-symbol "new-grp")))
			    (rxx-push-end new-grp-name repeat-grp-names)
			    (rxx-push-end (list 'named-grp new-grp-name
						(quote (seq ,@(cdr form-sans-sep-by)))) repeat-form)
			    (elu-dbg repeat-form new-grp-name repeat-grp-names)
			    (save-match-data
			      (setq parse-result
				    (rxx-parse
				     (rxx-to-string repeat-form
						    ,`(lambda (match-str)
							(mapcar (lambda (repeat-grp-name)
								  (rxx-match-val (list repeat-grp-name (quote ,grp-name))))
								(when (boundp 'repeat-grp-names) repeat-grp-names))))
				     match-str
				     (not 'partial-ok) 'error-ok
				     )))))
		      parse-result)))))
	    (rxx-env-bind (intern (concat (symbol-name grp-name) "-list"))
			  (make-rxx-info :parser new-parser :env (rxx-new-env) :num wrap-grp-num)
			  parent-rxx-env)))))))


(defadvice rx-or (around rxx-or first (form) activate compile)
  "For bounded recursion, remove any OR clauses consisting of
`recurs' forms for which recursion has bottomed out. (See
`rxx-parse-recurs' and `defrxxrecurse')."
  (unless (or (not (boundp 'rxx-env))
	      (and (boundp 'rxx-recurs-depth) (> rxx-recurs-depth 0))
	      (<= (length form) 2))
    (setq form (rxx-remove-if
		(lambda (elem)
		  (or (eq (car-safe elem) 'recurse)
		      (and (eq (car-safe elem) 'named-grp)
			   (eq (car-safe (car-safe
					  (cdr-safe
					   (cdr-safe elem)))) 'recurse))))
			  form)))
  ad-do-it
  (when (boundp 'rxx-env)
    (setq ad-return-value (rx-group-if ad-return-value '*))))

(defun rxx-remove-unneeded-shy-grps (re)
  "Remove shy groups that do nothing"
  (while (and t (>= (length re) 10) (string= (substring re 0 8) "\\(?:\\(?:")
     	      (string= (substring re -4) "\\)\\)"))
    (setq re (substring re 4 -2)))
  re)
  
(defun rxx-remove-outer-shy-grps (re)
  "Remove any outer shy groups."
  (while (and nil (>= (length re) 6) (string= (substring re 0 4) "\\(?:")
	      (string= (substring re -2) "\\)"))
    (setq re (substring re 4 -2)))
  re)

(defmacro rxx-set-prefix (prefix)
  "Specify a prefix to be automatically prepended to aregexps defined by `defrxx'.  Typically this would be the
name of the module in which the aregexps are being defined.   So, if you do (rxx-set-prefix my-module) then
(defrxx val ...) defines aregexp named my-module-val-regexp; it can be referred to as simply `val' when used
in larger regexps."
  `(defrxxconst rxx-prefix (when (quote ,prefix) (symbol-name (quote ,prefix)))))

(defmacro defrxxconst (symbol initvalue &optional docstring)
  (if (featurep 'xemacs)
      `(defconst ,symbol ,initvalue ,docstring)
    `(eval-and-compile
       (defconst ,symbol ,initvalue ,docstring))))

(defmacro defrxxcustom (symbol initvalue docstring &rest args)
  `(eval-and-compile
     (defcustom ,symbol ,initvalue ,docstring ,@args)))

(defmacro defrxx (var &rest args)
  "Define a regexp and a parser for it."
  (let (form parser descr)
    (cond
     ((stringp (nth 0 args))
      (setq descr (nth 0 args)
	    form (nth 1 args)
	    parser (nth 2 args)))
     ((stringp (nth 1 args))
      (setq form (nth 0 args)
	    descr (nth 1 args)
	    parser (nth 2 args)))
     (t (setq form (nth 0 args)
	      parser (nth 1 args)
	      descr (nth 2 args))))
    (if (featurep 'xemacs)
	`(defconst ,(rxx-symbol var) (rxx-to-string (quote ,form) (quote ,parser) ,descr) ,descr)
    `(defrxxconst ,(rxx-symbol var) (rxx ,form ,parser ,descr) ,descr))))

(defmacro defrxxrecurse (depth var regexp &optional parser descr)
  (if (featurep 'xemacs)
    `(defconst ,(rxx-symbol var) (let ((rxx-recurs-depth ,depth)) (rxx-to-string (quote ,regexp)
										 (quote ,parser) ,descr))
       ,descr)
  `(defrxxconst ,var ,(let ((rxx-recurs-depth depth))
			(rxx-to-string regexp parser descr)) ,descr)))

(defmacro defrxxstruct (var regexp &optional parser descr)
  `(progn
     (defrxxconst ,var ,regexp ,parser ,descr)
     ;; need to get the list of top-level groups here.
     (defstruct ,(rxx-symbol var 'no-regexp) ,@(rxx-env-groups (rxx-info-env (get-rxx-info (eval (rxx-symbol var))))))))

(defun rxx-add-font-lock-keywords ()
  (when (featurep 'font-lock)
    (put 'defrxxconst 'doc-string-elt 3)
    (put 'defrxx 'doc-string-elt 4)
    (put 'defrxxrecurse 'doc-string-elt 5)
	 (when (fboundp 'font-lock-add-keywords)
		(font-lock-add-keywords
		 nil
		 `((,(rx (seq bow (group (or "defrxx" "defrxxconst" "defrxxcustom" "defrxxstruct"))
						  (1+ blank) (group (1+ (not space))))) .
						  ((1 font-lock-keyword-face) (2 font-lock-variable-name-face)))
			(,(rx (seq bow (group "defrxxrecurse") (1+ blank) (1+ digit) (1+ blank) (group (1+ (not space))))) .
			 ((1 font-lock-keyword-face) (2 font-lock-variable-name-face))))))))
  
(add-hook 'emacs-lisp-mode-hook 'rxx-add-font-lock-keywords)

;; additional forms:
;;   seq-any-order
;;   sep-by-any-order
;;
;; debug/better-error-messaging facilities
;; enumerate matches
;; default parsers: if only one named grp, the value of that; if two or more, make a struct with these fields as names.

;; better support for defining these things at runtime.


(provide 'rxx)



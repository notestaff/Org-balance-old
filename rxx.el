;;; rxx.el --- Easily build simple parsers using regexps
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: regexp, parser, DSL
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
;; Externally callable functions and macros:
;;
;;  Defining regexps:
;;
;;  `defrxx', `defrxxrecurse'
;;
;;  Parsing:
;;
;;  `rxx-parse'
;;
;;  General utils:
;;
;;  `rxx-make-shy'
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


;;; History:
;; 

(require 'rx)
(require 'elu)
;;; Code:
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Utils for manipulating regexp strings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rxx-check-regexp-valid (regexp)
  "Throw an error unless REGEXP is a valid regexp; if it is valid, return it."
  (string-match regexp "")
  regexp)

(defun rxx-replace-posix (re)
  "Return a version of regexp RE with POSIX classes replaced, for xemacs
compatibility.  Adapted from `org-re'."
  (when (featurep 'xemacs)
    (save-match-data
      (dolist (repl '((alnum "a-zA-Z0-9") (word "a-zA-Z0-9") (alpha "a-zA-Z") (digit "0-9")
		      (lower "a-z") (upper "A-Z")
		      (blank " \t") (ascii "\000-\127") (punct "\001-@[-`{-~")))
	(while (string-match (concat "\\[:" (symbol-name (first repl)) ":\\]") re)
	  (setq re (replace-match (second repl) 'fixedcase 'literal re))))))
  (rxx-check-regexp-valid re))

(defun rxx-remove-unneeded-shy-grps (re)
  "Return a regexp equivalent to RE but removing shy groups that have no effect."
  (while (and t (>= (length re) 10) (string= (substring re 0 8) "\\(?:\\(?:")
     	      (string= (substring re -4) "\\)\\)"))
    (setq re (substring re 4 -2)))
  (rxx-check-regexp-valid re))
  
(defun rxx-remove-outer-shy-grps (re)
  "Return regexp equivalent to RE but with any outer shy groups removed."
  (while (and nil (>= (length re) 6) (string= (substring re 0 4) "\\(?:")
	      (string= (substring re -2) "\\)"))
    (setq re (substring re 4 -2)))
  (rxx-check-regexp-valid re))

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
  "Return a new regexp that makes all groups in REGEXP shy; and
wrap a shy group around the returned REGEXP.  WARNING: this will
kill any backrefs!  Adapted from `regexp-opt-depth'."
  ;; FIXME: check for backrefs, throw error if any present
  (save-match-data
    ;; Hack to signal an error if REGEXP does not have balanced parentheses.
    (string-match regexp "")
    ;; Count the number of open parentheses in REGEXP.
    
      ;; so, you need to replace non-shy groups.
      ;; under emacs, you also need to replace numbered groups.
    (unless (featurep 'xemacs)
      ;; remove explicitly numbered groups (xemacs does not support them)
      (while (and (string-match "\\\\(\\?\\([0-9]+\\):" regexp)
		  (rxx-subregexp-context-p regexp (match-beginning 0)))
	(setq regexp (replace-match "" 'fixedcase 'literal regexp 1))))
    ;; remove unnumbered, non-shy groups.
    (if (featurep 'xemacs)
	(while (and (string-match "\\\\(\\([^?]\\)" regexp)
		    (rxx-subregexp-context-p regexp (match-beginning 0))
		    )
	  (setq regexp (replace-match "\\(?:\\1" 'fixedcase (not 'literal) regexp)))
    (while (and (string-match "\\\\(\\(\\)[^?]" regexp)
		(rxx-subregexp-context-p regexp (match-beginning 0)))
      (setq regexp (replace-match "?:" 'fixedcase 'literal regexp 1))))
    (rxx-check-regexp-valid regexp)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: rxx-info
;;
;; Extra information we store with each constructed regexp.  This information
;; lets us reuse the regexp as a building block of larger regexps.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

     ENV - environment for resolving references to named subgroups of this regexp.  Maps subgroup name to
       `rxx-info' for the subgroup.
     NUM - the numbered group corresponding to to matches of this regexp (as would be passed to `match-string').
"
  form parser env num descr)

(defun put-rxx-info (regexp rxx-info)
  "Put rxx-info on a regexp string, replacing any already there.
This creates an aregexp (annotated regexp).
Return the annotated regexp."
  (put-text-property 0 (length regexp) 'rxx rxx-info regexp)
  (when (featurep 'xemacs)
      (put regexp 'rxx rxx-info))
  regexp)

(defun get-rxx-info (aregexp)
  "Extract rxx-info from regexp string AREGEXP,
if there, otherwise return nil."
  (when (stringp aregexp)
    (or (get-text-property 0 'rxx aregexp)
	(and (featurep 'xemacs)
	     (get aregexp 'rxx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Environments
;;
;; Map from named group name to information about that named group.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rxx-new-env (&optional parent-env)
  "Create a fresh environment mapping group names to rxx-infos,
with the parent environment PARENT-ENV.
There is an environment for the top-level regexp, and also a
separate one within each named group (for nested named groups).
We represent the environment as an alist.  The alist always has
at least one cell; this lets us add entries to an environment
that is bound (pointed to) by several lisp symbols, without
having to find and rebind all the symbols."
  (list (cons nil parent-env)))

(defun rxx-parent-env (rxx-env)
  "Return the parent environment of the environment RXX-ENV."
  (cdr (first rxx-env)))

(defun rxx-env-lookup (grp-name rxx-env)
  "Lookup the rxx-info for the named group GRP-NAME in the
environment RXX-ENV, or return nil if not found.  GRP-NAME is
either a symbol, or a list of symbols indicating a path through
nested named groups.  Since multiple groups may be bound to the
same name in an environment, this returns a list."
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
  "Return the list of top-level named groups in the environment RXX-ENV."
  (let (all-grps)
    (do-rxx-env grp-name rxx-infos rxx-env
      (push grp-name all-grps))
    all-grps))

(defun rxx-env-empty-p (rxx-env)
  "Return true if there are no bindings in the environment RXX-ENV."
  (null (cdr rxx-env)))

(defun rxx-named-grp-num (grp-name &optional aregexp)
  "Look up the explicitly numbered group number assigned to the
given named group, for passing as the SUBEXP argument to routines
such as `replace-match', `match-substitute-replacement' or
`replace-regexp-in-string'.  The annotated regexp must either be
passed in as AREGEXP or scoped in as RXX-AREGEXP. "
  (declare (special rxx-aregexp))
  (mapcar 'rxx-info-num
   (rxx-env-lookup
    grp-name
    (rxx-info-env
     (get-rxx-info
      (or aregexp (when (boundp 'rxx-aregexp) rxx-aregexp)
	  (error "The annotated regexp must be either passed in explicitly, or scoped in as `rxx-aregexp'")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Parsing the result of a regexp match into a programmatic object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		  (error "Annotated regexp created by `rxx' must either be passed in, or scoped in via RXX-AREGEXP"))))))
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

(defun rxx-get-marker (pos)
  "If searching within a string copied from the buffer,
convert position within the string to buffer marker position"
  (when (boundp 'rxx-marker)
    (let ((m (copy-marker rxx-marker)))
      (setq pos (set-marker m (+ (marker-position m) pos) (marker-buffer m)))))
  pos)

(defun rxx-match-beginning (grp-name &optional object aregexp)
  "Return the beginning position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special grp-num))
  (rxx-match-aux
   (lambda () (rxx-get-marker (match-beginning grp-num)))))

(defun rxx-match-end (grp-name &optional object aregexp)
  "Return the end position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special grp-num))
  (rxx-match-aux (lambda () (rxx-get-marker (match-end grp-num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: A simple module system for aregexp names
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rxx-start-module (prefix)
  "Specify a prefix to be automatically prepended to aregexps defined by `defrxx'.  Typically this would be the
name of the module in which the aregexps are being defined.   So, if you do (rxx-set-prefix my-module) then
\(defrxx val ...) defines aregexp named my-module-val-regexp; it can be referred to as simply `val' when used
in larger regexps."
  `(eval-and-compile
     (defrxxconst rxx-prefix (when (quote ,prefix) (symbol-name (quote ,prefix)))
       "Module name to prefix aregexp names with.")))

(defvar rxx-imports nil
  "Symbols imported from other modules using `rxx-import'.
An alist mapping imported symbol name to module name.")

(defmacro rxx-import (module &rest symbols)
  "Import aregexp symbols SYMBOLS from module MODULE so they can be used
without module name prefix."
  (elu-with-new-symbols symbol
  `(eval-and-compile
     (dolist (,symbol (quote ,symbols))
       (push (cons ,symbol (symbol-name (quote ,module))) rxx-imports)))))

(defmacro rxx-end-module (prefix)
  "End the specified module."
  `(eval-and-compile
     (assert (equal (symbol-name (quote ,prefix)) rxx-prefix))
     (rxx-start-module nil)
     (setq rxx-imports nil)))

(defun rxx-symbol (symbol &optional no-regexp)
  "If rxx prefix is defined (see `rxx-set-prefix'), and SYMBOL does not
end with -regexp or -re, return the full name of the symbol (prefix-SYMBOL-regexp).
If NO-REGEXP is non-nil, do not append the -regexp part and just prepend the prefix."
  (let ((prefix (or (elu-assoc-val symbol rxx-imports 'nil-ok)
		    (elu-safe-val rxx-prefix))))
    (if (and prefix
	     (not (elu-ends-with (symbol-name symbol) "-regexp"))
	     (not (elu-ends-with (symbol-name symbol) "-re")))
	(intern (concat prefix "-" (symbol-name symbol) (if no-regexp "" "-regexp")))
    symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Handling new `rx' forms, and extended handling of existing forms
;;          to recognize the use of aregexps.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rxx-process-named-grp (form)
  "Process the (named-grp GRP-NAME GRP-DEF) form, when called from `rx-to-string'.
GRP-DEF can be an annotated regexp, a plain regexp, or a form to
be recursively interpreted by `rxx'.  If it is an annotated
regexp, you can call `rxx-match-val' after doing a match to get
the parsed object matched by this named group."
  (declare (special rxx-num-grps rxx-env))
  (rx-check form)
  (or
   (and (boundp 'rxx-replace-named-grps)
	   (cdr-safe (assoc (second form) rxx-replace-named-grps)))
   (let* ((grp-name (second form))
	  (grp-def-raw (third form))
	  (grp-num (incf rxx-num-grps))
	  (old-rxx-env rxx-env)
	  (rxx-env (rxx-new-env old-rxx-env))  ;; within each named group, a new environment for group names
	  (grp-def
	   (or (and (symbolp grp-def-raw)
		    (boundp (rxx-symbol grp-def-raw))
		    (get-rxx-info (symbol-value (rxx-symbol grp-def-raw))))
	       (and (eq (car-safe grp-def-raw) 'regexp)
		    (get-rxx-info (second grp-def-raw)))
	       (and (eq (car-safe grp-def-raw) 'eval-regexp)
		    (get-rxx-info (eval (second grp-def-raw))))
	       (make-rxx-info :parser 'identity :env (rxx-new-env)
			      :form (or grp-def-raw (error "Missing named group definition: %s" form)))))
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
			     :form (rxx-info-form grp-def)) old-rxx-env)
     regexp-here)))

(defun rxx-process-named-backref (form)
  "Process the (named-backref GRP-NAME) form, when called from `rx-to-string'."
  (declare (special rxx-env))
  (rx-check form)
  (let* ((grp-name (second form))
	 (prev-grp-defs (rxx-env-lookup grp-name rxx-env)))
    (unless prev-grp-defs (error "Group in backref not yet seen: %s" grp-name))
    (unless (= (length prev-grp-defs) 1) (error "Ambiguous backref to group %s" grp-name))
    (rx-backref `(backref ,(rxx-info-num (first prev-grp-defs))))))

(defun rxx-process-eval-regexp (form &optional rx-parent)
  "Parse and produce code from FORM, which is `(eval-regexp FORM)'."
  (declare (special rxx-num-grps))
  (rx-check form)
  (let ((regexp (eval (second form))))
    (incf rxx-num-grps (regexp-opt-depth regexp))
    (concat "\\(?:" (rx-group-if regexp rx-parent) "\\)")))


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

;; (defun rx-or (form)
;;   "Parse and produce code from FORM, which is `(or FORM1 ...)'."
;;   (rx-check form)
;;   (rx-group-if
;;    (if (memq nil (mapcar 'stringp (cdr form)))
;;        (mapconcat (lambda (x) (rx-form x '|)) (cdr form) "\\|")
;;      (regexp-opt (cdr form)))
;;    (and (memq rx-parent '(: * t)) rx-parent)))

(defadvice rx-or (around rxx-or first (form) )
  "When called via `rxx-to-string', modify `rx-or' behavior
as described below.

For bounded recursion, remove any OR clauses consisting of
`recurs' forms for which recursion has bottomed out. (See
`rxx-parse-recurs' and `defrxxrecurse').  Formally, these `recurs'
forms represent never-matching expressions when they bottom out;
but simply removing them leads to a more compact result.

Also, keep track of the current path through the OR clauses.
This lets us disable particular OR clauses, for the purpose of
splitting up a large regexp into an OR of more manageable chunks.

Also, force each clause of the OR, as well as the whole expression,
to be wrapped in shy groups; this can prevent unexpected behaviors.
"
  (if (not (in-rxx)) ad-do-it
    
    )
  
  (unless (or (not (boundp 'rxx-env))
	      (and (boundp 'rxx-recurs-depth) (> rxx-recurs-depth 0))
	      (<= (length form) 2))
    (setq form (elu-remove-if
		(lambda (elem)
		  (or (eq (car-safe elem) 'recurse)
		      (and (eq (car-safe elem) 'named-grp)
			   (eq (elu-caaddr-safe elem) 'recurse))))
			  form)))
  ad-do-it
  (when (boundp 'rxx-env)
    (setq ad-return-value (rx-group-if ad-return-value '*))))

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
			    (elu-push-end (quote ,sep-by-form) repeat-form))
			  (let ((new-grp-name (make-symbol "new-grp")))
			    (elu-push-end new-grp-name repeat-grp-names)
			    (elu-push-end (list 'named-grp new-grp-name
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Defining aregexps
;;
;; User-callable functions and macros for defining aregexps.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		     :env rxx-env :descr descr)))
     (put-rxx-info regexp rxx-info)
     (setq regexp (rxx-replace-posix regexp))
     (assert (= rxx-num-grps (regexp-opt-depth regexp)))
     (rxx-check-regexp-valid regexp))))

(defun in-rxx ()
  "Test if we're in rxx, i.e. if we got here through a call
to `rxx-to-string'.  Used by pieces of advice to avoid changing
any behavior unless called through this module.

The use of this function means we specifically do NOT want
to declare `rxx-env' as a global variable using `defvar';
we only want it to be defined when we're being called via
`rxx-to-string'.
"
  (declare (special rxx-env))
  (boundp 'rxx-env))


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


(defmacro defrxx (var &rest args)
  "Define an annotated regexp (aregexp), and a parser that constructs
programmatic objects from matches to the regexp.

VAR is the name of the global variable to which the new regexp will be assigned.
If a module definition is active (see `rxx-start-module'), the variable
name will be modified by prepending the module name and then -regexp suffix.

ARGS can be one of: (DESCR FORM PARSER), (FORM DESCR PARSER) or (FORM PARSER DESCR).
See docstring of macro `rxx' for the meaning of these three arguments.
"
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
	`(defconst ,(rxx-symbol var) (rxx-to-string (quote ,form) (quote ,parser) ,descr) ,(or descr ""))
      (let* ((aregexp (rxx-to-string form parser descr))
	     struct-def)
	(when (eq parser 'struct)
	  (let* ((rxx-info (get-rxx-info aregexp))
		 (struct-name (symbol-name (rxx-symbol var 'no-regexp)))
		 (grps (rxx-env-groups (rxx-info-env rxx-info))))
	    (setf (rxx-info-parser rxx-info)
		  (cons (intern (concat "make-" struct-name))
			(apply 'append (mapcar (lambda (field) (list (intern (concat ":" (symbol-name field)))
								     field))
					       grps))))
	    (setq struct-def
		  `(defstruct ,(intern struct-name) ,(or descr "Parsed regexp")
		      ,@grps))))
	(list 'progn struct-def
	      `(defrxxconst ,(rxx-symbol var) ,aregexp ,descr))))))

(defmacro defrxxrecurse (depth var regexp &optional parser descr)
  "Same as `defrxx', but instantiates any recursive invocations 
to the specified DEPTH.   See description of the `recuse' form in
`rxx'."
  (if (featurep 'xemacs)
    `(defconst ,(rxx-symbol var) (let ((rxx-recurs-depth ,depth)) (rxx-to-string (quote ,regexp)
										 (quote ,parser) ,descr))
       ,(or descr ""))
  `(defrxxconst ,var ,(let ((rxx-recurs-depth depth))
			(rxx-to-string regexp parser descr)) ,descr)))


(defmacro rxxlet* (bindings &rest forms)
  (list 'let* (mapcar (lambda (binding) (list (first binding)
					      (list 'rxx (second binding) (third binding) (symbol-name (first binding)))))
		      bindings)
	(or (car-safe forms) (and bindings (car-safe (car-safe (last bindings)))))))

(defmacro defrxxconst (symbol initvalue &optional docstring)
  "Define a constant referenced from a `defrxx' definition.
If `defrxx' forms are evaluated at compile-time,
any variables they reference need to be made visible at
compile-time as well.  Other than making the value visible
at compile-time, works exactly like a `defconst': defines
SYMBOL as a constant value INITVALUE, and documents the created
global variable with the optional DOCSTRING."
  (if (featurep 'xemacs)
      `(defconst ,symbol ,initvalue ,(or docstring ""))
    `(eval-and-compile
       (defconst ,symbol ,initvalue ,(or docstring "")))))

(defmacro defrxxcustom (symbol initvalue docstring &rest args)
  "Defines a customization visible at compile-time, so that it
can be referenced from `defrxx' definitions.   See `defrxxconst'
for a fuller explanation."
  `(eval-and-compile
     (defcustom ,symbol ,initvalue ,docstring ,@args)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Searching and parsing
;;
;; User-callable functions for searching and parsing aregexps.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rxx-longest-match nil
  "When non-nil, make extra efforts to find the longest match rather than
just any match.  Affects `rxx-parse', etc.  See also `posix-search-forward'.")

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
	  (unless error-ok (error "%s: No match" error-msg))
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

(defun rxx-parse-fwd-one (aregexp &optional bound partial-match-ok)
  (save-match-data
    (save-excursion
      (rxx-search-fwd aregexp bound (not 'noerror) partial-match-ok))))


(defun* rxx-parse-fwd-old (aregexps &optional bound partial-match-ok)
  (let (ok-results nil-results error-results (aregexps (elu-make-seq aregexps)))
    (dolist (aregexp aregexps)
      (condition-case err
	  (let ((result (rxx-parse-fwd-one aregexp bound partial-match-ok)))
	    (if result
		(push result ok-results)
	      (push result nil-results)))
	(error (push err error-results))))
    (if ok-results (first ok-results)
      (if nil-results nil
	(let ((err (first error-results)))
	  (signal (car err) (cdr err)))))))



(defun* rxx-parse-fwd (aregexps &optional bound partial-match-ok)
  (let (ok-results nil-results error-results (aregexps (elu-make-seq aregexps)))
    (dolist (aregexp aregexps)
      (condition-case err
	  (let* ((rxx-marker (point-marker))
		 (the-str (buffer-substring-no-properties (point) (or bound (point-max))))
		 (result (rxx-parse aregexp the-str
				    partial-match-ok (not 'error-ok))))
	    (if result
		(push result ok-results)
	      (push result nil-results)))
	(error (push err error-results))))
    (if ok-results (first ok-results)
      (if nil-results nil
	(let ((err (first error-results)))
	  (signal (car err) (cdr err)))))))



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

;; additional forms:
;;   seq-any-order
;;   sep-by-any-order
;;
;; debug/better-error-messaging facilities
;; enumerate matches
;; default parsers: if only one named grp, the value of that; if two or more, make a struct with these fields as names.

;; better support for defining these things at runtime.


(provide 'rxx)



(provide 'rxx)

;;; rxx.el ends here

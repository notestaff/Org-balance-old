;;; rxx.el --- Create recursive grammars using nested regexps.
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

;; user-callable functions: rxx-named-grp-num, rxx, rxx-match-val, rxx-match-string, rxx-parse

(require 'rx)
(eval-when-compile (require 'cl))

;; struct: rxx-info - information about a regexp or one named group within it.  when the former, it is attached to a regexp
;;    as a text property by `put-rxx-info'; the resulting annotated regexp is referred to as an aregexp in this module.
(defstruct rxx-info
  ;; field: form - the lisp form passed to `rx-to-string' to generate this regexp
  form
  ;; field: regexp - the regexp string returned by `rx-to-string'
  regexp
  ;; field: parser - function that takes a string matching this regexp, and constructs a parsed object from it.
  ;;   in the simplest case it is the identity function that just returns the matched string itself.
  ;;   the function can call `rxx-match-val' and `rxx-match-string' to get the parsed object or the string matched by
  ;;   a named group of this regexp.
  parser
  ;; field: env - environment for resolving the names of named groups of this regexp.  maps name to rxx-info for the group.
  env
  ;; field: num - if this rxx-info is for the top-level regexp, nil; else, the number of the explicitly numbered group
  ;; to which this named group was mapped.
  num
  ;; field: descr - a string describing what is matched by this regexp; used for creating readable error messages.
  descr)

(defun get-rxx-info (aregexp)
  "Extract rxx-info from regexp string, if there, otherwise return nil."
  (when (stringp aregexp) (get-text-property 0 'rxx aregexp)))

(defun put-rxx-info (regexp rxx-info)
  "Put rxx-info on a regexp string, replacing any already there.  This creates an aregexp (annotated regexp)."
    (put-text-property 0 (length regexp) 'rxx rxx-info regexp))

(defun rxx-new-env ()
  "Create a fresh environment mapping group names to rxx-infos.  There is an environment for the top-level regexp, and
also a separate one within each named group (for nested named groups).  We represent the environment as an alist.
The alist always has at least one cell; this lets us add entries to an environment that is bound (pointed to) by
several lisp symbols, without having to find and rebind all the symbols."
  (list (cons nil nil)))

(defun rxx-env-lookup (grp-name rxx-env)
  "Lookup the rxx-info for the named group GRP-NAME in the environment RXX-ENV, or return nil if not found.  GRP-NAME is
either a symbol, or a list of symbols indicating a path through nested named groups.  Since multiple groups may be
bound to the same name in an environment, this returns a list."
  (when (symbolp grp-name) (setq grp-name (list grp-name)))
  (let ((grp-infos (cdr-safe (assq (first grp-name) rxx-env))))
    (apply 'append
	   (mapcar
	    (lambda (grp-info)
	      (if (cdr grp-name)
		  (rxx-env-lookup (cdr grp-name)
				  (rxx-info-env grp-info))
		(list grp-info)))
	    grp-infos))))

(defun rxx-env-bind (grp-name rxx-info rxx-env)
  "Bind group name GRP-NAME to group annotation RXX-INFO in the
environment RXX-ENV.  If already bound, add to the binding."
  (let ((entry (or (assq grp-name rxx-env)
		   (let ((new-entry (list (cons grp-name nil))))
		     (nconc rxx-env new-entry)
		     (first new-entry)))))
    (setcdr entry (cons rxx-info (cdr entry)))
    rxx-env))
    

(defun rxx-named-grp-num (grp-name &optional aregexp)
  "Look up the explicitly numbered group number assigned to the given named group, for passing as the SUBEXP argument
to routines such as `replace-match', `match-substitute-replacement' or `replace-regexp-in-string'.
The annotated regexp must either be passed in as AREGEXP or scoped in as RXX-AREGEXP. "
  (declare (special rxx-aregexp))
  (rxx-info-num
   (rxx-env-lookup
    grp-name
    (rxx-info-env
     (get-rxx-info
      (or aregexp (when (boundp 'rxx-aregexp) rxx-aregexp)
	  (error "The annotated regexp must be either passed in explicitly, or scoped in as `rxx-aregexp'.")))))))

(defun rxx-process-named-grp (form)
  "Process the (named-grp GRP-NAME GRP-DEF) form, when called from `rx-to-string'.  GRP-DEF can be an annotated regexp,
a plain regexp, or a form to be recursively interpreted by `rxx'.  If it is an annotated regexp, you can call 
`rxx-match-val' after doing a match to get the parsed object matched by this named group."

  ;
  ; if the form is a symbol, and not one of the reserved ones in rx,
  ; evaluate it as a variable.
  ;
  
  (declare (special rxx-next-grp-num rxx-env))
  (rx-check form)
  (let* ((grp-name (second form))
	 (grp-def-raw (third form))
	 (old-grp-defs (rxx-env-lookup grp-name rxx-env))
	 (equiv-old-grp-defs
	  (delq nil
		(mapcar (lambda (old-grp-def)
			  (when (or (null grp-def-raw)  (equal (rxx-info-form old-grp-def) grp-def-raw))
			    old-grp-def))
			old-grp-defs))))
    (if equiv-old-grp-defs (rxx-info-regexp (first equiv-old-grp-defs))
      (let* ((grp-num (incf rxx-next-grp-num))  ;; reserve a numbered group number unique within a top-level regexp
	     (old-rxx-env rxx-env)
	     (rxx-env (rxx-new-env))  ;; within each named group, a new environment for group names
	     (grp-def
	      (or (and (symbolp grp-def-raw)
		       (boundp grp-def-raw)
		       (get-rxx-info (symbol-value grp-def-raw)))
		  (and (listp grp-def-raw)
		       (eq (first grp-def-raw) 'regexp)
		       (get-rxx-info (second grp-def-raw)))
		  (and (listp grp-def-raw)
		       (eq (first grp-def-raw) 'eval-regexp)
		       (get-rxx-info (eval (second grp-def-raw))))
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
	     (regexp-here (format "\\(?%d:%s\\)" grp-num
				  (if (and (boundp 'rxx-disable-grps) (member grp-name rxx-disable-grps))
				      (progn
					(message "DISABLING %s" grp-name)
					(rx-to-string (rxx-info-form grp-def))
					".*")
				    (rx-to-string (rxx-info-form grp-def))))))
	(rxx-env-bind grp-name (make-rxx-info
				:num grp-num
				:parser (rxx-info-parser grp-def)
				:env rxx-env
				:regexp regexp-here
				:form (rxx-info-form grp-def)) old-rxx-env)
	regexp-here))))


(defun rxx-process-named-backref (form)
  "Process the (named-backref GRP-NAME) form, when called from `rx-to-string'."
  (rx-check form)
  (let* ((grp-name (second form))
	 (prev-grp-def (rxx-env-lookup grp-name rxx-env)))
    (unless prev-grp-def (error "Group in backref not yet seen: %s" grp-name))
    (rx-backref `(backref ,(rxx-info-num prev-grp-def)))))

(defun rxx-call-parser (rxx-info match-str)
  (let* ((symbols (delq nil (mapcar 'car (rxx-info-env rxx-info))))
	  (symbol-vals (mapcar 'rxx-match-val symbols))
	  (parser (rxx-info-parser rxx-info)))
    (progv symbols symbol-vals
      (if (functionp parser)
	    (funcall parser match-str)
	(eval parser)))))

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
	(unless (= (length matches-here) 1) (error "More than one match to group %s" grp-name))
	(let* ((match-info-here (first matches-here))
	       (match-here (car match-info-here))
	       (grp-info (cdr match-info-here))
	       (grp-num (rxx-info-num grp-info)))
	  (funcall code))))))

(defun rxx-match-val (grp-name &optional object aregexp)
  "Return the parsed object matched by named group GRP-NAME.  OBJECT, if given, is the string or buffer we last searched;
may be scoped in via RXX-OBJECT.  The annotated regexp must either be passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-object rxx-aregexp))
  (rxx-match-aux
   (lambda ()
       (let ((rxx-env (rxx-info-env grp-info))) 
	 (rxx-call-parser grp-info match-here)))))

(defun rxx-match-string (grp-name &optional object aregexp)
  "Return the substring matched by named group GRP-NAME.  OBJECT, if given, is the string or buffer we last searched;
may be scoped in via RXX-OBJECT.  The annotated regexp must either be passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-object rxx-aregexp))
  (rxx-match-aux (lambda () match-here)))

(defun rxx-match-beginning (grp-name &optional object aregexp)
  "Return the beginning position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-aregexp))
  (rxx-match-aux (lambda () (match-beginning grp-num))))

(defun rxx-match-end (grp-name &optional object aregexp)
  "Return the end position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-aregexp))
  (rxx-match-aux (lambda () (match-end grp-num))))

(defconst rxx-first-grp-num 1
  "When generating group numbers for explicitly numbered groups corresponding to named groups in a regexp, start
with this number.") 

(defun rxx-process-eval-regexp (form)
  "Parse and produce code from FORM, which is `(eval-regexp FORM)'."
  (rx-check form)
  (rx-group-if (eval (cadr form)) rx-parent))


(defun rxx-to-string (form &optional parser descr)
  "Construct a regexp from its readable representation as a lisp FORM, using the syntax of `rx-to-string' with some
extensions.  The extensions, taken together, allow specifying simple grammars
in a modular fashion using regular expressions.

For detailed description, see `rxx'.
"
  (declare (special rxx-first-grp-num))
  (let* ((rxx-env (rxx-new-env))
	 (rxx-next-grp-num rxx-first-grp-num)

	 ;; extend the syntax understood by `rx-to-string' with named groups and backrefs
	 (rx-constituents (append '((named-grp . (rxx-process-named-grp 1 nil))
				    (eval-regexp . (rxx-process-eval-regexp 1 1))
				    (shy-grp . seq)
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
	  (rx-to-string form))
	 (rxx-info (make-rxx-info
		    :form form :parser (if parser parser 'identity)
		    :env rxx-env :descr descr
		    )))
    (put-rxx-info regexp rxx-info)
    regexp))

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
  
  (rxx-to-string form parser descr))

(defmacro rxxlet* (bindings forms)
  (list 'let* (mapcar (lambda (binding) (list (first binding) (list 'rxx (second binding) (third binding) (symbol-name (first binding)))))
		      bindings)
	forms))

(defun rxx-parse (aregexp s &optional partial-match-ok)
  "Match the string against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
  (save-match-data
    (let ((rxx-info (or (get-rxx-info aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp))))
      (if (and (string-match aregexp s)
	       (or partial-match-ok
		   (and (= (match-beginning 0) 0)
			(= (match-end 0) (length s)))))
	  (let* ((rxx-env (rxx-info-env rxx-info))
		 (rxx-object s))
	    (rxx-call-parser rxx-info (match-string 0 s)))
	(error "Error parsing \`%s\' as %s" s
	       (or (rxx-info-descr rxx-info) (rxx-info-form rxx-info)))))))

(provide 'rxx)


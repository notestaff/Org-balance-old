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

(defmacro rxx-flet (bindings &rest body)
  "Temporarily replace functions, making previous definitions available.  Also, lets you use a function symbol
for the replacement function definition."
  (declare (indent 1))
  `(let 
       ,(mapcar (lambda (binding)
		  (let ((orig-func-symbol (intern (concat (symbol-name (first binding)) "-orig"))))
		    (list orig-func-symbol
			  `(if (boundp (quote ,orig-func-symbol)) ,orig-func-symbol
			     (when (fboundp (quote ,(first binding)))
			       (symbol-function (quote ,(first binding)))))))) bindings)
     (flet ,(append
	     (mapcar
	      (lambda (binding)
		(let ((orig-fn (intern (concat (symbol-name (first binding)) "-orig"))))
		  (list orig-fn '(&rest args) `(apply (symbol-value (quote ,orig-fn)) args))))
	      bindings)
	     (mapcar
	      (lambda (binding)
		(if (and (= (length binding) 2)
			 (symbolp (second binding)))
		    (list (first binding) '(&rest args) `(apply (quote ,(second binding)) args))
		  binding))
	      bindings))
       ,@body)))

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

(defun rxx-new-env (&optional parent-env)
  "Create a fresh environment mapping group names to rxx-infos.  There is an environment for the top-level regexp, and
also a separate one within each named group (for nested named groups).  We represent the environment as an alist.
The alist always has at least one cell; this lets us add entries to an environment that is bound (pointed to) by
several lisp symbols, without having to find and rebind all the symbols."
  (list (cons nil parent-env)))

(defun rxx-parent-env (rxx-env) (cdr (first rxx-env)))

(defun rxx-env-lookup (grp-name rxx-env)
  "Lookup the rxx-info for the named group GRP-NAME in the environment RXX-ENV, or return nil if not found.  GRP-NAME is
either a symbol, or a list of symbols indicating a path through nested named groups.  Since multiple groups may be
bound to the same name in an environment, this returns a list."
  (when (symbolp grp-name) (setq grp-name (list grp-name)))
  (if (eq (first grp-name) '..)
      (rxx-env-lookup (cdr grp-name) (rxx-parent-env rxx-env))
    (let ((grp-infos (cdr-safe (assq (first grp-name) (cdr rxx-env)))))
      (apply 'append
	     (mapcar
	      (lambda (grp-info)
		(if (cdr grp-name)
		    (rxx-env-lookup (cdr grp-name)
				    (rxx-info-env grp-info))
		  (list grp-info)))
	      grp-infos)))))

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

(defun rxx-env-empty-p (rxx-env)
  "Return true if there are no bindings"
  (null (cdr rxx-env)))

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

(defun rxx-make-shy (re)
  "Make all groups in re shy; and wrap a shy group around the re.  WARNING: this will kill any backrefs!"
  ;; FIXME: check for backrefs, throw error if any present
  (rx-group-if
   (save-match-data
     (replace-regexp-in-string
      (rx (seq "\\(" (group "") (not (any "?"))))
      "?:" (save-match-data
	     (replace-regexp-in-string
	      (rx (seq "\\(?" (group (one-or-more digit) ) ":"))
	      "" re 'fixedcase 'literal 1))
      'fixedcase 'literal 1))
   '*))

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
    (if equiv-old-grp-defs (rxx-info-regexp (first equiv-old-grp-defs))
      (let* ((grp-num (incf rxx-next-grp-num))  ;; reserve a numbered group number unique within a top-level regexp
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
	     (regexp-here (format "\\(?%d:%s\\)" grp-num
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



;;; Support for `progv': copied from `cl' package to avoid runtime dependence on it.
(defvar rxx-progv-save)
;;;###autoload
(defun rxx-progv-before (syms values)
  (while syms
    (push (if (boundp (car syms))
		 (cons (car syms) (symbol-value (car syms)))
	       (car syms)) rxx-progv-save)
    (if values
	(set (pop syms) (pop values))
      (makunbound (pop syms)))))

(defun rxx-progv-after ()
  (while rxx-progv-save
    (if (consp (car rxx-progv-save))
	(set (car (car rxx-progv-save)) (cdr (car rxx-progv-save)))
      (makunbound (car rxx-progv-save)))
    (pop rxx-progv-save)))

(defmacro rxx-progv (symbols values &rest body)
  "Bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each symbol in the first list is bound to the corresponding value in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time."
  (list 'let '((rxx-progv-save nil))
	(list 'unwind-protect
	      (list* 'progn (list 'rxx-progv-before symbols values) body)
	      '(rxx-progv-after))))


(defun rxx-call-parser (rxx-info match-str)
  (let ((rxx-env (rxx-info-env rxx-info)))
    (let* ((symbols (delq nil (mapcar 'car (rxx-info-env rxx-info))))
	   (symbol-vals (mapcar
			 (lambda (symbol)
			   (rxx-match-val symbol))
			 symbols))
	   (parser (rxx-info-parser rxx-info)))
      (rxx-progv symbols symbol-vals
	(let ((parser-result
	       (if (functionp parser)
		   (funcall parser match-str)
		 (eval parser))))
	  parser-result)))))

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

(defconst rxx-first-grp-num 1
  "When generating group numbers for explicitly numbered groups corresponding to named groups in a regexp, start
with this number.") 

(defun rxx-process-eval-regexp (form &optional rx-parent)
  "Parse and produce code from FORM, which is `(eval-regexp FORM)'."
  (rx-check form)
  (rx-group-if (eval (cadr form)) rx-parent))


(defmacro rxx-replace-posix (s)
  "Replace posix classes in regular expression.  Taken from `org-re' in `org-macs.el'."
  (if (featurep 'xemacs)
      (let ((ss s))
	(save-match-data
	  (while (string-match "\\[:alnum:\\]" ss)
	    (setq ss (replace-match "a-zA-Z0-9" t t ss)))
	  (while (string-match "\\[:word:\\]" ss)
	    (setq ss (replace-match "a-zA-Z0-9" t t ss)))
	  (while (string-match "\\[:alpha:\\]" ss)
	    (setq ss (replace-match "a-zA-Z" t t ss)))
	  (while (string-match "\\[:punct:\\]" ss)
	    (setq ss (replace-match "\001-@[-`{-~" t t ss)))
	  ss))
    s))


(defmacro rxx-dbg (&rest exprs)
  "Print the values of exprs, so you can write e.g. (dbg a b) to print 'a=1 b=2'.
Returns the value of the last expression."
  `(let ((expr-vals (list ,@exprs)))
     (when nil (apply 'message
		      (append (list ,(mapconcat (lambda (expr)
						  (concat (format "%s" expr) "=%s "))
						exprs
						""))
		    expr-vals)))
     (car-safe (with-no-warnings (last expr-vals)))))

;(defmacro rxx-dbg (&rest exprs) (last exprs))

(defmacro rxx-safe-val (x) `(if (boundp (quote ,x)) ,x 'unbound))

(defun rxx-symbol (symbol)
  (if (and (boundp 'rxx-prefix) rxx-prefix (not (save-match-data (string-match "-regexp$" (symbol-name symbol)))))
      (intern (concat rxx-prefix "-" (symbol-name symbol) "-regexp"))
    symbol))

(defun rxx-ends-with (s end)
  "Test if S ends with END"
  (and (>= (length s) (length end)) (string= (substring s (- (length s) (length end))) end)))

(defadvice rx-form (around rxx-form first (form &optional rx-parent) activate compile)
  (declare (special rxx-env))
  (if (not (boundp 'rxx-env))
      ad-do-it

    (when (and (symbolp form) (rxx-ends-with (symbol-name form) "?"))
      (setq form (list 'opt (intern (substring (symbol-name form) 0 -1)))))
    (cond ((and (consp form) (symbolp (first form)) (boundp (rxx-symbol (first form)))
		(get-rxx-info (symbol-value (rxx-symbol (first form)))))
	   (setq ad-return-value (rxx-process-named-grp (list 'named-grp (second form) (rxx-symbol (first form))))))
	  ((and (symbolp form) (boundp (rxx-symbol form)) (boundp 'rxx-env) (not (rxx-env-lookup (rxx-symbol form) rxx-env))
		(get-rxx-info (symbol-value (rxx-symbol form))))
	   (setq ad-return-value
		 (rxx-process-named-grp (list 'named-grp form form))))
	  (t ad-do-it))))

(defun rxx-to-string (form &optional parser descr)
  "Construct a regexp from its readable representation as a lisp FORM, using the syntax of `rx-to-string' with some
extensions.  The extensions, taken together, allow specifying simple grammars
in a modular fashion using regular expressions.

For detailed description, see `rxx'.
"
  (declare (special rxx-first-grp-num))
  (rxx-remove-unneeded-shy-grps
   (let* ((rxx-env (rxx-new-env))
	  (rxx-next-grp-num rxx-first-grp-num)
	  ;; extend the syntax understood by `rx-to-string' with named groups and backrefs
	  (rx-constituents (append '((named-grp . (rxx-process-named-grp 1 nil))
				     (eval-regexp . (rxx-process-eval-regexp 1 1))
				     (shy-grp . seq)
				     (& . seq)
				     (blanks . "\\(?:[[:blank:]]+\\)")
				     (digits . "\\(?:[[:digit:]]+\\)")
				     (blanks? . "\\(?:[[:blank:]]*\\)")
				     (digits? . "\\(?:[[:digit:]]*\\)")
				     (sep-by . (rxx-process-sep-by 1 nil))
				     (recurse . (rxx-process-recurse 1 nil))
				     (named-grp-recurs . (rxx-process-named-grp-recurs 1 nil))
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
     (rxx-replace-posix regexp))))

(defun rxx-process-sep-by (form)
  "Process the sep-by form, which looks like (sep-by separator ....)"
  (let ((separator (second form))
	(seq-elems (cddr form)))
    (rx-form
     (let ((form-with-separators '(seq)) seen-non-optional)
       (dolist (seq-elem seq-elems form-with-separators)

	 (when (and (symbolp seq-elem) (rxx-ends-with (symbol-name seq-elem) "?"))
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

(defconst rxx-never-match (rx (not (any ascii nonascii))))

(defun rxx-process-named-grp-recurs (form)
  "Process named-grp-recurs"
  (declare (special rxx-recurs-depth rxx-env))
  (if
      (or (not (boundp (quote rxx-recurs-depth)))
	  (< rxx-recurs-depth 1))
      (progn
	(rxx-env-bind (second form) (make-rxx-info :form `(regexp ,rxx-never-match)
						   :env (rxx-new-env) :num 0
						   :parser (lambda (match) nil))
		      rxx-env)
	rxx-never-match)
    (let ((rxx-recurs-depth (1- rxx-recurs-depth)))
      (rxx-process-named-grp `(named-grp ,(second form) (eval-regexp ,(third form)))))))


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

  (rxx-to-string form parser descr))


(defmacro rxxlet* (bindings &rest forms)
  (list 'let* (mapcar (lambda (binding) (list (first binding)
					      (list 'rxx (second binding) (third binding) (symbol-name (first binding)))))
		      bindings)
	(or (car-safe forms) (and bindings (car-safe (car-safe (last bindings)))))))

(defun rxx-parse (aregexp s &optional partial-match-ok error-ok)
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
				 "buffer text") aregexp)))
	(if (not (re-search-forward aregexp bound 'noerror))
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
    (rxx-parse (rxx-to-string unwound-aregexp) s partial-match-ok)
  ))

(defmacro rxx-push-end (elt lst)
  "Push elt onto end of list, and return the elt"
  `(progn
     (setq ,lst (append ,lst (list ,elt)))
     ,elt))

(defvar rx-greedy-flag)

(defadvice rx-kleene (around rxx-kleene first (form) activate)
  (declare (special rxx-env rxx-next-grp-num))
  (if (or (not (boundp 'rxx-env)) (not (boundp 'rxx-next-grp-num))
	  (memq (first form) '(optional opt zero-or-one ? ??)))
      ad-do-it
    (let* ((wrap-grp-num (when (boundp 'rxx-next-grp-num) (incf rxx-next-grp-num)))
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
		  
      (setq ad-return-value (format "\\(?%d:%s\\)" wrap-grp-num ad-return-value))
      (progn
	(do-rxx-env grp-name rxx-infos rxx-env
	  (rxx-dbg grp-name rxx-infos)
	  (let ((new-parser
		 `(lambda (match-str)
		    (let ((rxx-prefix ,rxx-prefix))
		      (rxx-dbg match-str)
		      (let ((repeat-form '(seq)) repeat-grp-names parse-result )
			(while (not parse-result)
			  (when (and repeat-grp-names ,have-sep-by)
			    (rxx-push-end (quote ,sep-by-form) repeat-form))
			  (let ((new-grp-name (make-symbol "new-grp")))
			    (rxx-push-end new-grp-name repeat-grp-names)
			    (rxx-push-end (list 'named-grp new-grp-name
						(quote (seq ,@(cdr form-sans-sep-by)))) repeat-form)
			    (rxx-dbg repeat-form new-grp-name repeat-grp-names)
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
  (unless (or (not (boundp 'rxx-env))
	      (and (boundp 'rxx-recurs-depth) (> rxx-recurs-depth 0))
	      (<= (length form) 2))
    (setq form (remove-if (lambda (elem) (or (eq (car-safe elem) 'recurse)
					     (and (eq (car-safe elem) 'named-grp)
						  (eq (car-safe (car-safe (cdr-safe (cdr-safe elem)))) 'recurse)))) form)))
  ad-do-it
  (when (boundp 'rxx-env) (not (rx-atomic-p ad-return-value))
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
  `(defrxxconst rxx-prefix (when (quote ,prefix) (symbol-name (quote ,prefix)))))

(defmacro defrxxconst (symbol initvalue &optional docstring)
  `(eval-and-compile
     (defconst ,symbol ,initvalue ,docstring)))

(defmacro defrxxcustom (symbol initvalue docstring &rest args)
  `(eval-and-compile
     (defcustom ,symbol ,initvalue ,docstring ,@args)))

(defmacro defrxx (var regexp &optional parser descr)
  `(defrxxconst ,(rxx-symbol var) (rxx ,regexp ,parser ,descr) ,descr))

(defmacro defrxxrecurse (depth var regexp &optional parser descr)
  `(defrxxconst ,var ,(let ((rxx-recurs-depth depth))
			(rxx-to-string regexp parser descr)) ,descr))


(defun rxx-add-font-lock-keywords ()
  (when (featurep 'font-lock)
    (put 'defrxxconst 'doc-string-elt 3)
    (put 'defrxx 'doc-string-elt 4)
    (put 'defrxxrecurse 'doc-string-elt 5)
    (font-lock-add-keywords
     nil
     `((,(rx (seq bow (group (or "defrxx" "defrxxconst" "defrxxcustom")) (1+ blank) (group (1+ (not space))))) .
	((1 font-lock-keyword-face) (2 font-lock-variable-name-face)))
       (,(rx (seq bow (group "defrxxrecurse") (1+ blank) (1+ digit) (1+ blank) (group (1+ (not space))))) .
	((1 font-lock-keyword-face) (2 font-lock-variable-name-face)))))))
  
(add-hook 'emacs-lisp-mode-hook 'rxx-add-font-lock-keywords)


(provide 'rxx)



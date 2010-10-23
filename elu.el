;;; elu.el --- Emacs Lisp Utilities: general-purpose elisp utils
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://ilya.cc/elu
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
;; General-purpose emacs utilities.  Also includes some functions present
;; in GNU Emacs but not XEmacs; putting them into the elu namespace
;; helps write portable code.
;;
;; See also: `elu-test.el'.

(defun elu-remove-if (predicate seq)
  "Return a new sequence containing elements of SEQ that do not satisfy
PREDICATE."
  (let (result)
    (dolist (e seq)
      (unless (funcall predicate e)
	(push e result)))
    (nreverse result)))

(defmacro elu-with-no-warnings (&rest forms)
  "Like progn, but with no warnings"
  (cons (if (featurep 'xemacs) 'progn 'with-no-warnings) forms))


(defun elu-make-seq (x)
  "Return X if X is a list, otherwise return (list X).
Used for iterating over arguments that can be a list or a singleton value."
  (if (listp x) x (list x)))

(defmacro elu-with-new-symbols (symbols &rest forms)
  "Bind each symbol in SYMBOLS to a newly created uninterned
symbol, and execute FORMS.  Useful for defining temp vars used in
macros. "
  (declare (indent 1))
  (append (list 'let (mapcar (lambda (symbol)
			      (list symbol `(make-symbol ,(symbol-name symbol))))
			    (elu-make-seq symbols)))
	  forms))

;; also make a version of elu-once, but one that works well with elisp. 

(defun elu-every (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...).
Taken from `every'."
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-every
	(apply 'map nil
	       (function (lambda (&rest cl-x)
			   (or (apply cl-pred cl-x) (throw 'cl-every nil))))
	       cl-seq cl-rest) t)
    (while (and cl-seq (funcall cl-pred (car cl-seq)))
      (setq cl-seq (cdr cl-seq)))
    (null cl-seq)))

(defvar *elu-gensym-counter* 0
  "Number suffix to append to variable names generated
by `elu-gensym'.")

;;;###autoload
(defun elu-gensym (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\".
Taken from `gensym'."
  (let ((pfix (if (stringp prefix) prefix "G"))
	(num (if (integerp prefix) prefix
	       (prog1 *elu-gensym-counter*
		 (setq *elu-gensym-counter* (1+ *elu-gensym-counter*))))))
    (make-symbol (format "%s%d" pfix num))))

(defmacro elu-with-gensyms (symbols &rest body)
  "Execute BODY in a context where the variables in SYMBOLS are bound to
fresh gensyms.  Adapted from URL http://www.emacswiki.org/emacs/macro-utils.el ."
  (assert (elu-every 'symbolp symbols))
  `(let ,(mapcar
	  (lambda (symbol)
	    (list symbol `(elu-gensym (symbol-name (quote ,symbol)))))
	  symbols)
    ,@body))

(defmacro elu-once-only (symbols &rest body)
  "Execute BODY in a context where the values bound to the variables in
SYMBOLS are bound to fresh gensyms, and the variables in SYMBOLS are bound
to the corresponding gensym.
Adapted from URL http://www.emacswiki.org/emacs/macro-utils.el ."
  (setq symbols (elu-make-seq symbols))
  (assert (elu-every #'symbolp symbols))
  (let ((gensyms (mapcar (lambda (x) (elu-gensym x)) symbols)))
    `(elu-with-gensyms ,gensyms
       (list 'let (elu-mapcar* #'list (list ,@gensyms) (list ,@symbols))
        ,(elu-list* 'let (elu-mapcar* #'list symbols gensyms)
           body)))))

(defun elu-list* (arg &rest rest)   ; See compiler macro in cl-macs.el
  "Return a new list with specified ARGs as elements, consed to last ARG.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'.
\n(fn ARG...)
Taken from `list*'."
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))


(defmacro elu-with (struct-type struct fields  &rest body)
  "Locally bind fields FIELDS of structure STRUCT of type STRUCT-TYPE (defined by `defstruct') for easy access.
FIELDS is a list of fields; each field is aliased to a local variable of the same name, then BODY forms are executed.
Setting these local variables will set the corresponding fields of STRUCT.

For example, if you had
\(defstruct my-struct a b)
...
\(setq x (make-my-struct :a 1 :b 2))
then you could write
\(elu-with my-struct x (a b)
  (+ a b))

Similar to WITH construct in Pascal."
  (declare (indent 3))
  (elu-once-only struct
    `(symbol-macrolet
	   ,(mapcar (lambda (field)
		      (list field
			    (list (intern (concat (symbol-name (eval struct-type))
						  "-" (symbol-name field))) struct))) fields)
	 ,@body)))


(defmacro* elu-do-seq ((var i seq &optional result) &rest body)
  "Iterate over a sequence as in `dolist', but giving access to the index of each item.
The variable passed as I will get assigned the index of the current item on each iteration.
Also, SEQ will be passed through `elu-make-seq'."
  `(let ((,i 0))
     (dolist (,var (elu-make-seq ,seq))
       (list ,@body (incf ,i)))))

(defun elu-trim-whitespace (s)
  "Trim trailing and leading whitespace from string"
  (save-match-data
    (replace-regexp-in-string
     (rx (or (seq string-start (zero-or-more whitespace))
	     (seq (zero-or-more whitespace) string-end)))
     "" (if (symbolp s) (symbol-name s) s))))

(defun elu-full-match (re s)
  "Do a string match, but fail unless the regexp matches the full string"
  (and (string-match re s)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length s))))



(defmacro elu-flet (bindings &rest body)
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


;;; Support for `progv': copied from `cl' package to avoid runtime dependence on it.
(defvar elu-progv-save)
;;;###autoload
(defun elu-progv-before (syms values)
  "Taken from `progv-before'."
  (while syms
    (push (if (boundp (car syms))
		 (cons (car syms) (symbol-value (car syms)))
	       (car syms)) elu-progv-save)
    (if values
	(set (pop syms) (pop values))
      (makunbound (pop syms)))))

(defun elu-progv-after ()
  "Taken from `progv-after'."
  (while elu-progv-save
    (if (consp (car elu-progv-save))
	(set (car (car elu-progv-save)) (cdr (car elu-progv-save)))
      (makunbound (car elu-progv-save)))
    (pop elu-progv-save)))

(defmacro elu-progv (symbols values &rest body)
  "Bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each symbol in the first list is bound to the corresponding value in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time.
Taken from `progv'."
  (declare (indent 2))
  (list 'let '((elu-progv-save nil))
	(list 'unwind-protect
	      (list* 'progn (list 'elu-progv-before symbols values) body)
	      '(elu-progv-after))))


(defun elu-uniquify (lst &optional compare-fn)
  "Return a new list containing elements of list LST minus any duplicates.
Adapted from `org-uniquify'."
  (let (res)
    (mapc (lambda (x) (elu-add-to-list 'res x 'append compare-fn)) lst)
    res))

(defmacro elu-dbg (&rest exprs)
  "Print the values of exprs, so you can write e.g. (dbg a b) to print 'a=1 b=2'.
Returns the value of the last expression."
  `(let ((expr-vals (list ,@exprs)))
     (when nil (apply 'message
		      (append (list ,(mapconcat (lambda (expr)
						  (concat (format "%s" expr) "=%s "))
						exprs
						""))
		    expr-vals)))
     (car-safe (elu-with-no-warnings (last expr-vals)))))

;(defmacro elu-dbg (&rest exprs) (last exprs))

(defmacro elu-safe-val (x)
  "If X is bound then return the value of X, else return nil."
  `(when (boundp (quote ,x)) ,x))

(defun elu-ends-with (s end)
  "Test if S ends with END"
  (and (>= (length s) (length end)) (string= (substring s (- (length s) (length end))) end)))

(defun elu-assoc-val (key alist &optional error-message)
  "Looks up KEY in association list ALIST.  Unlike `assoc', returns the associated value rather than the associated pair.
Also, converts key to a symbol if it is a string.
If ERROR-MESSAGE is given, and the key is not in the list, throws an error with this message unless
ERROR-MESSAGE is the symbol `nil-ok', in which case just return nil.
"
  (let ((assoc-result (assoc (if (stringp key) (intern key) key) alist)))
    (if assoc-result (cdr assoc-result)
      (if (eq error-message 'nil-ok) nil
	(error
	 (list (if error-message error-message (format "Key %s not in alist %s" key alist))))))))

;;;;;;;;;;;;;;;;;;;;

(defun elu-groupby (z key-func)
  "Group items in a list by their key, using the specified key extractor.
Return an a-list mapping keys to items with that key. 
Adapted from Python's itertools.groupby(). "
  (setq z (copy-sequence z))
  (setq z (sort z (lambda (x y) (< (funcall key-func x) (funcall key-func y)))))
  (let (result)
    (dolist (x z)
      (let* ((k (funcall key-func x)))
	(when (or (null result) (not (equal k (car (first result)))))
	  (push (cons k nil) result))
	(push x (cdr (first result)))))
    (reverse result)))


(defmacro elu-make-vector (length init)
  "Make a vector, evaluating the INIT expression for each element rather than just once."
  (elu-with-new-symbols (i v)
    `(let ((,v (make-vector ,length nil)))
       (dotimes (,i ,length ,v) (aset ,v ,i ,init)))))

(defmacro elu-gen-vector (i n &rest forms)
  "Construct a vector of size N from the expression for its I'th element."
  (declare (indent 2))
  (elu-with-new-symbols (save-n result)
    `(let* ((,save-n ,n)
	    (,result (make-vector ,save-n nil)))
       (dotimes (,i ,save-n ,result)
	 (aset ,result ,i (progn ,@forms))))))

(defun elu-number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative).

Taken from `number-sequence' in GNU Emacs; present here for XEmacs
compatibility.
"
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))

(defun elu-mapcar-many (cl-func cl-seqs)
  "Copy  of `cl-mapcar-many', used by `elu-mapcar'."
  (if (cdr (cdr cl-seqs))
      (let* ((cl-res nil)
	     (cl-n (apply 'min (mapcar 'length cl-seqs)))
	     (cl-i 0)
	     (cl-args (copy-sequence cl-seqs))
	     cl-p1 cl-p2)
	(setq cl-seqs (copy-sequence cl-seqs))
	(while (< cl-i cl-n)
	  (setq cl-p1 cl-seqs cl-p2 cl-args)
	  (while cl-p1
	    (setcar cl-p2
		    (if (consp (car cl-p1))
			(prog1 (car (car cl-p1))
			  (setcar cl-p1 (cdr (car cl-p1))))
		      (aref (car cl-p1) cl-i)))
	    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)))
	  (push (apply cl-func cl-args) cl-res)
	  (setq cl-i (1+ cl-i)))
	(nreverse cl-res))
    (let ((cl-res nil)
	  (cl-x (car cl-seqs))
	  (cl-y (nth 1 cl-seqs)))
      (let ((cl-n (min (length cl-x) (length cl-y)))
	    (cl-i -1))
	(while (< (setq cl-i (1+ cl-i)) cl-n)
	  (push (funcall cl-func
			    (if (consp cl-x) (pop cl-x) (aref cl-x cl-i))
			    (if (consp cl-y) (pop cl-y) (aref cl-y cl-i)))
		   cl-res)))
      (nreverse cl-res))))

(defun elu-mapcar* (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)

Copied from `mapcar*'.
"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (elu-mapcar-many cl-func (cons cl-x cl-rest))
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x)))

(defun elu-map-vectors (function &rest vecs)
  "Map a function over one or more vectors, and return the result as a vector."
  (apply 'vector (apply 'elu-mapcar* (cons function vecs))))

(defmacro elu-set-fields (struct-type struct &rest clauses)
  "Set multiple fields of a structure"
  (let ((my-struct (make-symbol "my-struct")))
    (let (result)
      (while clauses
	(push (list 'setf (list (intern (concat (symbol-name (eval struct-type)) "-"
						(substring (symbol-name (first clauses)) 1))) my-struct)
		    (second clauses)) result)
	(setq clauses (cddr clauses)))
      (append (list 'let (list (list my-struct struct)))
	      (nreverse result) (list my-struct)))))

(defmacro elu-modified-struct (struct-type struct &rest clauses)
  "Return a copy of the given structure STRUCT of type STRUCT-TYPE, with specified fields given new values and the
remaining fields taking values from STRUCT.   CLAUSES has the form :field1 val1 :field2 val2 ..."
  (declare (indent 2))
  `(elu-with ,struct-type ,struct
		     ,(delq nil
			    (mapcar
			     (lambda (clause)
			       (when (keywordp clause) (intern (substring (symbol-name clause) 1)))) clauses))
     (elu-set-fields ,struct-type (,(intern (concat "copy-" (symbol-name (eval struct-type)))) ,struct) ,@clauses)))


(defun elu-not-blank (s)
  "Return nil if S is nil or a blank string, else return S."
  (when (and s (save-match-data (string-match "[^ \t]" s)))
    s))


(defun elu-add-to-list (list-var element &optional append compare-fn)
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

Taken from `add-to-list'.  Reimplemented here because XEmacs' version
does not have the COMPARE-FN parameter.
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

(defmacro elu-push-end (elt lst)
  "Push value ELT onto end of list LST, and return the ELT.
Notice that this returns the new element, not the new list.
Notice also that this is a macro that modifies the actual variable
passed as LST.  See also `push'."
  `(progn
     (setq ,lst (append ,lst (list ,elt)))
     ,elt))

(defmacro elu-require (&rest modules)
  "Shorthand for requiring many modules in one command"
  (cons 'progn
	(mapcar (lambda (module) `(require (quote ,module)))
		modules)))

(provide 'elu)

;;; elu.el ends here


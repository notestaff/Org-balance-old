(setq load-path (cons "/cvar/sabeti-dav/ilya/sweep2ilya/bkp/fromscr1/ilya/usr3/share/emacs/site-lisp" load-path))

(require 'cl)
(require 'org)
(require 'org-clock)
(require 'org-agenda)

(defun run-tests ()
  (message "org-version=%s" org-version)
  (dolist (f '(elu rxx org-valu org-balance))
    (message "compiling %s" f)
    (byte-compile-file (concat (symbol-name f) ".el") 'load))
  (org-balance-regtests))


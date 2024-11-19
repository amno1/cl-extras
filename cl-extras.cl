;;; cl-extras.el --- Few sinple utils I haven't seen elsewhere -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; * lex         - like let* but uses plist-like syntax of pairs
;; * lex-if      - like if-let*   (uses lex under the hood)
;; * lex-when    - like when-let* (uses lex under the hood)
;; * rtimes      - like dotimes, but only repeat count
;; * while       _ simple while-loop as in elisp
;; * while*      - multiple condition while loop with named conditions
;; * named-let   - recursively called let with named conditions
;; * import-from - shadowing-import via slightly more convenient syntax
;; * defun       - declare elisp-style ignored lambda-list arguments

;;; Code:

(uiop:define-package "CL-EXTRAS"
  (:mix :cl)
  (:nicknames :ext)
  (:import-from :trivial-package-locks #:without-package-locks)
  (:export #:import-from))

(in-package :ext)

(defun lex--lambda-list (lambda-list name)
  (unless (evenp name)
    (signal 'wrong-number-of-arguments (list name (length lambda-list))))
  `(loop for x in ,lambda-list
        collect (list (pop ,lambda-list) (pop ,lambda-list)) into env
        end (nreverse env)))

(defmacro lex (varlist &rest body)
  "Bind variables according to VARLIST and then eval BODY.

VARLIST must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
  ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

Expands to `let*'."
  `(let* ,(lex--lambda-list varlist 'lex)
     ,@body))

(defmacro lex-if (varlist then-form &rest else-forms)
  "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORMS. ELSE-FORMS defaults to NIL.

BINDINGS must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
  ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
  (let* ((bindings (lex--lambda-list varlist 'lex-if))
         (variables (mapcar #'car bindings)))
    `(let* ,bindings
       (if (and ,@variables)
           ,then-form
           (progn ,@else-forms))))) ;; use progn so it works ootb with CL too

(defmacro lex-when (varlist &rest body)
  "Create new bindings according to VARLIST, and conditionally evaluate BODY.

BINDINGS must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
           ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
  (let* ((bindings (lex--lambda-list varlist 'lex-when))
         (variables (mapcar #'car bindings)))
        `(let* ,bindings
           (when (and ,@variables)
             ,@body))))


(defmacro rtimes (count &rest body)
  (let ((counter (gensym "rtimes-counter-"))
        (upper-bound (gensym "rtimes-bound-")))
    `(let ((,upper-bound ,count)
           (,counter 0))
       (while (< ,counter ,upper-bound)
         (progn ,@body)
         (setq ,counter (1+ ,counter))))))

(defmacro while (test &rest body)
  "If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.

The value of a `while' form is always nil."
  `(do () ((not ,test) nil) ,@body))

(defmacro lex-while (spec &rest body)
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all bindings are non-nil, eval BODY and repeat. Always returns nil.

This is similar to while-let from EmacsLisp but not the same. Let-form
uses 'lex' version of lambda-list, and does not allow for conditions
without names."
  (let ((bindings (lex--lambda-list spec 'while*)))
    `(tagbody
      repeat
        (let* ,bindings
          (if (and ,@(mapcar #'car bindings))
              (progn
                ,@body
                (go repeat))
              (go done)))
      done
      nil)))

;; adapted from elisp - subr-x.el
(defmacro named-let (name bindings &rest body)
  "Looping construct taken from Scheme.
Like `let', bind variables in BINDINGS and then evaluate BODY,
but with the twist that BODY can evaluate itself recursively by
calling NAME, where the arguments passed to NAME are used
as the new values of the bound variables in the recursive invocation.

Implementation after D. Hoyte from \"Let over lambda\"."
  (let ((fargs (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
        (aargs (mapcar (lambda (b) (if (consp b) (cadr b))) bindings)))
    `(labels ((,name ,fargs ,@body))
       (,name ,@aargs))))

(defun import-from (library &rest symbols)
  (let (imports notfound)
    (dolist (symbol symbols)
      (let ((import (find-symbol (symbol-name symbol) library)))
        (if import
            (push import imports)
            (push symbol notfound))))
    (when notfound
      (warn "Symbols: ~{~a~^ ~}~%are not found in ~a library"
            notfound library))
    (shadowing-import imports *package*)))

;; Emacs-style ignored arguments: ignore arguments starting with underscore
;; Like elisp, it also allows for multiple arguments with the same name
;; but only if the name is a single underscore character
;; This violates CL standard in a way that it allows one or more arguments
;; with the same name, a single underscore.

(without-package-locks
  (defmacro defun (name lambda-list &body body)
    "Like CL-DEFUN but allow underscore as indicator for ignored arguments."
    (let (arglist ignored)
      (dolist (arg lambda-list)
        (let ((name (symbol-name arg)))
          (cond
            ((char= #\_ (aref name 0))
             (when (= 1 (length name))
               (setf arg (gensym name)))
             (push arg arglist)
             (push arg ignored))
            (t (push name arglist)))))
      `(defun ,name ,(nreverse arglist)
         (declare (ignore ,@ignored))
         ,@body))))

(provide 'cl-extras)
;;; lex-bindings.el ends here

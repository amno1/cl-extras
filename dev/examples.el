;; -*- lexical-binding: t; eval: (font-lock-add-keywords nil '(("defexamples\\|def-example-group\\| => " (0 'font-lock-keyword-face)))); -*-

;; Only the first three examples per function are shown in the docs,
;; so make those good.
;;; Code:

(require 'ert)
(require 'lex-bindings)
;; Above emacs 24.1 assoc is core C.
(when (or (and (= emacs-major-version 24) (<= emacs-minor-version 2))
          (< emacs-major-version 24))
  (require 'assoc))

(def-example-group "lex"
    (defexamples lex
        (lex (x 1
              y 2
              z 3)
          (list x y z)) => (list 1 2 3)
        (lex (x nil
              y nil)
          (cons x y)) => (cons nil nil)
        (lex (x 1
              y (1+ x)
              z (1+ y))
          (list x y z)) => (list 1 2 3))

  (defexamples lex-if
      (lex-if (x t)
        1
        2) => 1

      (lex-if (x nil)
        1
        2) => 2

      (lex-if (x 1
               y (1+ x)
               z (< x y))
        y
        (setq x (1+ y))
        x) => 2

      (lex-if (x 1
               y (1+ x)
               z (> x y))
        y
        (setq x (1+ y))
        x) => 3)

  (defexamples lex-when

      (lex-when (x t)
        t) => t

      (lex-when (x nil)
        t) => nil

      (lex-when (x t
                 y t
                 z (and x y))
        z) => t)

  (defexamples lex-while

      (lex-while (x 10
                  run t)
        (setq x (1- x))
        (when (< x 5)
          (setq run nil))) => nil

      (let (new)
        (lex-while (l '(1 2 3))
          (push (pop l) new))
        new) => (list 3 2 1)

      (let ((s "no-run"))
        (lex-while (x (> 1 2))
          (setq s "I run!"))
        s) => "no-run"))

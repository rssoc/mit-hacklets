;;; undefined-variables.scm - Signal a warning when a block of
;;; evaluated code contains an unbound variable. Great for catching
;;; typos!
;;;
;;; DANGER LEVEL: None
;;; TESTED ON: Release 12.1 || SF || LIAR/x86-64
;;;
;;; (define (oops x)
;;;   (let ((x 3))
;;;     y
;;;     (let ((y 3))
;;;       (list x y z))))
;;; ;Warning:  y is undefined in (y (let ((y 3)) (list x y z)))
;;; ;Warning:  z is undefined in (list x y z)
;;; ;Value: oops
;;;
;;; NOTE: In Common Lisp, I would approach this by doing a single
;;; MACROLET then a MACROEXPAND-1. That would be the real "lispy" way
;;; of doing it, but unfortunately, mit-scheme has no MACROEXPAND-1
;;; equivalent. Had to do more work on my end :-(



(define (environment-all-bound-names environment)
  (let ((bound-names
         (append
          (environment-bound-names environment)
          (environment-macro-names environment))))
    (if environment
        (append
         bound-names
         (environment-all-bound-names
          (environment-parent environment)))
        bound-names)))



(define ((let/parse expr) bind)
  (bind (cadr expr) (cddr expr)))

(define ((lambda/parse expr) bind)
  (call-with-values
      (lambda () (parse-mit-lambda-list (cadr expr)))
    (lambda (required optional rest)
      (bind (append (or required '())
                    (or optional '())
                    (or rest '()))
            (cddr expr)))))

(define ((define/parse expr) bind)
  (bind (if (symbol? (cadr expr))
            (list (cadr expr))
            (call-with-values
                (lambda () (parse-mit-lambda-list (cadr expr)))
              (lambda (required optional rest)
                (append (or required '())
                        (or optional '())
                        (or rest '())))))
        (cddr expr)))

(define ((quote/parse expr) bind)
  (bind (cadr expr)))



(define (bind-and-descend expr bindings block)
  (cond
   ((list? expr)
    (case (car expr)
      ((let)
       ((let/parse expr)
        (lambda (new-bindings body)
          (bind-and-descend
           body (append (map car new-bindings) bindings)
           expr))))
      ((lambda)
       ((lambda/parse expr)
        (lambda (new-bindings body)
          (bind-and-descend
           body (append new-bindings bindings)
           expr))))
      ((define)
       ((define/parse expr)
        (lambda (new-bindings body)
          (bind-and-descend
           body (append new-bindings bindings)
           expr))))
      ((quote)
       ((quote/parse expr)
        (lambda (data) '())))
      (else
       (concatenate
        (map (lambda (e)
               (bind-and-descend e bindings expr))
             expr)))))
   ((symbol? expr)
    (if (member expr bindings)
        '()
        (list (cons expr block))))
   ((not (pair? expr))
    '())))



(define (warn-if-undefined expr env)
  (for-each
   (lambda (variable/block)
     (warn (car variable/block)
           'is 'undefined
           'in
           (cdr variable/block)))
   (bind-and-descend
    (unsyntax (syntax expr env))
    (environment-all-bound-names env)
    'top-level)))



(eval
 `(define hook/repl-eval
    (let ((hook/repl-eval hook/repl-eval))
      (lambda (expr env repl)
        (,warn-if-undefined expr env)
        (hook/repl-eval expr env repl))))
 (->environment '(runtime rep)))

;;; transparent-multiple-values.scm -- The nice fallback behavior of
;;; Common Lisp's multiple-values, with the allocation of MIT-Scheme.
;;;
;;; DANGER LEVEL: None
;;; TESTED ON: Release 12.1 || SF || LIAR/x86-64
;;;
;;; The semantics of Common Lisps multiple-values are interesting. If
;;; a function does not accept multiple-values, the other values are
;;; discarded:
;;;
;;; Example: (car (values (cons 'a 'b) 3)) => a
;;;
;;; Further, Common Lisp's multiple-values do not allocate anything,
;;; instead multiple-values are specially handled during compilation.
;;;
;;; While this is nice for some performance hacks, it can get in the
;;; way of other (arguably more interesting) hacks. Thankfully,
;;; MIT-Scheme allocates a #[multi-values] object! With this, we are
;;; very close to having (in my mind) a better multiple-value system
;;; than Common Lisp. This hack adds the missing "transparency" piece.
;;;
;;; One idea is abusing multiple-values as a transparent tagging
;;; mechanism. If you have a function, and want it to return some
;;; additional data without disrupting functions down the line, simply
;;; return multiple-values, with the first value preserving the
;;; original return type. The downstream will have no idea that the
;;; function's true return type has changed!
;;;
;;; TODO: This hack should be taken further. In fact, it'd be even
;;; more interesting if we could;
;;;
;;; (car (values (cons 'a 'b) 3)) => (values a 3)
;;;
;;; In fact, this is starting to remind me of The Sussman's layering
;;; idea, just without any sort of special CPS, Hmmmm... Perhaps this
;;; should be entertained further.



(bind-default-condition-handler
 (list condition-type:wrong-type-datum)
 (lambda (c)
   (let ((multi-values?
          (eval 'multi-values?
                (->environment '(runtime boot-definitions)))))
     (let ((datum (access-condition c 'datum)))
       (when (multi-values? datum)
         (use-value
          (call-with-values
              (lambda () (access-condition c 'datum))
            (lambda (first . _)
              first))))))))

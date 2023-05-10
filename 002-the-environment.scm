;;; 002-the-environment.scm -- Allow THE-ENVIRONMENT to be called from
;;; anywhere, not just the top level.
;;;
;;; DANGER LEVEL: Medium
;;; TESTED ON: Release 12.1 || SF || LIAR/x86-64
;;;
;;; Being able to grab the runtime environment anywhere in your
;;; program is incredibly useful. In fact, you used to be able to in
;;; older MIT-Scheme versions; I believe it was after some LIAR
;;; reworking that the maintainers decided to restrict THE-ENVIRONMENT
;;; calls to the top-level. I'm not a hundred percent sure why.
;;;
;;; Most of the time, it works as expected, however, there are
;;; instances when you might be surprised by the environment returned
;;; (particularly when the call to THE-ENVIRONMENT occurs inside of a
;;; macro).
;;;
;;; TODO: At the least, we should signal a warning when the call does
;;; not occur at the top-level.



(eval
 '(define $the-environment
    (spar-classifier->runtime
     (delay
       (spar-and
        (spar-subform)
        (spar-match-null)
        (spar-push-value the-environment-item
                         spar-arg:ctx)))))
 (->environment '(runtime syntax mit)))

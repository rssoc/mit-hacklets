;;; first-class-macros.scm -- First-class macros to MIT-Scheme.
;;;
;;; DANGER LEVEL: High (probably)
;;; TESTED ON: Release 12.1 || SF || LIAR/x86-64
;;;
;;; Yup! You read that right!
;;;
;;; (apply and '(#t #t #f #t)) => #f
;;; (apply if '(#f 'true-branch 'false-branch)) => false-branch
;;;
;;; And it's no-where near as difficult as it'd ought to be. In fact,
;;; it was so easy, that I'm just going to assume something /is/
;;; broken, despite not having any evidence. I'm sure the syntactic
;;; environments get all wonky with this.
;;;
;;; TODO: We should make this a hybrid system (somehow), we're sacrificing
;;; compile-time guarantees even when we don't need first-classness!



;; This is the the-environment-anywhere.scm hack.
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



(eval
 '(begin
    (define (compose f g)
      (lambda (x)
        (f (g x))))

    (define (make-safe-procedure proc guard? #!optional alt-proc)
      (lambda args
        (when (default-object? alt-proc)
          (set! alt-proc (lambda (x) x)))
        (if (apply guard? args)
            (apply proc args)
            (apply alt-proc args))))

    (define (environment-rssq obj environment)
      (let ((safe-car (make-safe-procedure car pair?))
            (safe-cdr (make-safe-procedure cdr pair?)))
        (let ((safe-cadr (compose safe-car safe-cdr)))
          (let ((rssq (association-procedure eqv? safe-cadr)))
            (or (safe-car (rssq obj (environment-bindings environment)))
                (and environment (environment-rssq obj (environment-parent environment))))))))

    (define env->senv
      runtime-environment->syntactic)

    (define (item-form item arguments)
      (cons (environment-rssq item (the-environment))
            arguments))

    (define ((item-partal-applicator item-impl) item)
      (lambda item-arguments
        (let* ((form (item-form item item-arguments))
               (environment (the-environment))
               (senv (env->senv environment))
               (hist (initial-hist form)))
          (let ((partial-item
                 (compile-item
                  (reclassify
                   ((item-impl item) form senv hist)
                   senv hist))))
            (eval
             (if (equal? item-impl classifier-item-impl)
                 (compile-item partial-item)
                 partial-item)
             environment)))))

    (define-item-compiler transformer-item?
      (item-partal-applicator transformer-item-impl))
    (define-item-compiler classifier-item?
      (item-partal-applicator classifier-item-impl)))
 (->environment '(runtime syntax top-level)))

;;; 003-slime.el -- Elisp code to make Slime connect to MIT-Scheme's
;;; Swank again!
;;;
;;; DANGER LEVEL: None
;;; TESTED ON: GNU Emacs 30.0.50
;;; TESTED ON: SLIME v2.27
;;; TESTED ON: Release 12.1 || SF || LIAR/x86-64
;;;
;;; Few people know that MIT-Scheme comes with a Swank server,
;;; probably because it's broken out of the box... Turns out it was a
;;; very minor issue with symbols being qualified differently from
;;; Slime's side. A tiny shim fixes the issue.
;;;
;;; Add the Elisp code below to your Emacs configuration, then start
;;; Slime with a negative-prefix ``M-- M-x slime'', select
;;; "mit-scheme", hit ``yes'' on the prompt, and voila! It works
;;; surprisingly well.
;;;
;;; NOTE: I don't use this hack myself, I like Emacs built-in
;;; MIT-Scheme interaction mode (``xscheme-mode'') more.



(eval-after-load 'slime
  '(progn
     (defun mit-scheme-start-swank (file encoding)
       (format
        "%S\n\n"
        `(begin
          (eval `(begin
                  (define (swank-shim sexp)
                          (cons (intern
                                 (string-concatenate
                                  (cons "swank:"
                                        (cdr ((string-splitter
                                               'delimiter
                                               (read-from-string "#\\:"))
                                              (symbol->string (car sexp)))))))
                                (cdr sexp)))
                  (define emacs-rex
                          (let ((shim emacs-rex))
                            (lambda (socket sexp pstring id)
                              (shim socket (swank-shim sexp) pstring id))))
                  (define-message-handler
                   '(':emacs-rex form datum datum index)
                   (lambda (socket level sexp pstring thread id)
                     thread
                     (call-with-current-continuation
                      (lambda (k)
                        (bind-condition-handler
                         (list condition-type:serious-condition)
                         (lambda (condition)
                           (dynamic-wind
                            (lambda () (read-from-string "#f"))
                            (lambda () (invoke-sldb socket (+ level 1) condition))
                            (lambda ()
                              (write-message
                               `(:return (:abort ,(condition/report-string condition))
                                         ,id)
                               socket)
                              (k unspecific))))
                         (lambda ()
                           (pp sexp (notification-output-port))
                           (let ((result `(:return (:ok ,(emacs-rex socket sexp pstring id)) ,id)))
                             (pp result (notification-output-port))
                             (newline (notification-output-port))
                             (write-message result socket))))))))
                  (define swank:init-presentations (lambda _ '())))
                (->environment '(runtime swank)))
          (start-swank ,file))))

     (defun mit-scheme-find-buffer-package ()
       (save-excursion
         (let ((case-fold-search t))
           (goto-char (point-min))
           (and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
                (match-string-no-properties 1)))))

     (defun mit-scheme-slime-mode-init ()
       (slime-mode t)
       (make-local-variable 'slime-find-buffer-package-function)
       (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

     (if (not (memq 'mit-scheme slime-lisp-implementations))
         (setq slime-lisp-implementations
               (cons '(mit-scheme ("mit-scheme")
                                  :init mit-scheme-start-swank)
                     slime-lisp-implementations)))

     (mit-scheme-slime-mode-init)))

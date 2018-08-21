
(define (vaquero-eval code env)
   (define compile-env
      (vaquero-environment env))
   (define prog
      (vaquero-compile (vaquero-expand code compile-env)))
   (prog env top-cont top-err))

(define (vaquero-apply obj xs opts cont err)
   (define (apply-or-die)
      (vaquero-send obj 'apply
         (lambda (af)
            (vaquero-apply af (list xs opts) 'null cont err))
           err))
   (cond
      ((procedure? obj)
         (cont
            (handle-exceptions exn
               (err
                  (handle-exceptions exn
                     (err "Something went seriously wrong in the interpreter." identity)
                     (list
                        'exn
                        (list 'location  ((condition-property-accessor 'exn 'location) exn))
                        (list 'arguments ((condition-property-accessor 'exn 'arguments) exn))
                        (list 'message   ((condition-property-accessor 'exn 'message) exn))))
                  (lambda (ys) (cont (apply obj ys))))
               (apply obj xs))))
      ((hash-table? obj)
         (let ((type (htr obj 'type)))
            (if (or (eq? type 'lambda) (eq? type 'op) (eq? type 'proc))
               ((htr obj 'exec) xs opts cont err)
               (apply-or-die))))
      (else (apply-or-die))))

(define (vaquero-apply-wrapper obj)
   (lambda xs
      (vaquero-apply obj xs 'null top-cont top-err)))


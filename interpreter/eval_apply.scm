
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
            (vaquero-apply af (list xs) opts cont err))
           err))
   (cond
      ((procedure? obj)
         (cont
            (handle-exceptions exn
               (err
                  (handle-exceptions exn
                     (err "Something went seriously wrong in the interpreter." identity)
                     (condition->list exn))
                  (lambda (ys) (cont (apply obj ys))))
               (apply obj xs))))
      ((vaquero-proc? obj)
         ((vaquero-proc-exec obj) xs opts cont err))
      (else (apply-or-die))))

(define (vaquero-apply-wrapper obj)
   (lambda xs
      (vaquero-apply obj xs 'null top-cont top-err)))



(define-record-type vaq-obj
   (vaquero-udf fields autos forwards default)
   vaquero-object?
   (fields vaquero-obj-fields)
   (autos vaquero-obj-autos)
   (forwards vaquero-obj-forwards)
   (default vaquero-obj-default vaquero-obj-set-default!))

(define (vaquero-object args autos forwards initial)
   (define fields (mkht))
   (define delegates (mkht))
   (define autoexec (mkht))
   (define (fset! k v)
      (hts! fields k v))
   (define (aset! k)
      (hts! autoexec k #t))
   (define (rset! k v)
      (hts! delegates k v))
   (define (set-forward! rlist)
      (let ((delegate (car rlist)) (msgs (cdr rlist)))
         (map (lambda (msg) (rset! msg delegate)) msgs)))
   (for-pairs fset! args)
   (if forwards
      (map set-forward! forwards))
   (if autos
      (map aset! autos))
   (define this
      (vaquero-udf fields autoexec delegates the-default))
   (define the-default
      (or initial
         (vaquero-proc
            primitive-type
            'object
            (lambda (args opts cont err)
               (idk this (car args) cont err)))))
   (vaquero-obj-set-default! this the-default)
   this)


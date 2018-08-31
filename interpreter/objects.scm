
(define-record-type vaq-obj
   (vaquero-udf fields autos forwards default)
   vaquero-object?
   (fields vaquero-obj-fields)
   (autos vaquero-obj-autos)
   (forwards vaquero-obj-forwards)
   (default vaquero-obj-default))

(define (vaquero-object args autos resends initial)
   (define this (mkht))
   (define fields (mkht))
   (define delegates (mkht))
   (define autoexec (mkht))
   (define (tset! k v)
      (hts! this k v))
   (define (fset! k v)
      (hts! fields k v))
   (define (aset! k)
      (hts! autoexec k #t))
   (define (rset! k v)
      (hts! delegates k v))
   (define (set-resend! rlist)
      (let ((delegate (car rlist)) (msgs (cdr rlist)))
         (map (lambda (msg) (rset! msg delegate)) msgs)))
   (for-pairs fset! args)
   (if resends
      (map set-resend! resends)
      #f)
   (if autos
      (map aset! autos))
   (tset! 'type 'object)
   (tset! 'fields fields)
   (tset! 'autos autoexec)
   (tset! 'resends delegates)
   (tset! 'default
      (or initial
         (vaquero-proc
            primitive-type
            'object
            (lambda (args opts cont err)
               (idk this (car args) cont err)))))
   this)


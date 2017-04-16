
(define vaquero-modules (mkht))

(define (def-vaquero-module path)
    (define has? (hte? vaquero-modules path))
    (if has?
        (htr vaquero-modules path)
        (let ((expanded (read-expand-cache-prog path (local-env))))
            (define mod-id (uuid-v4))
            (define module (cons mod-id expanded))
            (hts! vaquero-modules path module)
            (find-modules expanded)
            module)))

(define (vaquero-module-id path)
   (car (htr vaquero-modules path)))

(define (vaquero-module-code path)
   (cdr (htr vaquero-modules path)))

(define (join-to-symbol xs j)
   (string->symbol (string-join xs j)))

(define (uuid-ify name id)
   (join-to-symbol (list name id) "-"))

(define (find-modules prog)
   (if (pair? prog)
      (case (car prog)
         ((quote)
            #t)
         ((use)
            (def-vaquero-module (caddr prog))
            #t)
         (else
            (map find-modules prog)))
      #t))

(define (transform-uses code)
   (if (pair? code)
      (let ((head (car code)))
         (case head
            ((use)
               (define pkg-name (cadr code))
               (define mod-id (vaquero-module-id (caddr code)))
               `(def ,pkg-name (,(uuid-ify "vaquero-internal-module-proc" mod-id))))
            (else (map transform-uses code))))
      code))

(define (package-module mod-id mod-env code)
   ; should be expanded by now, so the form should be (seq ...)
   (define proc-name   (uuid-ify "vaquero-internal-module-proc" mod-id))
   (define object-name (uuid-ify "vaquero-internal-module-object" mod-id))
   (define loop-name   (uuid-ify "loop-name" mod-id))
   (define expo        (uuid-ify "export"    mod-id))
   (define expos       (uuid-ify "exports"   mod-id))
   (define rval        (uuid-ify "rval"      mod-id))
   (define body (cdr code))
   `(proc ,proc-name ()
      (if ((send ,mod-env (quote has?)) (quote ,object-name))
         ((send ,mod-env (quote get)) (quote ,object-name))
         (seq
            ; begin body
            ,@body
            ; end body
            (def ,object-name
               (apply object (pair 'type (pair 'module
                  ((proc ,loop-name (,expo ,expos ,rval)
                     (def nu-rval (pair ,expo (pair ((send env 'lookup) ,expo) ,rval)))
                     (if ,expos
                        (,loop-name (send ,expos 'head) (send ,expos 'tail) nu-rval)
                        nu-rval)) (send vaquero-internal-exports 'head) (send vaquero-internal-exports 'tail) ()))) (table)))

            ((send ,mod-env (quote def!)) (quote ,object-name) ,object-name)

            ,object-name))))

(define (vaquero-link prog)
   (define sys-UUID        (uuid-ify "sys" (uuid-v4)))
   (define main-UUID       (uuid-ify "vaquero-internal-main-program" (uuid-v4)))
   (define module-env-UUID (uuid-ify "vaquero-internal-module-env" (uuid-v4)))
   (define ok              (find-modules prog))
   (define module-list     (hash-table-values vaquero-modules))
   (define mk-mod
      (lambda (m)
         (package-module (car m) module-env-UUID (transform-uses (cdr m)))))
   (define modules (map mk-mod module-list))
   (define main-program (transform-uses prog))
   `(lambda (,sys-UUID)
      (let ()
         (def ,module-env-UUID env)

         ,@modules

         (proc ,main-UUID (sys)
            ; begin main program
            ,main-program
            ; end main program
         )

         (,main-UUID ,sys-UUID))))


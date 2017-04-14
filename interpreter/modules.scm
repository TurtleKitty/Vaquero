
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

(define (modularize program)
   (define body
      (if (pair? program)
         (transform-uses program)
         'null))
   `(proc ))

(define (join-to-symbol xs j)
   (string->symbol (string-join xs j)))

(define (uuid-ify name id)
   (join-to-symbol (list name id) "-"))

(define (find-modules prog)
   (define (finder form rest)
      (define (finish)
         (if (pair? rest)
            (finder (car rest) (cdr rest))
            #t))
      (if (pair? form)
         (case (car form)
            ((quote)
               #t)
            ((use)
               (def-vaquero-module (caddr form))
               (finish))
            (else
               (finish)))
         (finish)))
   (finder (car prog) (cdr prog)))

(define (transform-uses code)
   (if (pair? code)
      (let ((head (car code)))
         (case head
            ((use)
               (define pkg-name (cadr code))
               (define mod-id (vaquero-module-id (caddr code)))
               `(def ,pkg-name (,(uuid-fy "vaquero-internal-module-proc" mod-id))))
            (else (map transform-uses code))))
      code))

(define (package-module mod-id mod-env code)
   ; should be expanded by now, so the form should be (seq ...)
   (define proc-name   (uuid-fy "vaquero-internal-module-proc" mod-id))
   (define object-name (uuid-fy "vaquero-internal-module-object" mod-id))
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
                  (loop go (expo vaquero-internal-exports.head expos vaquero-internal-exports.tail rval ())
                     (def nu-rval (pair expo (pair (env.lookup expo) rval)))
                     (if expos
                        (go expos.head expos.tail nu-rval)
                        nu-rval)))) (table)))

            ((send ,mod-env (quote def!)) (quote ,object-name) ,object-name)

            ,object-name))))

(define the-expanded-global-prelude #f)

(define (expand-global-prelude)
   (if the-expanded-global-prelude
      the-expanded-global-prelude
      (let ((expanded
               (vaquero-expand
                  (vaquero-read-file
                     (open-input-string global-prelude-text)))))
         (set! the-expanded-global-prelude expanded)
         expanded)))

(define (vaquero-link prog)
   (define sys-UUID        (uuid-fy "sys" (uuid-v4)))
   (define main-UUID       (uuid-fy "vaquero-internal-main-program" (uuid-v4)))
   (define module-env-UUID (uuid-fy "vaquero-internal-module-env" (uuid-v4)))
   (define the-prelude (cdr (expand-global-prelude)))
   (define ok (find-modules prog))
   (define module-list (hash-table-values vaquero-modules))
   (define mk-mod
      (lambda (m)
         (package-module (car m) module-env-UUID (transform-uses (cdr m)))))
   (define modules (map mk-mod module-list))
   (define main-program (transform-uses prog))

   `(lambda (,sys-UUID)
      ,@the-prelude

      (def global env)

      (let ()
         (def ,module-env-UUID env)

         ,@modules

         (proc ,main-UUID (sys)
            ; begin main program
            ,main-program
            ; end main program
         )

         (,main-UUID ,sys-UUID))))


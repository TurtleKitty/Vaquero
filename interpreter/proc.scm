
(define-record-type vaq-proc
   (vaquero-procedure type code env exec formals arity)
   vaquero-proc?
   (type vaquero-proc-type)
   (code vaquero-proc-code)
   (env vaquero-proc-env)
   (exec vaquero-proc-exec)
   (formals vaquero-proc-formals)
   (arity vaquero-proc-arity))

(define (vaquero-proc code env compiled)
   (define proc-type
      (if (pair? code)
         (let ((operator (car code)))
            (if (eq? 'op operator)
               '(op)
               (if (eq? 'proc operator)
                  '(proc lambda)
                  '(lambda))))))
   (define formals
      (if (pair? code)
         ((if (eq? (car code) 'op) caddr cadr) code)
         '()))
   (vaquero-procedure proc-type code env compiled formals (length formals)))

(define vaquero-send-primitive-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-primitive-vtable msg))))

      (method autos
         (cont '(arity autos code env formals messages to-bool to-text type view)))

      (method messages
         (cont (htks vaquero-send-primitive-vtable)))

      (method type
         (cont '(lambda)))

      (method view
         (cont primitive-code))

      (method to-bool
         (cont #t))

      (method to-text
         (cont "0xDEADBEEF"))

      (method arity
         (cont
            (let ((pinfo (procedure-information obj)))
               (if (list? pinfo)
                  (sub1 (length pinfo))
                  '*))))

      (method code
         (cont '0xDEADBEEF))

      (method env
         (cont 'global))

      (method formals
         (cont (procedure-information obj)))

      (method primitive-apply
         (cont
            (lambda (args . whatever)
               (apply obj args))))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,view)
           (arity      . ,arity)
           (code       . ,code)
           (env        . ,env)
           (formals    . ,formals)
           (apply      . ,primitive-apply)
           (default    . ,idk)))))

(define vaquero-send-proc-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-proc-vtable msg))))

      (method autos
         (cont '(arity autos code env formals messages to-bool to-text type view)))

      (method messages
         (cont (htks vaquero-send-proc-vtable)))

      (method to-bool
         (cont #t))

      (method to-text
         (cont (vaquero-proc-code obj)))

      (method type
         (cont (vaquero-proc-type obj)))

      (method view
         (cont `(,(car (vaquero-proc-type obj)) ,(vaquero-proc-formals obj) ...)))

      (method arity
         (cont (vaquero-proc-arity obj)))

      (method code
         (cont (vaquero-proc-code obj)))

      (method env
         (cont (vaquero-proc-env obj)))

      (method formals
         (cont (vaquero-proc-formals obj)))

      (method get-attr
         (cont (htr obj msg)))

      (method proc-apply
         (cont
            (vaquero-proc
               primitive-code
               'proc
               (lambda (args opts cont err)
                  (define min-args 1)
                  (define options
                     (if (hte? opts 'opt)
                        (htr opts 'opt)
                        (vaquero-table)))
                  (if (< (length args) min-args)
                     (err (vaquero-error-object 'arity `((send ,obj apply) ,args) "proc.apply requires arguments!") cont)
                     (vaquero-apply obj (car args) options cont err))))))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,view)
           (arity      . ,arity)
           (code       . ,code)
           (env        . ,env)
           (formals    . ,formals)
           (apply      . ,proc-apply)
           (default    . ,idk)))))


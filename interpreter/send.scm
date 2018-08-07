
(include "bool.scm")
(include "eof.scm")
(include "null.scm")
(include "number.scm")
(include "symbol.scm")
(include "text.scm")
(include "pair.scm")
(include "proc.scm")
(include "stream.scm")
(include "vector.scm")

(define vaquero-universal-messages '(answers? autos messages to-bool to-text type view))

(define (vaquero-send obj msg cont err)
   (define obj-type (vaquero-type obj))
   (define handler (htr vaquero-send-vtable obj-type))
   (handler obj msg cont err))

(define (vaquero-send-atomic obj msg)
    (vaquero-send obj msg top-cont top-err))

(define (vaquero-answerer msgs)
   (define msgs+ (append msgs vaquero-universal-messages))
   (lambda (msg)
      (if (member msg msgs+) #t #f)))

(define (vaquero-send-generic vtable)
   (lambda (obj msg cont err)
      (define m
         (if (hte? vtable msg)
            (htr vtable msg)
            (htr vtable 'default)))
      (m obj msg cont err)))

(define vaquero-send-null
   (vaquero-send-generic vaquero-send-null-vtable))

(define vaquero-send-bool
   (vaquero-send-generic vaquero-send-bool-vtable))

(define vaquero-send-symbol
   (vaquero-send-generic vaquero-send-symbol-vtable))

(define vaquero-send-int
   (vaquero-send-generic vaquero-send-int-vtable))

(define vaquero-send-real
   (vaquero-send-generic vaquero-send-real-vtable))

(define vaquero-send-text
   (vaquero-send-generic vaquero-send-text-vtable))

(define vaquero-send-list
   (vaquero-send-generic vaquero-send-list-vtable))

(define vaquero-send-pair
   (vaquero-send-generic vaquero-send-pair-vtable))

(define vaquero-send-primitive
   (vaquero-send-generic vaquero-send-primitive-vtable))

(define vaquero-send-proc
   (vaquero-send-generic vaquero-send-proc-vtable))

(define vaquero-send-vector
   (vaquero-send-generic vaquero-send-vector-vtable))

(define vaquero-send-source
   (vaquero-send-generic vaquero-send-source-vtable))

(define vaquero-send-sink
   (vaquero-send-generic vaquero-send-sink-vtable))

(define vaquero-send-EOF
   (vaquero-send-generic vaquero-send-EOF-vtable))

(define (vaquero-ho code obj cont err)
    (vaquero-apply
        (vaquero-compile-method code)
        (list obj)
        'null
        cont
        err))

(define (vaquero-send-table obj msg cont err)
    (define msgs
        '(view size clone to-bool get put set! rm del! has? apply keys values pairs to-list to-opt to-text merge fold reduce map filter))
    (define vars (htr obj 'vars))
    (define (rdefault msg)
        (if (hte? vars msg)
            (htr vars msg)
            'null))
    (case msg
        ((type view size autos clone to-bool get put set! rm del! has? apply keys values pairs to-list to-opt to-text merge messages answers?)
            (cont
                (case msg
                    ((type) 'table)
                    ((view to-text)
                         (vaquero-view obj))
                    ((size)
                         (hash-table-size vars))
                    ((autos)
                         '(view size clone to-bool to-list to-text keys values pairs))
                    ((clone)
                        (let ((noob (vaquero-table)))
                            (hts! noob 'vars (hash-table-copy vars))
                            noob))
                    ((to-bool)
                        (> (hash-table-size vars) 0))
                    ((get)
                        (lambda (k)
                            (if (hte? vars k)
                                (htr vars k)
                                'null)))
                    ((put)
                        (lambda args
                            (define noob (vaquero-table))
                            (hts! noob 'vars (hash-table-copy vars))
                            (vaquero-send-table
                                noob
                                'set!
                                (lambda (setter!)
                                    (apply setter! args)
                                    noob)
                                err)))
                    ((set!)
                        (lambda args
                            (for-pairs (lambda (k v) (hts! vars k v)) args)
                            'null))
                    ((rm)
                        (lambda args
                            (define noob (vaquero-table))
                            (hts! noob 'vars (hash-table-copy vars))
                            (vaquero-send-table
                                noob
                                'del!
                                (lambda (deleter!)
                                    (apply deleter! args)
                                    noob)
                                err)))
                    ((del!)
                        (lambda args
                            (map (lambda (k) (htd! vars k)) args)
                            'null))
                    ((has?)
                        (lambda (x)
                            (hte? vars x)))
                    ((apply)
                        (vaquero-proc
                            primitive-type
                            'table
                            (lambda (args opts cont err)
                                (vaquero-send-table obj (caar args) cont err))))
                    ((keys) (htks vars))
                    ((values) (htvs vars))
                    ((pairs to-list) (hash-table->alist vars))
                    ((to-opt)
                        (fold
                            (lambda (p xs)
                                (cons (symbol->keyword (car p)) (cons (cdr p) xs)))
                            '()
                            (hash-table->alist vars)))
                    ((messages) msgs)
                    ((answers?)
                        (lambda (msg)
                            (or 
                                (hte? vars msg)
                                ((vaquero-answerer msgs) msg))))
                    ((merge)
                        (lambda (other)
                            (define nuvars (hash-table-merge (htr other 'vars) vars))
                            (define noob (mkht))
                            (hts! noob 'type 'table)
                            (hts! noob 'vars nuvars)
                            noob)))))
            ((fold) (vaquero-send-list
                        (hash-table->alist vars)
                        'fold
                        cont
                        err))
            ((reduce) (vaquero-send-list
                        (hash-table->alist vars)
                        'reduce
                        cont
                        err))
            ((map)
                (vaquero-ho
                    '(lambda (rec)
                        (lambda (funk)
                            (def mapped (rec.to-list.map funk))
                            mapped.to-table))
                    obj
                    cont
                    err))
            ((filter) 
                (vaquero-ho
                    '(lambda (rec)
                        (lambda (funk)
                            (def mapped (rec.to-list.filter funk))
                            mapped.to-table))
                    obj
                    cont
                    err))
            (else (cont (rdefault msg)))))

(define (vaquero-send-object obj msg cont err)
   (define fields  (htr obj 'fields))
   (define resends (htr obj 'resends))
   (define autos   (htr obj 'autos))
   (define (get-msgs)
      (append (hash-table-keys fields) (hash-table-keys resends)))
   (define (vaquero-object-view obj)
      (define type
         (if (hte? fields 'type)
            (htr fields 'type)
            'object))
      (define the-view
         (if (hte? fields 'view)
            (let ((view-proc (htr fields 'view)))
               (if (member 'apply (vaquero-send-atomic view-proc 'messages))
                  (vaquero-apply view-proc '() 'null identity err)
                  (err (vaquero-error-object 'view-must-answer-apply `(send ,obj 'view) "The object answered 'view with a non-applicative."))))
            (get-msgs)))
      (apply vector (cons type the-view)))
   (case msg
      ; unshadowable reflection messages
      ((answers?)  (cont (vaquero-answerer (get-msgs))))
      ((messages)  (cont (get-msgs)))
      ((autos)     (cont (hash-table-keys autos)))
      ((view)      (cont (vaquero-object-view obj)))
      (else
         (if (hte? fields msg)
            (let ((v (htr fields msg)))
               (if (hte? autos msg)
                  (vaquero-apply v '() 'null cont err) ; exec the thunk
                  (cont v)))
            (if (hte? resends msg)
               (vaquero-send (htr resends msg) msg cont err)
               (case msg
                  ((type) (cont 'object))
                  ((to-text) (cont "object"))
                  ((to-bool) (cont (not (eq? 0 (length (hash-table-keys fields))))))
                  (else (vaquero-apply (htr obj 'default) (list msg) 'null cont err))))))))

(define (vaquero-send-env obj msg cont err)
    (define msgs '(view to-text def! merge! has? get pairs lookup parent extend expand eval load))
    (define undefineds (list not-found will-exist 'null))
    (define vars (htr obj 'vars))
    (define (env-default msg)
        (lookup obj msg
            (lambda (val)
               (if (eq? val not-found)
                  (err (vaquero-error-object 'message-not-understood `(send ,obj ,msg) "Message not understood.") cont)
                  val))
            err))
    (case msg
        ((get has? to-bool keys values pairs)
            (vaquero-send-table vars msg cont err))
        ((type) (cont 'env))
        ((view to-text)
            (vaquero-view obj))
        ((autos) (cont '(view to-text to-bool keys values pairs)))
        ((def!)
            (cont
                (vaquero-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (define getter (vaquero-send-table vars 'get  top-cont err))
                        (define setter (vaquero-send-table vars 'set! top-cont err))
                        (if (null? args)
                           (cont 'null)
                           (let loop ((def-name (car args)) (def-val (cadr args)) (the-rest (cddr args)))
                              (define current (getter def-name))
                              (if (member current undefineds)
                                 (begin
                                    (setter def-name def-val)
                                    (if (null? the-rest)
                                       (cont def-val)
                                       (loop (car the-rest) (cadr the-rest) (cddr the-rest))))
                                 (err (vaquero-error-object 'name-already-defined `(def ,def-name ,def-val) "env: name is already defined.") cont))))))))
        ((merge!)
            (cont
               (vaquero-proc
                  primitive-type
                  'env
                  (lambda (args opts cont err)
                     (define (arg-fail form)
                        (err (vaquero-error-object 'argument-fail form "env: merge! requires an environment as an argument.") cont))
                     (if (not (pair? args))
                        (arg-fail '(env.merge!))
                        (let ((other-env (car args)))
                           (if (not (eq? 'env (vaquero-send-atomic other-env 'type)))
                              (arg-fail '(env.merge! WAT))
                              (let ((def-this (vaquero-send-atomic obj 'def!))
                                    (other-vars (hash-table->alist (htr (htr other-env 'vars) 'vars))))
                                 (if (not (pair? other-vars))
                                    (cont obj)
                                    (let loop ((v (car other-vars)) (vs (cdr other-vars)) (flat '()))
                                       (define nu-flat (cons (car v) (cons (cdr v) flat)))
                                       (if (pair? vs)
                                          (loop (car vs) (cdr vs) nu-flat)
                                          (vaquero-apply def-this nu-flat 'null (lambda (x) (cont obj)) err))))))))))))
        ((lookup)
            (cont
                (vaquero-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (lookup
                            obj
                            (car args)
                            (lambda (val)
                                (cont
                                    (if (eq? val not-found)
                                        'null
                                        val)))
                            err)))))
        ((extend)
            (cont
                (vaquero-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (let loop ((names '()) (vals '()) (left args))
                            (if (eq? '() left)
                                (extend obj names vals cont err)
                                (loop (cons (car left) names) (cons (cadr left) vals) (cddr left))))))))
        ((parent)
            (cont (htr obj 'parent)))
        ((eval)
            (cont
                (lambda (code)
                    (vaquero-eval code obj))))
        ((expand)
            (cont
                (lambda (code)
                    (vaquero-expand code obj))))
        ((load)
            (cont
               (lambda (stream)
                   (vaquero-eval (vaquero-read-file stream) obj))))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        (else (cont (env-default msg)))))

(define (vaquero-send-wtf obj msg cont err)
   (err (vaquero-error-object 'wtf-was-that? `(send ,obj ,msg) "Unknown object!")))

(define vaquero-send-vtable
   (alist->hash-table
      `((null         . ,vaquero-send-null)
        (bool         . ,vaquero-send-bool)
        (symbol       . ,vaquero-send-symbol)
        (keyword      . ,vaquero-send-symbol)
        (int          . ,vaquero-send-int)
        (real         . ,vaquero-send-real)
        (text         . ,vaquero-send-text)
        (list         . ,vaquero-send-list)
        (pair         . ,vaquero-send-pair)
        (primitive    . ,vaquero-send-primitive)
        (vector       . ,vaquero-send-vector)
        (source       . ,vaquero-send-source)
        (sink         . ,vaquero-send-sink)
        (env          . ,vaquero-send-env)
        (table        . ,vaquero-send-table)
        (proc         . ,vaquero-send-proc)
        (object       . ,vaquero-send-object)
        (eof          . ,vaquero-send-EOF)
        (WTF          . ,vaquero-send-wtf))))


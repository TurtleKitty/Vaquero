
(define vaquero-send-vector-vtable
   (let ()
      (define (get-pairs obj)
         (vector->list
            (vector-map (lambda (i x) (cons i x)) obj)))

      (method answers?
         (cont (lambda (msg)
            (or
               (and (number? msg) (> (vector-length obj) msg))
               (hte? vaquero-send-vector-vtable msg)))))

      (method autos
         (cont '(autos messages to-bool type view to-list size pairs clone)))

      (method messages
         (cont (htks vaquero-send-vector-vtable)))

      (method to-bool
         (cont (not (eq? 0 (vector-length obj)))))

      (method to-text
         (cont (vaquero-view obj)))

      (method type
         (cont '(vector)))

      (method view
         (cont (vaquero-view obj)))

      (method to-list
         (cont (vector->list obj)))

      (method to-set
         (cont (apply make-vaquero-set (get-pairs obj))))

      (method to-table
         (cont (vaquero-send-atomic (get-pairs obj) 'to-table)))

      (method to-vector
         (cont obj))

      (method clone
         (cont (vector-copy obj)))

      (method pairs
         (cont (get-pairs obj)))

      (method size
         (cont (vector-length obj)))

      (method has?
         (cont
            (lambda (item)
               (if (vector-index (lambda (x) (eq? x item)) obj)
                  #t
                  #f))))

      (method v-append
         (cont (lambda (other) (vector-append obj other))))

      (define (check-idx obj msg cont err idx funk)
         (if (not (number? idx))
            (err (vaquero-error-object 'not-a-number `(,obj ,idx) (string-join `("vector:" ,msg "requires a number as its argument.") " ")) cont)
            (if (> idx (vector-length obj))
               (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont)
               (cont (funk)))))

      (method v-get
         (cont
            (vaquero-proc
               primitive-type
               'vector
               (lambda (args opts cont err)
                  (define idx (car args))
                  (check-idx obj "get" cont err idx
                     (lambda ()
                        (vector-ref obj idx)))))))

      (method v-set!
         (cont
            (vaquero-proc
               primitive-type
               'vector
               (lambda (args opts cont err)
                  (define idx (car args))
                  (define val (cadr args))
                  (check-idx obj "set!" cont err idx
                     (lambda ()
                        (vector-set! obj idx val)
                        obj))))))

      (method v-apply
         (cont
            (vaquero-proc
               primitive-type
               'vector
               (lambda (args opts cont err)
                  (vaquero-send-vector obj (caar args) cont err)))))

      (method v-default
         (if (number? msg)
            (if (> (vector-length obj) msg)
               (cont (vector-ref obj msg))
               (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont))
            (idk obj msg cont err)))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-list    . ,to-list)
           (to-set     . ,to-set)
           (to-table   . ,to-table)
           (to-vector  . ,to-vector)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,view)
           (clone      . ,clone)
           (pairs      . ,pairs)
           (size       . ,size)
           (has?       . ,has?)
           (get        . ,v-get)
           (set!       . ,v-set!)
           (apply      . ,v-apply)
           (append     . ,v-append)
           (default    . ,v-default)))))


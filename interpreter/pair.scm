
(define vaquero-send-list-vtable #f)
(define vaquero-send-pair-vtable #f)

(let ()
   (method view
      (cont (vaquero-view obj)))

   (define to-text view)

   (set! vaquero-send-list-vtable
      (let ()
         (define (list-set! list k val)
            (if (zero? k)
               (set-car! list val)
               (list-set! (cdr list) (- k 1) val)))

         (method list-default
            (if (number? msg)
               (if (> (length obj) msg)
                  (cont (list-ref obj msg))
                  (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "list: index out of bounds.") cont))
               (idk obj msg cont err)))

         (method answers?
            (cont (lambda (msg)
               (or
                  (and (number? msg) (> (length obj) msg))
                  (hte? vaquero-send-list-vtable msg)))))

         (method autos
            (cont '(autos messages type view empty? to-bool to-text to-list to-vector to-table head tail key val size reverse clone)))

         (method messages
            (cont (htks vaquero-send-list-vtable)))

         (method type
            (cont '(list pair)))

         (method to-bool
            (cont (not (eq? obj '()))))

         (method to-list
            (cont obj))

         (method to-vector
            (cont (list->vector obj)))

         (method to-table
            (if (not (every pair? obj))
               (err (vaquero-error-object 'not-an-associative-list! `(send ,obj to-table) "list: to-table only works on associative lists." ) cont)
               (let ((r (vaquero-table)))
                  (for-each (lambda (p) (hts! r (car p) (cdr p))) obj)
                  (cont r))))

         (method to-set
            (cont (apply make-vaquero-set obj)))

         (method empty?
            (cont (eq? obj '())))

         (method head
            (cont
               (if (eq? obj '())
                  'null
                  (car obj))))

         (method tail
            (cont
               (if (eq? obj '())
                  'null
                  (cdr obj))))

         (method head!
            (if (eq? obj '())
               (err (vaquero-error-object 'not-a-pair `(send ,obj ,head!) "list: attempt to mutate an empty list.") cont)
               (cont (lambda (v) (set-car! obj v) v))))

         (method tail!
            (if (eq? obj '())
               (err (vaquero-error-object 'not-a-pair `(send ,obj ,tail!) "list: attempt to mutate an empty list.") cont)
               (cont (lambda (v) (set-cdr! obj v) v))))

         (method set!
            (if (eq? obj '())
               (err (vaquero-error-object 'not-a-pair `(send ,obj ,set!) "list: attempt to mutate an empty list.") cont)
               (cont
                  (lambda (i v)
                     (if (> (length obj) i)
                        (begin (list-set! obj i v) v)
                        (err (vaquero-error-object 'out-of-bounds `(,obj ,i) "list: index out of bounds.") cont))))))

         (method list-cons
            (cont (lambda (v) (cons v obj))))

         (method size
            (cont (length obj)))

         (method clone
            (cont (list-copy obj)))

         (method has?
            (cont
               (lambda (item)
                   (if (member item obj)
                       #t
                       #f))))

         (method list-append
            (cont (lambda (other) (append obj other))))

         (method list-reverse
            (cont (reverse obj)))

         (method list-take
            (cont (lambda (n) (take obj n))))

         (method list-drop
            (cont (lambda (n) (drop obj n))))

         (method list-apply
            (cont
               (vaquero-proc
                   primitive-type
                   'list
                   (lambda (args opts cont err)
                       (if (pair? (car args))
                           (vaquero-send-list obj (caar args) cont err)
                           (err (vaquero-error-object 'message-not-understood `(,obj ,args ,opts) "Message not understood.") cont))))))

         (alist->hash-table
            `((answers?   . ,answers?)
              (autos      . ,autos)
              (messages   . ,messages)
              (to-bool    . ,to-bool)
              (to-list    . ,to-list)
              (to-set     . ,to-set)
              (to-table   . ,to-table)
              (to-text    . ,to-text)
              (to-vector  . ,to-vector)
              (type       . ,type)
              (view       . ,view)
              (empty?     . ,empty?)
              (cons       . ,list-cons)
              (head       . ,head)
              (key        . ,head)
              (car        . ,head)
              (tail       . ,tail)
              (val        . ,tail)
              (cdr        . ,tail)
              (head!      . ,head!)
              (tail!      . ,tail!)
              (set!       . ,set!)
              (size       . ,size)
              (clone      . ,clone)
              (has?       . ,has?)
              (reverse    . ,list-reverse)
              (append     . ,list-append)
              (take       . ,list-take)
              (drop       . ,list-drop)
              (apply      . ,list-apply)
              (default    . ,list-default)))))

   (set! vaquero-send-pair-vtable
      (let ()
         (method answers?
            (cont (lambda (msg) (hte? vaquero-send-pair-vtable msg))))

         (method autos
            (cont '(autos messages type view to-bool to-text head tail key val size clone)))

         (method messages
            (cont (htks vaquero-send-pair-vtable)))

         (method type
            (cont '(pair)))

         (method to-bool
            (cont #t))

         (method head
            (cont (car obj)))

         (method tail
            (cont (cdr obj)))

         (method head!
            (cont (lambda (v) (set-car! obj v) v)))

         (method tail!
            (cont (lambda (v) (set-cdr! obj v) v)))

         (method size
            (cont 2))

         (method clone
            (cont (cons (car obj) (cdr obj))))

         ;(method to-list (cont (list (car obj) (cdr obj))))
         ;(method to-table (cont (vaquero-table (car obj) (cdr obj))))

         (alist->hash-table
            `((answers?   . ,answers?)
              (autos      . ,autos)
              (messages   . ,messages)
              (to-bool    . ,to-bool)
              (to-text    . ,to-text)
              (type       . ,type)
              (view       . ,view)
              (head       . ,head)
              (key        . ,head)
              (tail       . ,tail)
              (val        . ,tail)
              (head!      . ,head!)
              (tail!      . ,tail!)
              (size       . ,size)
              (clone      . ,clone)
              (default    . ,idk))))))




(define-record-type vaq-tuple
   (vaquero-tuple fields size)
   vaquero-tuple?
   (fields vaq-tuple-fields)
   (size vaq-tuple-size))

(define (make-vaquero-tuple . args)
   (if (null? args)
      (vaquero-tuple '() 0)
      (let loop ((k (car args)) (v (cadr args)) (rest (cddr args)) (acc '()))
         (let* ((t (cons k v)) (acc+ (cons t acc)))
            (if (null? rest)
               (vaquero-tuple acc+ (length acc+))
               (loop (car rest) (cadr rest) (cddr rest) acc+))))))

(define (bangify s)
   (string->symbol (string-append (symbol->string s) "!")))

(define (unbang s)
   (string->symbol (string-drop-right (symbol->string s) 1)))

(define (fields->fields! fs)
   (map bangify fs))

(define vaquero-send-tuple-vtable
   (let ()
      (define (fieldlist t)
         (map car (vaq-tuple-fields t)))

      (method answers?
         (cont
            (lambda (msg)
               (define fields (fieldlist obj))
               (define fields! (fields->fields! fields))
               (or (hte? vaquero-send-tuple-vtable msg)
                   (if (member msg fields) #t #f)
                   (if (member msg fields!) #t #f)))))

      (method autos
         (cont (append '(autos clone messages size to-bool to-text type view)
                        (fieldlist obj))))

      (method tuple-clone
         (cont
            (vaquero-tuple (alist-copy (vaq-tuple-fields obj)) (vaq-tuple-size obj))))

      (method tuple-add
         (cont (lambda (k v)
            (define new-fields (cons (cons k v) (list-copy (vaq-tuple-fields obj))))
            (vaquero-tuple new-fields (length new-fields)))))

      (method tuple-del
         (cont (lambda (k)
            (define new-fields (filter (lambda (p) (not (eq? k (car p))))
                                       (list-copy (vaq-tuple-fields obj))))
            (vaquero-tuple new-fields (length new-fields)))))

      (method messages
         (let ((fields (fieldlist obj)))
            (let ((fields! (fields->fields! fields)))
               (cont (append (htks vaquero-send-tuple-vtable) fields fields!)))))

      (method size
         (cont (vaq-tuple-size obj)))

      (method type
         (cont '(tuple)))

      (method to-bool
         (cont (if (= 0 (vaq-tuple-size obj)) #f #t)))

      (method to-text
         (cont (vaquero-view obj)))

      (method to-list
         (cont (vaq-tuple-fields obj)))

      (method tuple-fields
         (cont (fieldlist obj)))

      (method tuple-default
         (let ((fields (vaq-tuple-fields obj)))
            (if (null? fields)
               (idk obj msg cont err)
               (let ((kv (assoc msg fields)))
                  (if kv
                     (cont (cdr kv))
                     (let ((kv! (assoc (unbang msg) fields)))
                        (if kv!
                           (cont 
                              (lambda (v)
                                 (set-cdr! kv! v)
                                 v))
                           (idk obj msg cont err))))))))

      (method tuple-eq?
         (cont (lambda (other)
            (if (vaquero-tuple? other)
               (let ((x-pairs (sort-symbol-alist (vaq-tuple-fields obj)))
                     (y-pairs (sort-symbol-alist (vaq-tuple-fields other))))
                  (vaquero-equal? x-pairs y-pairs))
               #f))))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,to-text)
           (clone      . ,tuple-clone)
           (add        . ,tuple-add)
           (del        . ,tuple-del)
           (size       . ,size)
           (eq?        . ,tuple-eq?)
           (fields     . ,tuple-fields)
           (default    . ,tuple-default)))))


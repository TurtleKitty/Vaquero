

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
      (method answers?
         (cont
            (lambda (msg)
               (define fields (vaq-tuple-fields obj))
               (define fields! (fields->fields! fields))
               (or (hte? vaquero-send-tuple-vtable msg)
                   (member? msg fields)
                   (member? msg fields!)))))

      (method autos
         (cont (append '(autos clone messages size to-bool to-text type view)
                        (map car (vaq-tuple-fields obj)))))

      (method tuple-clone
         (cont
            (vaquero-tuple (list-copy (vaq-tuple-fields obj) (vaq-tuple-size obj)))))

      (method tuple-add
         (cont (lambda (k v)
            (define new-fields (cons (cons k v) (list-copy (vaq-tuple-fields obj))))
            (vaquero-tuple new-fields (length new-fields)))))

      (method tuple-del
         (cont (lambda (k)
            (define new-fields (filter (lambda (x) (not (eq? x k)))
                                       (list-copy (vaq-tuple-fields))))
            (vaquero-tuple new-fields (length new-fields)))))

      (method messages
         (let ((fields (vaq-tuple-fields obj)))
            (let ((fields! (fields->fields! fields)))
               (cont (append (htks vaquero-send-tuple-vtable) fields fields!)))))

      (method size
         (cont (vaq-tuple-size obj)))

      (method type
         (cont '(tuple)))

      (method view
         (cont "xyzzy"))

      (method to-bool
         (cont (if (= 0 (vaq-tuple-size obj)) #f #t)))

      (method to-text
         (cont (vaquero-view obj)))

      (method to-list
         (cont (vaq-tuple-fields obj)))

      (method tuple-fields
         (cont (map car (vaq-tuple-fields obj))))

      (method tuple-default
         (cont
            (let ((fields (vaq-tuple-fields obj)))
               (if (null? fields)
                  (idk obj msg cont err)
                  (let ((kv (assoc msg obj)))
                     (if kv
                        (cdr kv)
                        (let ((kv! (assoc (unbang msg) obj)))
                           (if kv!
                              (lambda (v)
                                 (set-cdr! kv! v))
                              (idk obj msg cont err)))))))))

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
           (fields     . ,tuple-fields)
           (default    . ,tuple-default)))))


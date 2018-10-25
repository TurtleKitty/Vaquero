
(define-record-type vaq-set
   (vaquero-set items) 
   vaquero-set?
   (items vaquero-set-items))

(define (make-vaquero-set . args)
   (define this (mkht))
   (define (add x) (hts! this x #t))
   (map add args)
   (vaquero-set this))

(define vaquero-send-set-vtable
   (let ()
      (method answers?
         (cont (lambda (msg)
             (hte? vaquero-send-table-vtable msg))))

      (method autos
         (cont '(clone size to-bool to-list to-text type view)))

      (method messages
         (cont (htks vaquero-send-set-vtable)))

      (method to-bool
         (cont (> (hash-table-size (vaquero-set-items obj)) 0)))

      (method to-list
         (cont (htks (vaquero-set-items obj))))

      (method to-text
         (cont (vaquero-view obj)))

      (method type
         (cont '(set)))

      (define (set-copy-items s)
         (hash-table-copy (vaquero-set-items s)))

      (define (set->list s)
         (htks (vaquero-set-items s)))

      (method clone
         (cont
            (vaquero-set (set-copy-items obj))))

      (method size
         (cont (hash-table-size (vaquero-set-items obj))))

      (method set-add
         (cont
            (lambda args
               (define noob (set-copy-items obj))
               (define (add x) (hts! noob x #t))
               (map add args)
               (vaquero-set noob))))

      (method set-del
         (cont
            (lambda args
               (define noob (set-copy-items obj))
               (define (del x) (htd! noob x))
               (map del args)
               (vaquero-set noob))))

      (method set-add!
         (cont
            (lambda args
               (define this (vaquero-set-items obj))
               (define (add x) (hts! this x #t))
               (map add args)
               obj)))

      (method set-del!
         (cont
            (lambda args
               (define this (vaquero-set-items obj))
               (define (del x) (htd! this x))
               (map del args)
               obj)))

      (method has?
         (cont
            (lambda (x)
               (hte? (vaquero-set-items obj) x))))

      (method union
         (cont
            (lambda (other)
               (vaquero-set
                  (hash-table-merge (vaquero-set-items other)
                                    (vaquero-set-items obj))))))

      (method intersect
         (cont
            (lambda (other)
               (define ours (set->list obj))
               (define theirs (set->list other))
               (apply make-vaquero-set
                  (filter (lambda (x) (and (member x ours) (member x theirs)))
                          (append ours theirs))))))

      (method set-diff
         (cont
            (lambda (other)
               (define ours (set->list obj))
               (define theirs (set->list other))
               (apply make-vaquero-set
                  (filter (lambda (x) (and (member x ours) (not (member x theirs))))
                          (append ours theirs))))))

      (method set-xor
         (cont
            (lambda (other)
               (define ours (set->list obj))
               (define theirs (set->list other))
               (apply make-vaquero-set
                  (filter (lambda (x) (not (and (member x ours) (member x theirs))))
                          (append ours theirs))))))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-list    . ,to-list)
           (to-text    . ,to-text)
           (view       . ,to-text)
           (type       . ,type)
           (clone      . ,clone)
           (size       . ,size)
           (add        . ,set-add)
           (del        . ,set-del)
           (add!       . ,set-add!)
           (del!       . ,set-del!)
           (has?       . ,has?)
           (union      . ,union)
           (intersect  . ,intersect)
           (diff       . ,set-diff)
           (xor        . ,set-xor)
           (default    . ,idk)))))



(define (vaquero-table . args)
   (define this (mkht))
   (for-pairs
      (lambda (k v)
         (hts! this k v))
      args)
   this)

(define vaquero-send-table-vtable
   (let ()
      (method answers?
         (cont (lambda (msg)
            (or (hte? obj msg)
                (hte? vaquero-send-table-vtable msg)))))

      (method autos
         (cont '(view size clone to-bool to-list to-text keys values pairs)))

      (method messages
         (cont (htks vaquero-send-table-vtable)))

      (method to-bool
         (cont (> (hash-table-size obj) 0)))

      (method to-list
         (cont (hash-table->alist obj)))

      (method to-text
         (cont (vaquero-view obj)))

      (method type
         (cont 'table))

      (method clone
         (cont
            (hash-table-copy obj)))

      (method size
         (cont (hash-table-size obj)))

      (method get
         (cont
            (lambda (k)
               (if (hte? obj k)
                  (htr obj k)
                  'null))))

      (method put
         (cont
            (lambda args
               (define noob (hash-table-copy obj))
               (vaquero-send-table
                  noob
                  'set!
                  (lambda (setter!)
                     (apply setter! args)
                     noob)
                  err))))

      (method rm
         (cont
            (lambda args
               (define noob (hash-table-copy obj))
               (vaquero-send-table
                  noob
                  'del!
                  (lambda (deleter!)
                      (apply deleter! args)
                      noob)
                  err))))

      (method table-set!
         (cont
            (lambda args
                (for-pairs (lambda (k v) (hts! obj k v)) args)
                'null)))

      (method del!
         (cont
            (lambda args
               (map (lambda (k) (htd! obj k)) args)
               'null)))

      (method has?
         (cont
            (lambda (x)
               (hte? obj x))))

      (method table-keys
         (cont (htks obj)))

      (method table-values
         (cont (htvs obj)))

      (method vaquero-table-merge
         (cont
            (lambda (other)
               (hash-table-merge other obj))))

      (method table-apply
         (cont
            (vaquero-proc
               primitive-type
               'table
               (lambda (args opts cont err)
                  (vaquero-send-table obj (caar args) cont err)))))

      (method table-default
         (cont
            (if (hte? obj msg)
               (htr obj msg)
               'null)))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-list    . ,to-list)
           (to-text    . ,to-text)
           (pairs      . ,to-list)
           (view       . ,to-text)
           (type       . ,type)
           (clone      . ,clone)
           (size       . ,size)
           (get        . ,get)
           (put        . ,put)
           (rm         . ,rm)
           (set!       . ,table-set!)
           (del!       . ,del!)
           (has?       . ,has?)
           (keys       . ,table-keys)
           (values     . ,table-values)
           (apply      . ,table-apply)
           (default    . ,table-default)))))



(define-syntax get-table-vars
   (ir-macro-transformer
      (lambda (expr inject compare)
         `(define ,(inject 'vars) (,(inject 'htr) ,(inject 'obj) ,(inject ''vars))))))

(define vaquero-send-table-vtable
   (let ()
      (method answers?
         (get-table-vars)
         (cont (lambda (msg)
            (or (hte? vars msg)
                (hte? vaquero-send-table-vtable msg)))))

      (method autos
         (cont '(view size clone to-bool to-list to-text keys values pairs)))

      (method messages
         (cont (htks vaquero-send-table-vtable)))

      (method to-bool
         (get-table-vars)
         (cont (> (hash-table-size vars) 0)))

      (method to-list
         (get-table-vars)
         (cont (hash-table->alist vars)))

      (method to-text
         (cont (vaquero-view obj)))

      (method type
         (cont 'table))

      (method clone
         (get-table-vars)
         (cont
            (let ((noob (vaquero-table)))
                (hts! noob 'vars (hash-table-copy vars))
                noob)))

      (method size
         (get-table-vars)
         (cont (hash-table-size vars)))

      (method get
         (get-table-vars)
         (cont
            (lambda (k)
               (if (hte? vars k)
                  (htr vars k)
                  'null))))

      (method put
         (get-table-vars)
         (cont
            (lambda args
               (define noob (vaquero-table))
               (hts! noob 'vars (hash-table-copy vars))
               (vaquero-send-table
                  noob
                  'set!
                  (lambda (setter!)
                     (apply setter! args)
                     noob)
                  err))))

      (method rm
         (get-table-vars)
         (cont
            (lambda args
               (define noob (vaquero-table))
               (hts! noob 'vars (hash-table-copy vars))
               (vaquero-send-table
                  noob
                  'del!
                  (lambda (deleter!)
                      (apply deleter! args)
                      noob)
                  err))))

      (method table-set!
         (get-table-vars)
         (cont
            (lambda args
                (for-pairs (lambda (k v) (hts! vars k v)) args)
                'null)))

      (method del!
         (get-table-vars)
         (cont
            (lambda args
               (map (lambda (k) (htd! vars k)) args)
               'null)))

      (method has?
         (get-table-vars)
         (cont
            (lambda (x)
               (hte? vars x))))

      (method table-keys
         (get-table-vars)
         (cont (htks vars)))

      (method table-values
         (get-table-vars)
         (cont (htvs vars)))

      (method table-apply
         (cont
            (vaquero-proc
               primitive-type
               'table
               (lambda (args opts cont err)
                  (vaquero-send-table obj (caar args) cont err)))))

      (method table-default
         (get-table-vars)
         (cont
            (if (hte? vars msg)
               (htr vars msg)
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

(define (vaquero-table-merge table other)
   (get-table-vars)
   (define nuvars (hash-table-merge (htr other 'vars) vars))
   (define noob (mkht))
   (hts! noob 'type 'table)
   (hts! noob 'vars nuvars)
   noob)


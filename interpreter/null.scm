
(define vaquero-send-null-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-null-vtable msg))))

      (method autos
         (cont '(autos messages to-bool to-text type view)))

      (method messages
         (cont (htks vaquero-send-null-vtable)))

      (method to-bool
         (cont #f))

      (method to-text
         (cont ""))

      (method type
         (cont '(null)))

      (method view
         (cont 'null))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,view)
           (default    . ,idk)))))


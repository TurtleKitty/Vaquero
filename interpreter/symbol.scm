
(define vaquero-send-symbol-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-symbol-vtable msg))))

      (method autos
         (cont '(autos messages to-bool to-symbol to-text type view)))

      (method messages
         (cont (htks vaquero-send-symbol-vtable)))

      (method to-bool
         (cont #t))

      (method to-symbol
         (cont obj))

      (method to-text
         (cont (symbol->string obj)))

      (method type
         (cont 'symbol))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-symbol  . ,to-symbol)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,to-symbol)
           (default    . ,idk)))))


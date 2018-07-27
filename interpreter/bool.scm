
(define vaquero-send-bool-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-bool-vtable msg))))

      (method autos
         (cont '(autos messages not to-bool to-text type view)))

      (method messages
         (cont (htks vaquero-send-bool-vtable)))

      (method bool-not
         (cont (not obj)))

      (method to-bool
         (cont obj))

      (method to-symbol
         (cont (if obj 'true 'false)))

      (method to-text
         (cont (if obj "true" "false")))

      (method type
         (cont 'bool))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (not        . ,bool-not)
           (to-bool    . ,to-bool)
           (to-symbol  . ,to-symbol)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,to-symbol)
           (default    . ,idk)))))


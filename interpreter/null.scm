
(define vaquero-send-null-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-null-vtable msg))))

      (method null-apply
         (err (vaquero-error-object 'null-is-not-applicable '(null ...) "Null can not be used as a procedure.") cont))

      (method autos
         (cont '(autos messages to-bool to-text type view)))

      (method default
         (cont 'null))  ; FIXME make null blow up like a little grenade

      (method messages
         (cont (htks vaquero-send-null-vtable)))

      (method to-bool
         (cont #f))

      (method to-text
         (cont ""))

      (method type
         (cont 'null))

      (method view
         (cont 'null))

      (alist->hash-table
         `((answers?   . ,answers?)
           (apply      . ,null-apply)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,view)
           (default    . ,default)))))


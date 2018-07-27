
(define vaquero-send-EOF-vtable
   (let ()
      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-EOF-vtable msg))))

      (method EOF-apply
         (err (vaquero-error-object 'eof-is-not-applicable '(EOF ...) "EOF objects can not be used as procedures.") cont))

      (method autos
         (cont '(autos messages to-bool to-text type view)))

      (method messages
         (cont (htks vaquero-send-EOF-vtable)))

      (method to-bool
         (cont #f))

      (method to-text
         (cont "END OF LINE."))

      (method type
         (cont 'EOF))

      (method view
         (cont 'EOF))

      (alist->hash-table
         `((answers?   . ,answers?)
           (apply      . ,EOF-apply)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,view)
           (default    . ,idk)))))


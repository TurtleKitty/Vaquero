
(use r7rs)

(define-library (send symbol)
  (import (scheme base)) ; utils
  (export vaquero-send-symbol-vtable)
  (begin

(define vaquero-send-symbol-vtable
   (alist->hash-table
      `((answers?   . ,answers?)
        (autos      . ,autos)
        (can?       . ,can)
        (does       . ,does)
        (messages   . ,messages)
        (to-bool    . ,to-bool)
        (to-symbol  . ,to-symbol)
        (to-text    . ,to-text)
        (type       . ,type)
        (view       . ,to-symbol)
        (default    . ,idk))))

(method answers?
   (cont (lambda (msg) (hte? vaquero-send-symbol-vtable msg))))

(method autos
   (cont '(autos does messages to-bool to-symbol to-text type view default)))

(method can?
   (cont (lambda (t) (eq t 'symbol)))) 

(method does
   (cont <empty set>))

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

))

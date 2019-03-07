
(define vaquero-send-int-vtable #f)
(define vaquero-send-real-vtable #f)

(let ()
   (method vaquero-abs
      (cont (abs obj)))

   (method pos?
      (cont (> obj 0)))

   (method neg?
      (cont (< obj 0)))

   (method zero?
      (cont (= obj 0)))

   (method to-bool
      (cont (not (= obj 0))))

   (method to-text
      (cont (number->string obj)))

   (method to-number
      (cont obj))

   (set! vaquero-send-int-vtable
      (let ()
         (method answers?
            (cont (lambda (msg) (hte? vaquero-send-int-vtable msg))))

         (method autos
            (cont '(autos messages to-bool to-text type view zero? pos? neg? abs floor ceil round truncate inc dec even? odd?)))

         (method messages
            (cont (htks vaquero-send-int-vtable)))

         (method type
            (cont '(int number)))

         (method view
            (cont obj))

         (method inc
            (cont (+ obj 1)))

         (method dec
            (cont (- obj 1)))

         (method int-even?
            (cont (even? obj)))

         (method int-odd?
            (cont (odd? obj)))

         (method int-floor
            (cont obj))

         (method int-ceil
            (cont obj))

         (method int-round
            (cont obj))

         (method int-truncate
            (cont obj))

         (alist->hash-table
            `((answers?   . ,answers?)
              (autos      . ,autos)
              (messages   . ,messages)
              (to-bool    . ,to-bool)
              (to-number  . ,to-number)
              (to-text    . ,to-text)
              (type       . ,type)
              (view       . ,view)
              (abs        . ,vaquero-abs)
              (pos?       . ,pos?)
              (neg?       . ,neg?)
              (zero?      . ,zero?)
              (even?      . ,int-even?)
              (odd?       . ,int-odd?)
              (floor      . ,int-floor)
              (ceil       . ,int-ceil)
              (round      . ,int-round)
              (truncate   . ,int-truncate)
              (inc        . ,inc)
              (dec        . ,dec)
              (default    . ,idk)))))

   (set! vaquero-send-real-vtable
      (let ()
         (method answers?
            (cont (lambda (msg) (hte? vaquero-send-real-vtable msg))))

         (method autos
            (cont '(autos messages to-bool to-text type view abs pos? neg? zero? floor ceil round truncate)))

         (method messages
            (cont (htks vaquero-send-real-vtable)))

         (method type
            (cont '(real number)))

         (method view
            (cont (* 1.0 obj)))

         (method real-floor
            (cont (inexact->exact (floor obj))))

         (method real-ceil
            (cont (inexact->exact (ceiling obj))))

         (method real-round
            (cont (inexact->exact (round obj))))

         (method real-truncate
            (cont (inexact->exact (truncate obj))))

         (alist->hash-table
            `((answers?   . ,answers?)
              (autos      . ,autos)
              (messages   . ,messages)
              (to-bool    . ,to-bool)
              (to-text    . ,to-text)
              (to-number  . ,to-number)
              (type       . ,type)
              (view       . ,view)
              (abs        . ,vaquero-abs)
              (pos?       . ,pos?)
              (neg?       . ,neg?)
              (zero?      . ,zero?)
              (floor      . ,real-floor)
              (ceil       . ,real-ceil)
              (round      . ,real-round)
              (truncate   . ,real-truncate)
              (default    . ,idk))))))


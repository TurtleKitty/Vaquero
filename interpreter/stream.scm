
(define vaquero-send-source-vtable #f)
(define vaquero-send-sink-vtable #f)

(let ()
   (method view
      (cont obj))

   (method input?
      (cont (input-port? obj)))

   (method output?
      (cont (output-port? obj)))

   (method open?
      (cont (not (port-closed? obj))))

   (set! vaquero-send-source-vtable
      (let ()
         (define (if-alive obj msg cont err thunk)
            (if (port-closed? obj)
                (err (vaquero-error-object 'source-closed `(send ,obj ,msg) "Source closed.") cont)
                (cont (thunk))))

         (method answers?
            (cont (lambda (msg) (hte? vaquero-send-source-vtable msg))))

         (method autos
            (cont '(view to-text to-bool to-list ready? input? output? open? read read-seq read-char peek-char read-line read-lines read-text)))

         (method messages
            (cont (htks vaquero-send-source-vtable)))

         (method type
            (cont '(source stream)))

         (method ready?
            (cont (char-ready? obj)))

         (method stream-read
            (if-alive obj msg cont err
               (vaquero-wrap-user-facing-interpreter
                  (lambda ()
                     (vaquero-read obj)))))

         (method read-seq
            (if-alive obj msg cont err
               (vaquero-wrap-user-facing-interpreter
                  (lambda ()
                     (vaquero-read-file obj)))))

         (method stream-read-char
            (if-alive obj msg cont err
               (lambda ()
                  (string (read-char obj)))))

         (method stream-peek-char
            (if-alive obj msg cont err
               (lambda ()
                  (string (peek-char obj)))))

         (method stream-read-line
            (if-alive obj msg cont err
               (lambda ()
                  (read-line obj))))

         (method stream-read-lines
            (if-alive obj msg cont err
               (lambda ()
                  (read-lines obj))))

         (method stream-read-text
            (if-alive obj msg cont err
               (lambda ()
                  (read-string #f obj))))

         (method read-n
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (n)
                     (read-string n obj)))))

         (method read-while
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (s)
                     (define runes (string->list s))
                     (let loop ((tok (peek-char obj)) (acc '()))
                        (if (member tok runes)
                           (let ((t (read-char obj)))
                              (loop (peek-char obj) (cons t acc)))
                           (list->string (reverse acc))))))))

         (method read-until
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (s)
                     (define runes (string->list s))
                     (let loop ((tok (peek-char obj)) (acc '()))
                        (if (member tok runes)
                           (list->string (reverse acc))
                           (let ((t (read-char obj)))
                              (loop (peek-char obj) (cons t acc)))))))))

         (method skip-n
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (n)
                     (read-string n obj)
                     'null))))

         (method skip-while
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (s)
                     (define runes (string->list s))
                     (let loop ((tok (peek-char obj)))
                        (if (member tok runes)
                           (begin
                              (read-char obj)
                              (loop (peek-char obj)))
                           'null))))))

         (method skip-until
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (s)
                     (define runes (string->list s))
                     (let loop ((tok (peek-char obj)))
                        (if (member tok runes)
                           'null
                           (begin
                              (read-char obj)
                              (loop (peek-char obj)))))))))

         (method close
            (cont (close-input-port obj) (cont 'null)))

         (alist->hash-table
            `((answers?     . ,answers?)
              (autos        . ,autos)
              (messages     . ,messages)
              (to-bool      . ,open?)
              (to-text      . ,stream-read-text)
              (to-list      . ,stream-read-lines)
              (to-stream    . ,view)
              (type         . ,type)
              (view         . ,view)
              (input?       . ,input?)
              (output?      . ,output?)
              (open?        . ,open?)
              (ready?       . ,ready?)
              (read         . ,stream-read)
              (read-seq     . ,read-seq)
              (read-char    . ,stream-read-char)
              (peek-char    . ,stream-peek-char)
              (read-line    . ,stream-read-line)
              (read-lines   . ,stream-read-lines)
              (read-text    . ,stream-read-text)
              (read-n       . ,read-n)
              (read-while   . ,read-while)
              (read-until   . ,read-until)
              (skip-n       . ,skip-n)
              (skip-while   . ,skip-while)
              (skip-until   . ,skip-until)
              (close        . ,close)
              (default      . ,idk)))))

   (set! vaquero-send-sink-vtable
      (let ()
         (define (if-alive obj msg cont err thunk)
            (if (port-closed? obj)
                (err (vaquero-error-object 'sink-closed `(send ,obj ,msg) "Sink closed.") cont)
                (cont (thunk))))

         (method answers?
            (cont (lambda (msg) (hte? vaquero-send-sink-vtable msg))))

         (method autos
            (cont '(view to-bool input? output? open? nl close)))

         (method messages
            (cont (htks vaquero-send-sink-vtable)))

         (method type
            (cont '(sink stream)))

         (method stream-write
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (x)
                     (vaquero-write x obj)
                     'null))))

         (method stream-print
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (x)
                     (vaquero-print x obj)
                     'null))))

         (method stream-say
            (if-alive obj msg cont err
               (lambda ()
                  (lambda (x)
                     (vaquero-print x obj)
                     (newline obj)
                     'null))))

         (method nl
            (if-alive obj msg cont err
               (lambda ()
                  (newline obj)
                  'null)))

         (method flush
            (flush-output obj) (cont 'null))

         (method close
            (close-output-port obj) (cont 'null))

         (alist->hash-table
            `((answers?   . ,answers?)
              (autos      . ,autos)
              (messages   . ,messages)
              (to-bool    . ,open?)
              (to-stream  . ,view)
              (type       . ,type)
              (view       . ,view)
              (input?     . ,input?)
              (output?    . ,output?)
              (open?      . ,open?)
              (write      . ,stream-write)
              (print      . ,stream-print)
              (say        . ,stream-say)
              (nl         . ,nl)
              (flush      . ,flush)
              (close      . ,close)
              (default    . ,idk))))))


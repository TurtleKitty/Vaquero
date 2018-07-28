
(define vaquero-send-text-vtable
   (let ()
      (define (build-regex re flags)
         (define opts
            (append
               (list re 'fast 'utf8)
               (filter
                  (lambda (x) (not (eq? x 'g)))
                  (map string->symbol (string-split flags "")))))
         (apply irregex opts))

      (method answers?
         (cont (lambda (msg)
                  (or (hte? vaquero-send-text-vtable msg)
                      (and (number? msg) (> (string-length obj) msg))))))

      (method autos (cont '(view to-bool to-symbol to-text to-keyword to-number to-list to-stream size chomp ltrim rtrim trim alphabetic? numeric? whitespace? uc? lc? uc lc)))

      (method messages
         (cont (htks vaquero-send-text-vtable)))

      (method type
         (cont 'text))

      (method view
         (cont obj))

      (method clone
         (cont (string-copy obj)))

      (method to-bool
         (cont (not (eq? 0 (string-length obj)))))

      (method to-symbol
         (cont (string->symbol obj)))

      (method to-keyword
         (cont (string->keyword obj)))

      (method to-number
         (cont (string->number obj)))

      (method to-list
         (cont (string->list obj)))

      (method to-vector
         (cont (list->vector (string->list obj))))

      (method to-text
         (cont obj))

      (method to-stream
         (cont (open-input-string obj)))

      (method alphabetic?
         (cont (every char-alphabetic? (string->list obj))))

      (method numeric?
         (cont (if (string->number obj) #t #f)))

      (method whitespace?
         (cont (every char-whitespace? (string->list obj))))

      (method uc?
         (cont (every char-upper-case? (string->list obj))))

      (method lc?
         (cont (every char-lower-case? (string->list obj))))

      (method uc
         (cont (string-upcase obj)))

      (method lc
         (cont (string-downcase obj)))

      (method take
         (cont (lambda (n) (string-take obj n))))

      (method drop
         (cont (lambda (n) (string-drop obj n))))

      (method trim
         (cont (string-trim-both obj)))

      (method ltrim
         (cont (string-trim obj)))

      (method rtrim
         (cont (string-trim-right obj)))

      (method lpad
         (cont (lambda (pad n) (string-pad obj n (string-ref pad 0)))))

      (method rpad
         (cont (lambda (pad n) (string-pad-right obj n (string-ref pad 0)))))

      (method chomp
         (cont (string-chomp obj)))

      (method index
         (cont (lambda (which) (substring-index which obj))))

      (method size
         (cont (string-length obj)))

      (method split
         (cont
            (vaquero-proc
               primitive-type
              'text
              (lambda (args opts cont err)
                 (define flags (vaquero-send-atomic opts 'flags))
                 (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                 (cont (irregex-split re obj))))))

      (method match
         (cont
            (vaquero-proc
               primitive-type
               'text
               (lambda (args opts cont err)
                  (define flags (vaquero-send-atomic opts 'flags))
                  (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                  (define rez (irregex-search re obj))
                  (cont
                      (if rez
                          #t
                          #f))))))

      (method capture
         (cont
            (vaquero-proc
               primitive-type
               'text
               (lambda (args opts cont err)
                  (define flags (vaquero-send-atomic opts 'flags))
                  (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                  (cont
                     (irregex-fold
                        re
                        (lambda (idx match acc)
                           (define n (irregex-match-num-submatches match))
                           (let loop ((this n) (matches '()))
                              (if (= this 0)
                                 (cons matches acc)
                                 (loop (- this 1) (cons (irregex-match-substring match this) matches)))))
                        '()
                        obj
                        (lambda (idx acc) (reverse acc))))))))

      (method replace
         (cont
            (vaquero-proc
               primitive-type
               'text
               (lambda (args opts cont err)
                  (define fopt (vaquero-send-atomic opts 'flags))
                  (define flags (if (eq? 'null fopt) "" fopt))
                  (define re (build-regex (car args) flags))
                  (cont
                     (if (string-contains flags "g")
                         (apply irregex-replace/all (cons re (cons obj (cdr args))))
                         (apply irregex-replace (cons re (cons obj (cdr args))))))))))

      (method text-set!
         (cont
            (lambda (idx val)
               (if (not (number? idx))
                  (err (vaquero-error-object 'not-a-number `(,obj ,idx) "text: set! requires a number as its first argument.") cont)
                  (if (> idx (string-length obj))
                     (err (vaquero-error-object 'out-of-bounds `(,obj ,idx) "text: index out of bounds.") cont)
                     (begin
                        (string-set! obj idx (string-ref val 0))
                        obj))))))

      (method text-default
         (if (number? msg)
            (if (> (string-length obj) msg)
               (cont (string (string-ref obj msg)))
               (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "text: index out of bounds.") cont))
            (idk obj msg cont err)))

      (alist->hash-table
         `((answers?     .   ,answers?)
           (autos        .   ,autos)
           (messages     .   ,messages)
           (to-bool      .   ,to-bool)
           (to-symbol    .   ,to-symbol)
           (to-text      .   ,to-text)
           (type         .   ,type)
           (view         .   ,view)
           (to-keyword   .   ,to-keyword)
           (to-number    .   ,to-number)
           (to-list      .   ,to-list)
           (to-vector    .   ,to-vector)
           (to-stream    .   ,to-stream)
           (alphabetic?  .   ,alphabetic?)
           (numeric?     .   ,numeric?)
           (whitespace?  .   ,whitespace?)
           (uc?          .   ,uc?)
           (lc?          .   ,lc?)
           (uc           .   ,uc)
           (lc           .   ,lc)
           (take         .   ,take)
           (drop         .   ,drop)
           (trim         .   ,trim)
           (ltrim        .   ,ltrim)
           (rtrim        .   ,rtrim)
           (lpad         .   ,lpad)
           (rpad         .   ,rpad)
           (chomp        .   ,chomp)
           (index        .   ,index)
           (size         .   ,size)
           (clone        .   ,clone)
           (set!         .   ,text-set!)
           (split        .   ,split)
           (match        .   ,match)
           (capture      .   ,capture)
           (replace      .   ,replace)
           (default      .   ,text-default)))))




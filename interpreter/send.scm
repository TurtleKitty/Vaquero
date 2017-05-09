
(define vaquero-universal-messages '(type view messages autos answers? to-bool to-text))

(define (vaquero-send obj msg cont err)
   (define obj-type (vaquero-type obj))
   (define handler (htr vaquero-send-vtable obj-type))
   (handler obj msg cont err))

(define (vaquero-send-atomic obj msg)
    (vaquero-send obj msg top-cont top-err))

(define (vaquero-answerer msgs)
   (define msgs+ (append msgs vaquero-universal-messages))
   (lambda (msg)
      (if (member msg msgs+) #t #f)))

(define (vaquero-send-symbol obj msg cont err)
    (define msgs '(view to-text to-bool))
    (case msg
        ((autos) (cont '(view to-bool to-text)))
        ((view) (cont obj))
        ((to-symbol)
            (cont
                (if (keyword? obj)
                    (keyword->symbol obj)
                    obj)))
        ((to-text) (cont (symbol->string obj)))
        (else
            (case obj
                ((true)  (vaquero-send-bool #t msg cont err))
                ((false) (vaquero-send-bool #f msg cont err))
                ((null)  (vaquero-send-null obj msg cont err))
                (else
                    (case msg
                        ((type) (cont 'symbol))
                        ((to-bool) (cont #t))
                        ((messages) (cont msgs))
                        ((answers?) (cont (vaquero-answerer msgs)))
                        (else (idk obj msg cont err))))))))

(define (vaquero-send-bool obj msg cont err)
    (define msgs '(view to-text to-bool to-symbol not))
    (case msg
        ((type) (cont 'bool))
        ((autos) (cont '(view to-bool to-text to-symbol)))
        ((to-bool) (cont obj))
        ((view to-symbol) (cont (if obj 'true 'false)))
        ((to-text) (cont (if obj "true" "false")))
        ((not) (cont (not obj)))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        (else (idk obj msg cont err))))

(define (vaquero-send-null obj msg cont err)
    (define msgs '(view to-text to-bool to-symbol))
    (case msg
        ((to-bool) (cont #f))
        ((apply) (err (vaquero-error-object 'null-is-not-applicable '(null ...) "Null can not be used as a procedure.") cont))
        ((messages) (cont msgs))
        ((answers?) (cont (lambda (msg) #t)))
        (else (cont 'null))))

(define (vaquero-send-number obj msg cont err)
    (case msg
        ((zero?) (cont (= obj 0)))
        ((pos?) (cont (> obj 0)))
        ((neg?) (cont (< obj 0)))
        ((abs) (cont (abs obj)))
        ((to-bool) (cont (not (= obj 0))))
        ((to-text) (cont (number->string obj)))
        ((view to-number) (cont obj))
        (else
            (if (integer? obj)
               (vaquero-send-int obj msg cont err)
               (vaquero-send-real obj msg cont err)))))

(define (vaquero-send-int obj msg cont err)
    (define msgs '(view to-text to-bool zero? pos? neg? abs floor ceil round truncate inc dec even? odd?))
    (case msg
        ((type) (cont 'int))
        ((autos) (cont '(view to-bool to-text zero? pos? neg? abs floor ceil round truncate inc dec even? odd?)))
        ((inc) (cont (+ obj 1)))
        ((dec) (cont (- obj 1)))
        ((even?) (cont (even? obj)))
        ((odd?) (cont (odd? obj)))
        ((floor) (cont obj))
        ((ceil) (cont obj))
        ((round) (cont obj))
        ((truncate) (cont obj))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        (else (idk obj msg cont err))))
 
(define (vaquero-send-real obj msg cont err)
    (define msgs '(view to-text to-bool zero? pos? neg? abs floor ceil round truncate))
    (case msg
        ((type) (cont 'number))
        ((floor) (cont (inexact->exact (floor obj))))
        ((ceil) (cont (inexact->exact (ceiling obj))))
        ((round) (cont (inexact->exact (round obj))))
        ((truncate) (cont (inexact->exact (truncate obj))))
        ((autos) (cont '(view to-bool to-text zero? pos? neg? abs floor ceil round truncate)))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        (else (idk obj msg cont err))))

(define (vaquero-send-rune obj msg cont err)
    (define msgs '(view code to-rune to-text to-bool to-number alpha? digit? whitespace? uc? lc? uc lc))
    (case msg
        ((type) (cont 'rune))
        ((autos) (cont '(view code to-bool to-rune to-text to-number alpha? digit? whitespace? uc? lc? uc lc)))
        ((view)
            (cont
                (vector 'rune 
                    (case obj
                        ((#\space) "space")
                        ((#\newline) "lf")
                        ((#\return) "cr")
                        ((#\tab) "tab")
                        (else (string obj))))))
        ((code) (cont (char->integer obj)))
        ((alpha?) (cont (char-alphabetic? obj)))
        ((digit?) (cont (char-numeric? obj)))
        ((whitespace?) (cont (char-whitespace? obj)))
        ((uc?) (cont (char-upper-case? obj)))
        ((lc?) (cont (char-lower-case? obj)))
        ((uc) (cont (char-upcase obj)))
        ((lc) (cont (char-downcase obj)))
        ((to-bool) (cont #t))
        ((to-number) (cont (string->number (string obj))))
        ((to-text) (cont (string obj)))
        ((to-rune) (cont obj))
        ((messages) (cont msgs))
        ((answers?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        (else (idk obj msg cont err))))

(define (vaquero-send-text obj msg cont err)
    (define msgs
        '(view clone to-bool to-symbol to-keyword to-number
          to-list to-text to-stream size chomp index take drop
          trim ltrim rtrim lpad rpad set! split match capture replace
          alphabetic? numeric? whitespace? uc? lc? uc lc))
    (define (build-regex re flags)
        (define opts
            (append
                (list re 'fast 'utf8)
                (filter
                    (lambda (x) (not (eq? x 'g)))
                    (map string->symbol (string-split flags "")))))
        (apply irregex opts))
    (define me-answers? (vaquero-answerer msgs))
    (case msg
        ((type view autos clone to-bool to-symbol to-keyword to-number to-list to-text to-stream size chomp index alphabetic? numeric? whitespace? uc? lc? uc lc take drop trim ltrim rtrim lpad rpad)
            (cont
                (case msg
                    ((type) 'text)
                    ((view) obj)
                    ((autos) '(view to-bool to-symbol to-text to-keyword to-number to-list to-stream size chomp ltrim rtrim trim alphabetic? numeric? whitespace? uc? lc? uc lc))
                    ((clone) (string-copy obj))
                    ((to-bool) (not (eq? (string-length obj) 0)))
                    ((to-symbol) (string->symbol obj))
                    ((to-keyword) (string->keyword obj))
                    ((to-number) (string->number obj))
                    ((to-list) (string->list obj))
                    ((to-vector) (list->vector (string->list obj)))
                    ((to-text) obj)
                    ((to-stream) (open-input-string obj))
                    ((alphabetic?) (cont (every char-alphabetic? (string->list obj))))
                    ((numeric?)    (cont (if (string->number obj) #t #f)))
                    ((whitespace?) (cont (every char-whitespace? (string->list obj))))
                    ((uc?)  (cont (every char-upper-case? (string->list obj))))
                    ((lc?)  (cont (every char-lower-case? (string->list obj))))
                    ((uc)   (string-upcase obj))
                    ((lc)   (string-downcase obj))
                    ((take) (lambda (n) (string-take obj n)))
                    ((drop) (lambda (n) (string-drop obj n)))
                    ((trim) (string-trim-both obj))
                    ((ltrim) (string-trim obj))
                    ((rtrim) (string-trim-right obj))
                    ((lpad) (lambda (pad n) (string-pad obj n (string-ref pad 0))))
                    ((rpad) (lambda (pad n) (string-pad-right obj n (string-ref pad 0))))
                    ((chomp) (string-chomp obj))
                    ((index) (lambda (which) (substring-index which obj)))
                    ((size) (string-length obj))
                    ((messages) msgs)
                    ((answers?)
                        (lambda (msg)
                            (or
                                (and (number? msg) (> (string-length obj) msg))
                                (me-answers? msg)))))))
        ((split)
            (cont
                (vaquero-proc
                    primitive-type
                    'text
                    (lambda (args opts cont err)
                        (define flags (vaquero-send-atomic opts 'flags))
                        (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                        (cont (irregex-split re obj))))))
        ((match)
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
        ((capture)
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
        ((replace)
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
        ((set!)
            (cont 
                (lambda (idx val)
                    (if (not (number? idx))
                        (err (vaquero-error-object 'not-a-number `(,obj ,idx) "text: set! requires a number as its first argument.") cont)
                        (if (> idx (string-length obj))
                            (err (vaquero-error-object 'out-of-bounds `(,obj ,idx) "text: index out of bounds.") cont)
                            (begin
                                (string-set! obj idx (string-ref val 0))
                                obj))))))
        (else
            (if (number? msg)
                (if (> (string-length obj) msg)
                    (cont (string (string-ref obj msg)))
                    (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "text: index out of bounds.") cont))
                (idk obj msg cont err)))))

(define (vaquero-ho code obj cont err)
    (vaquero-apply
        (vaquero-compile-method code)
        (list obj)
        'null
        cont
        err))

(define (vaquero-send-empty obj msg cont err)
    (case msg
        ((type empty? autos view to-bool to-text to-list head tail key val car cdr size)
            (cont
                (case msg
                    ((type) 'list)
                    ((autos) '(view empty? to-bool to-text to-list head tail key val size))
                    ((empty?) #t)
                    ((to-bool) #f)
                    ((view to-list) '())
                    ((to-text) "()")
                    ((head tail key val car cdr) 'null)
                    ((size) 0))))
        (else (vaquero-send-list obj msg cont err))))

(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

(define (vaquero-send-list obj msg cont err)
    (define msgs
        '(empty? to-list to-vector to-table head key car head! tail val cdr tail! set! cons
          size reverse has? append take drop apply fold reduce each map filter sort))
    (define (ldefault msg)
        (if (number? msg)
            (if (> (length obj) msg)
                (cont (list-ref obj msg))
                (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "list: index out of bounds.") cont))
            (idk obj msg cont err)))
    (case msg
        ((empty? type view to-bool to-list to-text to-vector head key car tail val cdr head! tail! set! cons size reverse has? append take drop apply messages answers?)
            (cont
                (case msg
                    ((type) 'list)
                    ((empty?) #f)
                    ((autos) '(view empty? to-bool to-text to-list to-vector to-table head tail key val size reverse))
                    ((view) (vaquero-view obj))
                    ((to-text) (apply string obj))
                    ((to-bool) #t)
                    ((to-list) obj)
                    ((to-vector) (list->vector obj))
                    ((head key car) (car obj))
                    ((tail val cdr) (cdr obj))
                    ((head!) (lambda (v)   (set-car! obj v) v))
                    ((tail!) (lambda (v)   (set-cdr! obj v) v))
                    ((set!)
                        (lambda (i v)
                           (if (> (length obj) i)
                               (begin (list-set! obj i v) v)
                               (err (vaquero-error-object 'out-of-bounds `(,obj ,i) "list: index out of bounds.") cont))))
                    ((cons) (lambda (v) (cons v obj)))
                    ((size) (length obj))
                    ((clone) (list-copy obj))
                    ((reverse) (reverse obj))
                    ((has?)
                        (lambda (item)
                            (if (member item obj)
                                #t
                                #f)))
                    ((append) (lambda (other) (append obj other)))
                    ((take) (lambda (n) (take obj n)))
                    ((drop) (lambda (n) (drop obj n)))
                    ((messages) msgs)
                    ((answers?)
                        (lambda (msg)
                            (or
                                (and (number? msg) (> (length obj) msg))
                                ((vaquero-answerer msgs) msg))))
                    ((apply)
                        (vaquero-proc
                            primitive-type
                            'pair
                            (lambda (args opts cont err)
                                (if (pair? (car args))
                                    (vaquero-send-list obj (caar args) cont err)
                                    (err (vaquero-error-object 'bad-message! `(,obj ,args ,opts) "Message not understood.") cont))))))))
        ((to-table)
            (if (not (every pair? obj))
                (err (vaquero-error-object 'not-an-associative-list! `(send ,obj to-table) "list: to-table only works on associative lists." ) cont)
                (let ((r (vaquero-table)))
                    (define vars (htr r 'vars))
                    (for-each (lambda (p) (hts! vars (car p) (cdr p))) obj)
                    (cont r))))
        ((fold)
            (vaquero-ho
                '(lambda (xs)
                    (lambda (acc funk)
                        (if xs.empty?
                            acc
                            (xs.tail.fold (funk acc xs.head) funk))))
                obj
                cont
                err))
        ((reduce)
            (vaquero-ho
                '(lambda (xs)
                    (lambda (acc funk)
                        (if xs.empty?
                            acc
                            (funk xs.head (xs.tail.reduce acc funk)))))
                obj
                cont
                err))
        ((each)
            (vaquero-ho
                '(lambda (xs)
                    (lambda (funk)
                        (if xs.empty?
                            null
                            (seq
                                (funk xs.head)
                                (xs.tail.each funk)))))
                obj
                cont
                err))
        ((map)
            (vaquero-ho
                '(lambda (xs)
                    (lambda (funk)
                        (xs.reduce '() (lambda (x y) (pair (funk x) y)))))
                obj
                cont
                err))
        ((filter)
            (vaquero-ho
                '(lambda (xs)
                    (lambda (funk)
                        (xs.reduce '() (lambda (x y) (if (funk x) (pair x y) y)))))
                obj
                cont
                err))
        ((sort)
            (vaquero-ho
                '(lambda (xs)
                    (lambda (funk)
                        (def merge (lambda (a b)
                            (if a.size.zero?
                                b
                                (if b.size.zero?
                                    a
                                    (if (funk a.head b.head)
                                        (pair a.0 (merge a.tail b))
                                        (pair b.0 (merge a b.tail)))))))
                        (def sort (lambda (yarr)
                            (def len yarr.size)
                            (if (< len 2)
                                yarr
                                (seq
                                    (def half (send (/ len 2) 'floor))
                                    (merge (sort (yarr.take half)) (sort (yarr.drop half)))))))
                        (sort xs)))
                obj
                cont
                err))
        (else (ldefault msg))))

(define (vaquero-send-pair obj msg cont err)
    (define msgs
        '(empty? view to-text to-bool to-list to-table head key car tail val cdr head! tail! cons size clone))
    (define msgs+ (append msgs '(messages answers? type)))
    (if (member msg msgs+)
        (cont 
            (case msg
                ((type) 'pair)
                ((view to-text) (vaquero-view obj))
                ((autos) '(view empty? to-bool to-text to-list to-table head tail key val size))
                ((to-bool) #t)
                ((to-list) (list (car obj) (cdr obj)))
                ((to-table) (vaquero-table (car obj) (cdr obj)))
                ((head key car) (car obj))
                ((tail val cdr) (cdr obj))
                ((head!) (lambda (v) (set-car! obj v) v))
                ((tail!) (lambda (v) (set-cdr! obj v) v))
                ((cons) (lambda (v) (cons v obj)))
                ((size) 2)
                ((clone) (cons (car obj) (cdr obj)))
                ((messages) msgs)
                ((answers?) (vaquero-answerer msgs))))
        (idk obj msg cont err)))

(define (vaquero-send-primitive obj msg cont err)
    (define msgs '(view code to-bool to-text env arity apply))
    (define msgs+ (append msgs '(messages answers? type autos)))
    (if (member msg msgs+)
        (cont 
            (case msg
                ((type) 'proc)
                ((view) primitive-type)
                ((code) '0xDEADBEEF)
                ((to-bool) #t)
                ((to-text) "0xDEADBEEF")
                ((autos) '(view code to-bool to-text env arity))
                ((env) 'global)
                ((arity)
                    (let ((pinfo (procedure-information obj)))
                        (if (list? pinfo)
                            (sub1 (length pinfo))
                            '*)))
                ((messages) msgs)
                ((answers?) (vaquero-answerer msgs))
                ((apply)
                    (lambda (args opts)
                        (apply obj args)))))
        (idk obj msg cont err)))

(define (vaquero-send-table obj msg cont err)
    (define msgs
        '(view size clone to-bool get put set! rm del! has? apply keys values pairs to-list to-opt to-text merge fold reduce map filter))
    (define vars (htr obj 'vars))
    (define (rdefault msg)
        (if (hte? vars msg)
            (htr vars msg)
            'null))
    (case msg
        ((type view size autos clone to-bool get put set! rm del! has? apply keys values pairs to-list to-opt to-text merge messages answers?)
            (cont
                (case msg
                    ((type) 'table)
                    ((view to-text)
                         (vaquero-view obj))
                    ((size)
                         (hash-table-size vars))
                    ((autos)
                         '(view size clone to-bool to-list to-text keys values pairs))
                    ((clone)
                        (let ((noob (vaquero-table)))
                            (hts! noob 'vars (hash-table-copy vars))
                            noob))
                    ((to-bool)
                        (> (hash-table-size vars) 0))
                    ((get)
                        (lambda (k)
                            (if (hte? vars k)
                                (htr vars k)
                                'null)))
                    ((put)
                        (lambda args
                            (define noob (vaquero-table))
                            (hts! noob 'vars (hash-table-copy vars))
                            (vaquero-send-table
                                noob
                                'set!
                                (lambda (setter!)
                                    (apply setter! args)
                                    noob)
                                err)))
                    ((set!)
                        (lambda args
                            (for-pairs (lambda (k v) (hts! vars k v)) args)
                            'null))
                    ((rm)
                        (lambda args
                            (define noob (vaquero-table))
                            (hts! noob 'vars (hash-table-copy vars))
                            (vaquero-send-table
                                noob
                                'del!
                                (lambda (deleter!)
                                    (apply deleter! args)
                                    noob)
                                err)))
                    ((del!)
                        (lambda args
                            (map (lambda (k) (htd! vars k)) args)
                            'null))
                    ((has?)
                        (lambda (x)
                            (hte? vars x)))
                    ((apply)
                        (vaquero-proc
                            primitive-type
                            'table
                            (lambda (args opts cont err)
                                (vaquero-send-table obj (caar args) cont err))))
                    ((keys) (htks vars))
                    ((values) (htvs vars))
                    ((pairs to-list) (hash-table->alist vars))
                    ((to-opt)
                        (fold
                            (lambda (p xs)
                                (cons (symbol->keyword (car p)) (cons (cdr p) xs)))
                            '()
                            (hash-table->alist vars)))
                    ((messages) msgs)
                    ((answers?)
                        (lambda (msg)
                            (or 
                                (hte? vars msg)
                                ((vaquero-answerer msgs) msg))))
                    ((merge)
                        (lambda (other)
                            (define nuvars (hash-table-merge (htr other 'vars) vars))
                            (define noob (mkht))
                            (hts! noob 'type 'table)
                            (hts! noob 'vars nuvars)
                            noob)))))
            ((fold) (vaquero-send-list
                        (hash-table->alist vars)
                        'fold
                        cont
                        err))
            ((reduce) (vaquero-send-list
                        (hash-table->alist vars)
                        'reduce
                        cont
                        err))
            ((map)
                (vaquero-ho
                    '(lambda (rec)
                        (lambda (funk)
                            (def mapped (rec.to-list.map funk))
                            mapped.to-table))
                    obj
                    cont
                    err))
            ((filter) 
                (vaquero-ho
                    '(lambda (rec)
                        (lambda (funk)
                            (def mapped (rec.to-list.filter funk))
                            mapped.to-table))
                    obj
                    cont
                    err))
            (else (cont (rdefault msg)))))

(define (vaquero-send-object obj msg cont err)
   (define fields  (htr obj 'fields))
   (define resends (htr obj 'resends))
   (define autos   (htr obj 'autos))
   (define (get-msgs)
      (append (hash-table-keys fields) (hash-table-keys resends)))
   (define (vaquero-object-view obj)
      (define type
         (if (hte? fields 'type)
            (htr fields 'type)
            'object))
      (define the-view
         (if (hte? fields 'view)
            (let ((view-proc (htr fields 'view)))
               (if (member 'apply (vaquero-send-atomic view-proc 'messages))
                  (vaquero-apply view-proc '() 'null identity err)
                  (err (vaquero-error-object 'view-must-answer-apply `(send ,obj 'view) "The object answered 'view with a non-applicative."))))
            (get-msgs)))
      (apply vector (cons type the-view)))
   (case msg
      ; unshadowable reflection messages
      ((answers?)  (cont (vaquero-answerer (get-msgs))))
      ((messages)  (cont (get-msgs)))
      ((autos)     (cont (hash-table-keys autos)))
      ((view)      (cont (vaquero-object-view obj)))
      (else
         (if (hte? fields msg)
            (let ((v (htr fields msg)))
               (if (hte? autos msg)
                  (vaquero-apply v '() 'null cont err) ; exec the thunk
                  (cont v)))
            (if (hte? resends msg)
               (vaquero-send (htr resends msg) msg cont err)
               (case msg
                  ((type) (cont 'object))
                  ((to-text) (cont "object"))
                  ((to-bool) (cont (not (eq? 0 (length (hash-table-keys fields))))))
                  (else (vaquero-apply (htr obj 'default) (list msg) 'null cont err))))))))

(define (vaquero-send-proc obj msg cont err)
    (define msgs '(type view to-bool to-text arity code env formals apply))
    (case msg
        ((type) (cont (htr obj 'type)))
        ((view) (cont `(,(htr obj 'type) ,(htr obj 'formals) ...)))
        ((to-bool) (cont #t))
        ((to-text) (cont (htr obj 'code)))
        ((arity code env formals) (cont (htr obj msg)))
        ((apply)
            (cont 
                (vaquero-proc
                    primitive-type
                    'proc
                    (lambda (args opts cont err)
                        (if (< (length args) 2)
                            (err (vaquero-error-object 'arity `((send ,obj apply) ,args) "proc.apply requires 2 arguments!") cont)
                            (vaquero-apply obj (car args) (cadr args) cont err))))))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        ((autos) (cont '(view to-bool to-text arity code env formals)))
        (else (idk obj msg cont err))))

(define (vaquero-send-env obj msg cont err)
    (define msgs '(view to-text def! merge! has? get pairs lookup parent extend expand eval load))
    (define undefineds (list not-found will-exist 'null))
    (define vars (htr obj 'vars))
    (define (env-default msg)
        (lookup obj msg
            (lambda (val)
               (if (eq? val not-found)
                  (err (vaquero-error-object 'message-not-understood `(send ,obj ,msg) "Message not understood.") cont)
                  val))
            err))
    (case msg
        ((get has? to-bool keys values pairs)
            (vaquero-send-table vars msg cont err))
        ((type) (cont 'env))
        ((view to-text)
            (vaquero-view obj))
        ((autos) (cont '(view to-text to-bool keys values pairs)))
        ((def!)
            (cont
                (vaquero-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (define getter (vaquero-send-table vars 'get  top-cont err))
                        (define setter (vaquero-send-table vars 'set! top-cont err))
                        (if (null? args)
                           (cont 'null)
                           (let loop ((def-name (car args)) (def-val (cadr args)) (the-rest (cddr args)))
                              (define current (getter def-name))
                              (if (member current undefineds)
                                 (begin
                                    (setter def-name def-val)
                                    (if (null? the-rest)
                                       (cont def-val)
                                       (loop (car the-rest) (cadr the-rest) (cddr the-rest))))
                                 (err (vaquero-error-object 'name-already-defined `(def ,def-name ,def-val) "env: name is already defined.") cont))))))))
        ((merge!)
            (cont
               (vaquero-proc
                  primitive-type
                  'env
                  (lambda (args opts cont err)
                     (define (arg-fail form)
                        (err (vaquero-error-object 'argument-fail form "env: merge! requires an environment as an argument.") cont))
                     (if (not (pair? args))
                        (arg-fail '(env.merge!))
                        (let ((other-env (car args)))
                           (if (not (eq? 'env (vaquero-send-atomic other-env 'type)))
                              (arg-fail '(env.merge! WAT))
                              (let ((def-this (vaquero-send-atomic obj 'def!))
                                    (other-vars (hash-table->alist (htr (htr other-env 'vars) 'vars))))
                                 (if (not (pair? other-vars))
                                    (cont obj)
                                    (let loop ((v (car other-vars)) (vs (cdr other-vars)) (flat '()))
                                       (define nu-flat (cons (car v) (cons (cdr v) flat)))
                                       (if (pair? vs)
                                          (loop (car vs) (cdr vs) nu-flat)
                                          (vaquero-apply def-this nu-flat 'null (lambda (x) (cont obj)) err))))))))))))
        ((lookup)
            (cont
                (vaquero-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (lookup
                            obj
                            (car args)
                            (lambda (val)
                                (cont
                                    (if (eq? val not-found)
                                        'null
                                        val)))
                            err)))))
        ((extend)
            (cont
                (vaquero-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (let loop ((names '()) (vals '()) (left args))
                            (if (eq? '() left)
                                (extend obj names vals cont err)
                                (loop (cons (car left) names) (cons (cadr left) vals) (cddr left))))))))
        ((parent)
            (cont (htr obj 'parent)))
        ((eval)
            (cont
                (lambda (code)
                    (vaquero-eval code obj))))
        ((expand)
            (cont
                (lambda (code)
                    (vaquero-expand code obj))))
        ((load)
            (cont
               (lambda (stream)
                   (vaquero-eval (vaquero-read-file stream) obj))))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        (else (cont (env-default msg)))))

(define (vaquero-send-vector obj msg cont err)
    (define msgs '(view to-bool to-text to-list pairs size clone has? get set! apply fold reduce map filter sort))
    (define (vdefault msg)
        (if (number? msg)
            (if (> (vector-length obj) msg)
                (cont (vector-ref obj msg))
                (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont))
            (idk obj msg cont err)))
    (case msg
        ((type view autos to-bool to-text to-list pairs size clone has? get set! apply messages answers?)
            (cont 
                (case msg
                    ((type) 'vector)
                    ((view) (vaquero-view obj))
                    ((to-bool) (not (eq? (vector-length obj) 0)))
                    ((to-list) (vector->list obj))
                    ((to-text) (apply string (vector->list obj)))
                    ((autos) '(view to-text to-bool to-list size pairs clone))
                    ((pairs) (vector->list (vector-map (lambda (i x) (cons i x)) obj)))
                    ((size) (vector-length obj))
                    ((clone) (vector-copy obj))
                    ((has?)
                        (lambda (item)
                            (if (vector-index
                                    (lambda (x) (eq? x item))
                                    obj)
                                #t
                                #f)))
                    ((get)
                        (lambda (idx)
                            (if (not (number? idx))
                                (err (vaquero-error-object 'not-a-number `(,obj ,idx) "vector: get requires a number as its argument.") cont)
                                (if (> idx (vector-length obj))
                                    (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont)
                                    (vector-ref obj idx)))))
                    ((set!)
                        (lambda (idx val)
                            (if (not (number? idx))
                                (err (vaquero-error-object 'not-a-number `(,obj ,idx) "vector: set! requires a number as its first argument.") cont)
                                (if (> idx (vector-length obj))
                                    (err (vaquero-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont)
                                    (begin
                                        (vector-set! obj idx val)
                                        obj)))))
                    ((messages) msgs)
                    ((answers?)
                        (lambda (msg)
                            (or
                                (and (number? msg) (> (vector-length obj) msg))
                                ((vaquero-answers msgs) msg))))
                    ((apply)
                        (vaquero-proc
                            primitive-type
                            'pair
                            (lambda (args opts cont err)
                                (vaquero-send-vector obj (caar args) cont err)))))))
        ((fold)
            (vaquero-ho
                '(lambda (vec)
                    (lambda (acc funk)
                        (vec.to-list.fold acc funk)))
                obj
                cont
                err))
        ((reduce)
            (vaquero-ho
                '(lambda (vec)
                    (lambda (acc funk)
                        (vec.to-list.reduce acc funk)))
                obj
                cont
                err))
        ((map)
            (vaquero-ho
                '(lambda (vec)
                    (lambda (funk)
                        (def mapped (vec.to-list.map funk))
                        mapped.to-vector))
                obj
                cont
                err))
        ((filter)
            (vaquero-ho
                '(lambda (vec)
                    (lambda (funk)
                        (def mapped (vec.to-list.filter funk))
                        mapped.to-vector))
                obj
                cont
                err))
        ((sort)
            (vaquero-ho
                '(lambda (vec)
                    (lambda (funk)
                        (def sorted (vec.to-list.sort funk))
                        sorted.to-vector))
                obj
                cont
                err))
        (else
            (vdefault msg))))

(define (vaquero-send-stream obj msg cont err)
    (case msg
        ((type view to-bool to-stream input? output? open?)
            (cont 
                (case msg
                    ((type) 'stream)
                    ((view to-text) obj)
                    ((to-bool) #t)
                    ((to-stream) obj)
                    ((input?) (input-port? obj))
                    ((output?) (output-port? obj))
                    ((open?) (not (port-closed? obj))))))
        (else
            (if (input-port? obj)
                (vaquero-send-input-stream obj msg cont err) 
                (vaquero-send-output-stream obj msg cont err)))))

(define (vaquero-send-input-stream obj msg cont err)
    (define msgs
        '(view to-bool input? output? open? close
          ready? read read-rune peek-rune read-line read-text read-lines to-text to-list to-bool
          read-n read-while read-until skip-n skip-while skip-until))
    (case msg
        ((ready? autos read read-seq read-rune peek-rune read-line read-text read-lines
          read-n read-while read-until skip-n skip-while skip-until to-text to-list to-bool
          messages answers?)
            (if (port-closed? obj)
                (err (vaquero-error-object 'input-stream-closed `(send ,obj ,msg) "Input stream closed.") cont)
                (cont 
                    (case msg
                        ((autos)    '(view to-text to-bool to-list ready? input? output? open? read read-seq read-rune peek-rune read-line read-lines read-text))
                        ((ready? to-bool) (char-ready? obj))
                        ((read)      (vaquero-read obj))
                        ((read-seq)  (vaquero-read-file obj))
                        ((read-rune) (string (read-char obj)))
                        ((peek-rune) (string (peek-char obj)))
                        ((read-line) (read-line obj))
                        ((read-lines to-list) (read-lines obj))
                        ((read-text to-text)  (read-string #f obj))
                        ((read-n)
                            (lambda (n)
                                (read-string n obj)))
                        ((read-while)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)) (acc '()))
                                    (if (member tok runes)
                                        (let ((t (read-char obj)))
                                            (loop (peek-char obj) (cons t acc)))
                                        (list->string (reverse acc))))))
                        ((read-until)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)) (acc '()))
                                    (if (member tok runes)
                                        (list->string (reverse acc))
                                        (let ((t (read-char obj)))
                                            (loop (peek-char obj) (cons t acc)))))))
                        ((skip-n)
                            (lambda (n)
                                (read-string n obj)
                                'null))
                        ((skip-while)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)))
                                    (if (member tok runes)
                                        (begin
                                            (read-char obj)
                                            (loop (peek-char obj)))
                                        'null))))
                        ((skip-until)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)))
                                    (if (member tok runes)
                                        'null
                                        (begin
                                            (read-char obj)
                                            (loop (peek-char obj)))))))
                        ((messages) msgs)
                        ((answers?) (vaquero-answerer msgs))))))
        ((close) (close-input-port obj) (cont 'null))
        (else (idk msg obj cont err))))

(define (vaquero-send-output-stream obj msg cont err)
    (define msgs
        '(input? output? open? write print say nl flush close))
    (case msg
        ((write print say nl autos)
            (if (port-closed? obj)
                (err (vaquero-error-object 'output-stream-closed `(send ,obj ,msg) "Output stream closed.") cont)
                (cont
                    (case msg
                        ((autos) '(view to-bool ready? input? output? open? nl close)) 
                        ((write)
                            (lambda (x)
                                (vaquero-write x obj)
                                'null))
                        ((print)
                            (lambda (x)
                                (vaquero-print x obj)
                                'null))
                        ((say)
                            (lambda (x)
                                (vaquero-print x obj)
                                (newline obj)
                                'null))
                        ((nl) (newline obj) 'null)))))
        ((messages) (cont msgs))
        ((answers?) (cont (vaquero-answerer msgs)))
        ((flush) (flush-output obj) (cont 'null))
        ((close) (close-output-port obj) (cont 'null))
        (else (idk msg obj cont err))))

(define (vaquero-send-eof obj msg cont err)
   (define msgs '())
   (case msg
      ((type) (cont 'EOF))
      ((view) (cont 'EOF))
      ((to-bool) (cont #f))
      ((to-text) (cont "END OF LINE."))
      ((autos) '(view to-text to-bool))
      ((apply) (err (vaquero-error-object 'eof-is-not-applicable '(EOF ...) "EOF objects can not be used as procedures.") cont))
      ((messages) (cont msgs))
      ((answers?) (cont (vaquero-answerer msgs)))
      (else (idk msg obj cont err))))

(define (vaquero-send-wtf obj msg cont err)
   (err (vaquero-error-object 'wtf-was-that? `(send ,obj ,msg) "Unknown object!")))

(define vaquero-send-vtable
   (alist->hash-table
      `((bool       . ,vaquero-send-bool)
        (symbol     . ,vaquero-send-symbol)
        (number     . ,vaquero-send-number)
        (rune       . ,vaquero-send-rune)
        (text       . ,vaquero-send-text)
        (empty      . ,vaquero-send-empty)
        (list       . ,vaquero-send-list)
        (pair       . ,vaquero-send-pair)
        (primitive  . ,vaquero-send-primitive)
        (vector     . ,vaquero-send-vector)
        (stream     . ,vaquero-send-stream)
        (env        . ,vaquero-send-env)
        (table      . ,vaquero-send-table)
        (proc       . ,vaquero-send-proc)
        (object     . ,vaquero-send-object)
        (eof        . ,vaquero-send-eof)
        (WTF        . ,vaquero-send-wtf))))


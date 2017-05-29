
(define mkht make-hash-table)
(define htr hash-table-ref)
(define htks hash-table-keys)
(define htvs hash-table-values)
(define hte? hash-table-exists?)
(define hts! hash-table-set!)
(define htd! hash-table-delete!)

(define primitive-type 'primitive-procedure)
(define not-found 'vaquero-internal-this-name-was-not-found)
(define will-exist 'vaquero-internal-this-name-is-about-to-be-defined)

(define (idk obj msg cont err)
   (err (vaquero-error-object 'message-not-understood `(send ,obj ,msg) "Message not understood.") cont))

(define (p x . xs)
   (write (cons x xs))
   (newline))

(define (debug x . xs)
    (define e (current-error-port))
    (if (null? xs)
        (begin (vaquero-write x e) (newline e))
        (begin (vaquero-write (cons x xs) e) (newline e))))

(define (debug-obj x)
    (define ps (vaquero-view x))
    (map debug ps))

(define (for-pairs fn args)
    (if (not (eq? (modulo (length args) 2) 0))
        (error (list "for-pairs requires an even number of arguments!" args))
        (let loop ((newlist '()) (pairs args))
            (if (atom? pairs)
                newlist
                (let ((key (first pairs)) (val (second pairs)))
                    (loop (fn key val) (cddr pairs)))))))

(define (vaquero-error name form message)
   (display "ERROR: ") (display message) (newline)
   (abort (list name form message)))

(define (vaquero-error-object name form to-text)
    (vaquero-object `(type error name ,name form ,form to-text ,to-text message ,to-text view (error ,name ,form ,to-text)) #f #f #f))

(define (vaquero-bool obj cont err)
    (vaquero-send obj 'to-bool cont err))

(define (vaquero-view obj)
   (define visited '())
   (define (helper obj)
      (define obj-type (vaquero-type obj))
      (if (member obj-type '(pair list vector table env))
         (let ((seen (member obj visited eq?)))
            (if seen
               'cyclical-reference
               (begin 
                  (set! visited (cons obj visited))
                  (case obj-type
                     ((pair)
                        (cons (helper (car obj)) (helper (cdr obj))))
                     ((list)
                        (map helper obj))
                     ((vector)
                        (let* ((size (vector-length obj)) (noob (make-vector (+ size 1))))
                           (vector-set! noob 0 'vector)
                           (let loop ((i 0))
                              (if (= i size)
                                 noob
                                 (begin
                                    (vector-set! noob (+ i 1) (helper (vector-ref obj i)))
                                    (loop (+ i 1)))))))
                     ((table)
                        (apply vector (cons 'table
                           (fold
                              (lambda (p xs)
                                 (cons (helper (car p)) (cons (helper (cdr p)) xs)))
                              '()
                              (hash-table->alist (htr obj 'vars))))))
                     ((env)
                        (apply vector (cons 'env
                           (fold
                              (lambda (p xs)
                                 (cons (helper (car p)) (cons (helper (cdr p)) xs)))
                              '()
                              (hash-table->alist (htr (htr obj 'vars) 'vars))))))))))
         (vaquero-send-atomic obj 'view)))
   (helper obj))

(define (sort-symbol-alist ps)
    (sort ps
        (lambda (a b)
            (string<? (symbol->string (car a)) (symbol->string (car b))))))

(define (vaquero-bool? x)
    (or (eq? x 'true) (eq? x 'false)))

(define (vaquero-null? x)
    (eq? x 'null))

(define (vaquero-equal? x y)
    (define (no-way)
        (vaquero-error 'bad-= (list '= x y) (list "= cannot compare objects " x " and " y "!"))
        #f)
    (cond
        ((and (number? x) (number? y))
            (= x y))
        ((and (hash-table? x) (hash-table? y))
            (let ((xt (htr x 'type)) (yt (htr y 'type)))
                (if (not (eq? xt yt))
                    #f
                    (case xt
                        ((env lambda proc op) (eq? x y))
                        ((table)
                            (let ((x-pairs (sort-symbol-alist (hash-table->alist (htr x 'vars))))
                                  (y-pairs (sort-symbol-alist (hash-table->alist (htr y 'vars)))))
                                (map vaquero-equal? x-pairs y-pairs)))
                        (else (no-way))))))
        (else
            (equal? x y))))

(define (vaquero-type obj)
    (cond
        ((boolean? obj)   'bool)
        ((symbol? obj)    'symbol)
        ((number? obj)    'number)
        ((string? obj)    'text)
        ((null? obj)      'empty)
        ((list? obj)      'list)
        ((pair? obj)      'pair)
        ((procedure? obj) 'primitive)
        ((vector? obj)    'vector)
        ((port? obj)      'stream)
        ((hash-table? obj)
            (let ((t (htr obj 'type)))
                (case t
                    ((env)            'env)
                    ((table)          'table)
                    ((lambda proc op) 'proc)
                    (else             'object))))
        ((eof-object? obj) 'eof)
        (else 'WTF)))

(define (vaquero-type-ord x)
    (cond
        ((vaquero-bool? x) 1)
        ((boolean? x)   1)
        ((number? x)    2)
        ((char? x)      3)
        ((symbol? x)    4)
        ((string? x)    5)
        (else #f)))

(define (vaquero-< x y)
    (define (no-way)
        (vaquero-error 'bad-< (list '< x y) (list "< cannot compare objects " x " and " y "!"))
        #f)
    (cond
        ((and (number? x) (number? y)) (< x y))
        ((vaquero-null? x) #t)
        ((vaquero-null? y) #f)
        ((and (vaquero-bool? x) (vaquero-bool? y)) (and (eq? x 'false) (eq? y 'true)))
        ((and (vaquero-bool? x) (boolean? y)) (and (eq? x 'false) y))
        ((and (char? x) (char? y) (char<? x y)))
        ((and (symbol? x) (symbol? y)) (string<? (symbol->string x) (symbol->string y)))
        ((and (string? x) (string? y)) (string<? x y))
        ((and (boolean? x) (boolean? y)) (and (not x) y))
        ((and (boolean? x) (vaquero-bool? y)) (and (not x) (eq? y 'true)))
        (else
            (let ((x-ord (vaquero-type-ord x)) (y-ord (vaquero-type-ord y)))
                (if (not (and x-ord y-ord))
                    (no-way)
                    (< x-ord y-ord))))))

(define (vaquero-> x y)
    (define (no-way)
        (vaquero-error 'bad-> (list '> x y) (list "> cannot compare objects " x " and " y "!"))
        #f)
    (cond
        ((and (number? x) (number? y)) (> x y))
        ((vaquero-null? x) #f)
        ((vaquero-null? y) #t)
        ((and (vaquero-bool? x) (vaquero-bool? y)) (and (eq? x 'true) (eq? y 'false)))
        ((and (vaquero-bool? x) (boolean? y)) (and (eq? x 'true) (not y)))
        ((and (char? x) (char? y) (char>? x y)))
        ((and (symbol? x) (symbol? y)) (string>? (symbol->string x) (symbol->string y)))
        ((and (string? x) (string? y)) (string>? x y))
        ((and (boolean? x) (boolean? y)) (and x (not y)))
        ((and (boolean? x) (vaquero-bool? y)) (and x (eq? y 'false)))
        (else
            (let ((x-ord (vaquero-type-ord x)) (y-ord (vaquero-type-ord y)))
                (if (not (and x-ord y-ord))
                    (no-way)
                    (> x-ord y-ord))))))

(define (nodef x)
    (vaquero-error 'undefined-symbol x "Symbol not defined"))

(define (keyword->symbol k)
    (string->symbol (keyword->string k)))

(define (vaquero-compile-method code)
    ((vaquero-compile-lambda (vaquero-parse code)) (local-env) identity identity))

(define blessed
    '(def quote if seq op lambda proc let wall gate capture guard fail env opt rest return))

(define (holy? name)
    (or (member name blessed)
        (let ((x (glookup name)))
            (not
                (or
                    (eq? x not-found)
                    (eq? x will-exist))))))

(define (blasphemy name)
    (string-join
        (list
            "The name \""
            (symbol->string name)
            "\" is sacred.  It cannot be redefined.")
        ""))

(define (vaquero-read-file port)
    (define one (peek-char port))
    (define hash-bang
        (if (eq? one #\#)
            (read-line port)
            #f))
    (define program
        (let loop ((noob (vaquero-read port)) (code '()))
            (if (eof-object? noob)
                (reverse code)
                (loop (vaquero-read port) (cons noob code)))))
    (close-input-port port)
    (cons 'seq program))

(define (vaquero-cli-args xs)
    (define (rval args opts)
        (cons
            (if (and (pair? args) (> (length args) 1))
                (cddr (reverse args))
                '())
            opts))
    (if (pair? xs)
        (let* ((options (vaquero-table))
               (setopt! (vaquero-send-atomic options 'set!)))
            (let loop ((head (car xs)) (tail (cdr xs)) (args '()))
                (if (eq? (string-ref head 0) #\-)
                    (let ((k (string->symbol (irregex-replace/all "^-+" head ""))) (v (car tail)))
                        (setopt! k v)
                        (if (pair? (cdr tail))
                            (loop (cadr tail) (cddr tail) args)
                            (rval args options)))
                    (if (pair? tail)
                        (loop (car tail) (cdr tail) (cons head args))
                        (rval (cons head args) options)))))
        (rval '() (vaquero-table))))

(define (the-end v) (exit))

(define (compile-and-apply-prog program cont err)
   (define compiled-scheme-lambda (vaquero-compile program))
   (define compiled-vaquero-lambda
      (compiled-scheme-lambda (global-env) top-cont top-err))
   (vaquero-apply compiled-vaquero-lambda (list sys) 'null cont err))

(define (vaquero-run program)
   (compile-and-apply-prog program the-end top-err))


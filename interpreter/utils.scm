
(define mkht make-hash-table)
(define htr hash-table-ref)
(define htks hash-table-keys)
(define htvs hash-table-values)
(define hte? hash-table-exists?)
(define hts! hash-table-set!)
(define htd! hash-table-delete!)

(define null            'null)
(define primitive-code  '(proc (xyzzy) 0xDEADBEEF))
(define not-found       'vaquero-internal-this-name-was-not-found)
(define will-exist      'vaquero-internal-this-name-is-about-to-be-defined)

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

(define-syntax method
   (ir-macro-transformer
      (lambda (expr inject compare)
         (let ((name (cadr expr)) (body (cddr expr)))
            `(define (,name ,(inject 'obj) ,(inject 'msg) ,(inject 'cont) ,(inject 'err)) ,@body)))))

(define (vaquero-warning name form message)
   (display (list 'WARNING name form message)) (newline))

(define (vaquero-error name form message)
   (signal
      (condition
         `(exn
            location vaquero-error
            name ,name
            form ,form
            message ,message))))

(define (vaquero-error-object name form to-text)
   (vaquero-object `(type (error) name ,name form ,form to-text ,to-text message ,to-text view ,(lambda () (vector 'error name form to-text))) #f #f #f))

(define (vaquero-error? obj)
   (and (vaquero-object? obj) (equal? (vaquero-send-atomic obj 'type) '(error))))

(define (vaquero-bool obj cont err)
   (vaquero-send obj 'to-bool cont err))

(define (logger x)
   (write x)
   (newline))

(define (vaquero-view obj)
   (define (vector-name name lst)
      (apply vector (cons name lst)))
   (define (helper obj visited)
      (define obj-type (vaquero-type obj))
      (if (member obj-type '(tuple set pair list vector table env))
         (let ((seen (member obj visited eq?)))
            (if seen
               'cyclical-reference
               (let* ((nu-visited (cons obj visited))
                      (help-list (lambda (x) (helper x nu-visited)))
                      (pair->list (lambda (p xs)
                        (cons (helper (car p) nu-visited) (cons (helper (cdr p) nu-visited) xs))))
                      (help-alist (lambda (xs)
                        (fold pair->list '() xs))))
                  (case obj-type
                     ((list)
                        (map help-list obj))
                     ((tuple)
                        (vector-name 'tuple (help-alist (vaq-tuple-fields obj))))
                     ((set)
                        (vector-name 'set (map help-list (htks (vaquero-set-items obj)))))
                     ((pair)
                        (cons (helper (car obj) nu-visited) (helper (cdr obj) nu-visited)))
                     ((vector)
                        (let* ((size (vector-length obj)) (noob (make-vector (+ size 1))))
                           (vector-set! noob 0 'vector)
                           (let loop ((i 0))
                              (if (= i size)
                                 noob
                                 (begin
                                    (vector-set! noob (+ i 1) (helper (vector-ref obj i) nu-visited))
                                    (loop (+ i 1)))))))
                     ((table)
                        (vector-name 'table (help-alist (hash-table->alist obj))))
                     ((env)
                        (vector-name 'env (help-alist (hash-table->alist (vaquero-env-vars obj)))))))))
         (if (eq? obj-type 'WTF)
            '???
            (vaquero-send-atomic obj 'view))))
   (helper obj '()))

(define (sort-symbol-alist ps)
   (sort ps
      (lambda (a b)
         (string<? (symbol->string (car a)) (symbol->string (car b))))))

(define (vaquero-sort-alist ps)
   (sort ps
      (lambda (a b)
         (vaquero-apply vaquero-lt (list (car a) (car b)) 'null top-cont top-err))))

(define (vaquero-bool? x)
   (or (eq? x 'true) (eq? x 'false)))

(define (vaquero-null? x)
   (eq? x 'null))

(define (vaquero-member? x xs)
   (not (eq? #f (member x xs vaquero-equal?))))

(define (make-list-tester xs)
   (lambda (x) (vaquero-member? x xs)))

(define (vaquero-equal? x y)
   (cond
      ((and (number? x) (number? y))
         (= x y))
      ((or (pair? x) (vaquero-tuple? x) (vaquero-set? x) (vector? x) (hash-table? x) (vaquero-object? x))
         (vaquero-apply (vaquero-send-atomic x 'eq?) (list y) 'null top-cont top-err))
      ((or (vaquero-proc? x) (vaquero-env? x))
         (eq? x y))
      (else
         (equal? x y))))

(define (vaquero-type obj)
   (cond
      ((eq? obj 'null)        'null)
      ((eq? obj 'true)        'bool)
      ((eq? obj 'false)       'bool)
      ((boolean? obj)         'bool)
      ((keyword? obj)         'keyword)
      ((symbol? obj)          'symbol)
      ((integer? obj)         'int)
      ((number? obj)          'real)
      ((string? obj)          'text)
      ((list? obj)            'list)
      ((pair? obj)            'pair)
      ((procedure? obj)       'primitive)
      ((vector? obj)          'vector)
      ((input-port? obj)      'source)
      ((output-port? obj)     'sink)
      ((vaquero-proc? obj)    'proc)
      ((vaquero-env? obj)     'env)
      ((vaquero-tuple? obj)   'tuple)
      ((vaquero-set? obj)     'set)
      ((vaquero-object? obj)  'object)
      ((hash-table? obj)      'table)
      ((eof-object? obj)      'eof)
      (else 'WTF)))

(define (vaquero-type-ord x)
   (cond
      ((vaquero-bool? x) 1)
      ((boolean? x)   1)
      ((number? x)   2)
      ((char? x)     3)
      ((symbol? x)   4)
      ((string? x)   5)
      (else #f)))

(define (keyword->symbol k)
   (string->symbol (keyword->string k)))

(define (symbol->keyword s)
   (string->keyword (symbol->string s)))

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

; stolen from https://github.com/dsosby/chicken-uuid/blob/master/uuid.scm

(define uuid-v4-hex-vals (string->list "0123456789abcdef"))
(define uuid-v4-pattern "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx")

(define (uuid-v4 #!optional [randfn pseudo-random-integer])
  (letrec ([x-replace (lambda () (list-ref uuid-v4-hex-vals (randfn 16)))]
      [y-replace (lambda () (list-ref uuid-v4-hex-vals (bitwise-ior (bitwise-and (randfn 16) #x08) #x03)))]
      [map-proc (lambda (c) (cond ((eq? c #\x) (x-replace)) ((eq? c #\y) (y-replace)) (else c)))])
    (string-map map-proc uuid-v4-pattern)))

; /stolen

(define (vaquero-gensym #!optional (name "gensym"))
   (string->symbol
      (string-append name "-" (uuid-v4))))

(define (compile-and-apply-prog program cont err)
   (define compiled-scheme-lambda (vaquero-compile program))
   (define compiled-vaquero-lambda
      (compiled-scheme-lambda (global-env) top-cont top-err))
   (vaquero-apply compiled-vaquero-lambda (list sys) 'null cont err))

(define (vaquero-run program)
   (compile-and-apply-prog program the-end top-err))

(define (vaquero-internal-primitive-proc cont hand opts formals env compiled)
   (vaquero-procedure '(lambda) 'code-missing env compiled formals (length formals)))

(define (vaquero-internal-primitive-apply cont hand opts fn args)
   (vaquero-apply fn args opts cont hand))

(define (vaquero-wrap-user-facing-interpreter funk)
   (lambda rest
      (handle-exceptions exn
         (condition->list exn)
         (apply funk rest))))



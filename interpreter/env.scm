
(define (local-env)
    (vaquero-environment #f))

(define (cli-env)
    (define lenv (local-env))
    (extend lenv
        '(sys)
        (list sys)
        top-cont
        top-err))

(define (global-env)
    (define (make-new)
        (define prelude (local-env))
        (define preset! (vaquero-send-atomic prelude 'def!))
        (define (fill-prelude fs)
            (define (setem! p)
                (preset! (car p) (cdr p)))
            (map setem! fs))
        (define primitives
            (list
                (cons 'global? holy?)
                (cons 'is? eq?)
                (cons '+ +)
                (cons '- -)
                (cons '* *)
                (cons '/ /)
                (cons '= vaquero-equal?)
                (cons '> vaquero->)
                (cons '< vaquero-<)
                (cons 'div quotient)
                (cons 'rem remainder)
                (cons 'mod modulo)
                (cons 'num? number?)
                (cons 'int? integer?)
                (cons 'nint?
                    (lambda (x)
                        (and (number? x) (not (integer? x)))))
                (cons 'rune? char?)
                (cons 'pair cons)
                (cons 'pair? pair?)
                (cons 'list list)
                (cons 'list? list?)
                (cons 'option? keyword?)
                (cons 'syntax-ok? (lambda (form) (check-vaquero-syntax (list form))))
                (cons 'vector
                    (vaquero-proc
                        primitive-type
                        'global
                        (lambda (args opts cont err)
                            (define size ((vaquero-send-atomic opts 'get) 'size))
                            (define init ((vaquero-send-atomic opts 'get) 'init))
                            (cont
                                (if (integer? size)
                                    (let ((v (make-vector size init)))
                                        (vector-map (lambda (i x) (vector-set! v i x)) (list->vector args))
                                        v)
                                    (apply vector args))))))
                (cons 'vector? vector?)
                (cons 'text
                    (vaquero-proc
                        primitive-type
                        'global
                        (lambda (args opts cont err)
                            (define size ((vaquero-send-atomic opts 'get) 'size))
                            (define init ((vaquero-send-atomic opts 'get) 'init))
                            (cont
                                (if (integer? size)
                                    (let ((s (make-string size (if (char? init) init #\space))))
                                        (vector-map (lambda (i x) (string-set! s i x)) (list->vector args))
                                        s)
                                    (apply string args))))))
                (cons 'text? string?)
                (cons 'rand random)
                (cons 'table
                    (vaquero-proc
                        primitive-type
                        'global
                        (lambda (args opts cont err)
                            (cont (apply vaquero-table args)))))
                (cons 'object
                    (vaquero-proc
                        primitive-type
                        'global
                        (lambda (args opts cont err)
                            (define autos (vaquero-send-atomic opts 'auto))
                            (define rsend (vaquero-send-atomic opts 'resend))
                            (define default ((vaquero-send-atomic opts 'get) 'default))
                            (if (eq? autos 'null) (set! autos #f) #f)
                            (if (eq? rsend 'null) (set! rsend #f) #f)
                            (if (eq? default 'null) (set! default #f) #f)
                            (cont (vaquero-object args autos rsend default)))))
                (cons 'send
                    (vaquero-proc
                        primitive-type
                        'global
                        (lambda (args opts cont err)
                            (define l (length args))
                            (if (< l 2)
                                (err
                                    (vaquero-error-object 'arity-mismatch `(send ,@args) "send requires two arguments: an object and a message.")
                                    cont)
                                (vaquero-send (car args) (cadr args) cont err)))))
                (cons 'math
                    (vaquero-object
                        (list
                            'e      2.718281828459045
                            'phi    1.618033988749895
                            'pi     3.141592653589793
                            'tau    6.283185307179587
                            'root-2 1.414213562373095
                            'max max
                            'min min
                            'sum (lambda (xs) (apply + xs))
                            'product (lambda (xs) (apply * xs))
                            'pow (lambda (x y) (expt x y))
                            'sqrt sqrt
                            'log log
                            'sin sin
                            'cos cos
                            'tan tan
                        )
                        #f
                        #f
                        #f))
                (cons 'gensym vaquero-gensym)
                (cons 'uuid uuid-v4)
                (cons 'cat
                    (vaquero-proc
                        primitive-type
                        'global
                        (lambda (args opts cont err)
                            (define l (length args))
                            (define texts (map (lambda (x) (vaquero-send-atomic x 'to-text)) args))
                            (define strings (map (lambda (t) (if (string? t) t "???")) texts))
                            (define joiner
                                (let ((j (vaquero-send-atomic opts 'with)))
                                    (if (string? j)
                                        j
                                        "")))
                            (cont
                                (if (< l 1)
                                    ""
                                    (string-join strings joiner))))))
                (cons 'debug
                    (lambda (crap)
                        (define stderr (current-error-port))
                        (vaquero-write crap stderr)
                        (newline stderr)
                        'null))
                (cons 'FILE_NOT_FOUND 'neither-true-nor-false)
                (cons 'T_PAAMAYIM_NEKUDOTAYIM (quote ::))))
        (fill-prelude primitives)
        prelude)
    (if genv
        genv
        (let ((noob (make-new)))
            (set! genv noob)
            (set! g-has? (vaquero-send-env noob 'has? top-cont top-err))
            (set! g-get (vaquero-send-env noob 'get top-cont top-err))
            noob)))

(define-syntax import-global-prelude
    (ir-macro-transformer
         (lambda (expr inject compare)
            (define global-prelude-file "prelude.vaq")
            (define text
                (with-input-from-file global-prelude-file read-string))
            `(define ,(inject 'global-prelude-text) ,text))))

(import-global-prelude)

(define (add-global-prelude this-env)
    (define cpath "~/.vaquero/prelude.vaq")
    (define is-cached (file-exists? cpath))
    (define expanded-prelude
        (if is-cached
            (with-input-from-file
                cpath
                (lambda ()
                    (read)))
            (let ((expanded
                    (vaquero-expand
                        (vaquero-read-file
                            (open-input-string global-prelude-text))
                        (local-env))))
                (with-output-to-file
                    cpath
                    (lambda ()
                        (write expanded)))
                expanded)))
    (define prelude-c
        (vaquero-seq-subcontractor expanded-prelude #t))
    (define full
        (prelude-c
                this-env
                top-cont
                top-err))
    'null)

(define (vaquero-global? x)
    (not (eq? not-found (glookup x))))

(define (lookup env x cont err)
    (vaquero-send-env
        env
        'has?
        (lambda (has?)
            (if (has? x)
                (vaquero-send-env
                    env
                    'get
                    (lambda (getter)
                        (cont (getter x)))
                    err)
                (vaquero-send-env
                    env
                    'parent
                    (lambda (mom)
                        (if (and mom (not (eq? mom 'null)))
                            (lookup mom x cont err)
                            (cont not-found)))
                    err)))
        err))

(define (extend env names vals cont err)
    (define noob (vaquero-environment env))
    (define args
        (let loop ((ns names) (vs vals) (yargs '()))
            (if (eq? '() ns)
                yargs
                (loop (cdr ns) (cdr vs) (cons (car ns) (cons (car vs) yargs))))))
    (define params
        (append
            (list
                noob
                (lambda (null) (cont noob))
                err)
            args))
    (apply mutate! params))

(define (mutate! env cont err . args)
    (vaquero-send-env
        env
        'def!
        (lambda (def!)
            (apply def! args)
            (cont 'null))
        err))

(define (update! env k v cont err)
    (vaquero-send-env
        env
        'has?
        (lambda (has?)
            (if (has? k)
                (vaquero-send-env
                    env
                    'def!
                    (lambda (def!)
                        (def! k v)
                        (cont 'null))
                    err)
                (vaquero-send-env
                    env
                    'parent
                    (lambda (mom)
                        (if (and mom (not (eq? mom 'null)))
                            (update! mom k v cont err)
                            (cont not-found)))
                    err)))
        err))

(define (delete! env k cont err)
    (vaquero-send-env
        env
        'has?
        (lambda (has?)
            (if (has? k)
                (vaquero-send-env
                    env
                    'rm!
                    (lambda (rm!)
                        (vaquero-apply rm! (list k) my-empty-table (lambda (_) (cont 'ok))  err))
                    err)
                (vaquero-send-env
                    env
                    'parent
                    (lambda (mom)
                        (if (and mom (not (eq? mom 'null)))
                            (delete! mom k cont err)
                            (cont not-found)))
                    err)))
        err))

(define (glookup x)
    (if (g-has? x)
        (g-get x)
        not-found))


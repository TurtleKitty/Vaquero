
(define (prep-defs seq env cont err)
    ; predefine all defs for mutual recursion
    (define (get-names seq)
        (map
            cadr
            (filter
                (lambda (x)
                    (and (list? x)
                         (> (length x) 1)
                         (or
                            (eq? (car x) 'def)
                            (and (eq? (car x) 'proc) (symbol? (cadr x)))
                            (eq? (car x) 'op))))
                seq)))
    (define names (get-names seq))
    (define haz? (vaquero-send-env env 'has? top-cont top-err))
    (define needed (filter (lambda (n) (not (haz? n)))  names))
    (define margs (flatten (zip needed (make-list (length needed) will-exist))))
    (if (> (length margs) 0)
        (apply mutate! (cons env (cons cont (cons err margs))))
        (cont 'null)))

(define (prepare-vaquero-args xs)
    (define (rval args opts)
        (cons (reverse args) (reverse opts)))
    (if (pair? xs)
        (let loop ((head (car xs)) (tail (cdr xs)) (args '()) (opts '()))
            (if (keyword? head)
                (let ((k head) (v (car tail)))
                    (if (pair? (cdr tail))
                        (loop (cadr tail) (cddr tail) args (cons v (cons k opts)))
                        (rval args (cons v (cons k opts)))))
                (if (pair? tail)
                    (loop (car tail) (cdr tail) (cons head args) opts)
                    (rval (cons head args) opts))))
        (rval '() '())))

(define my-empty-table (vaquero-table))

(define (prep-options opts)
    (if (= 0 (length opts))
        my-empty-table
        (apply vaquero-table
               (map (lambda (k) (if (keyword? k) (keyword->symbol k) k)) opts))))

(define-syntax frag
    (ir-macro-transformer
        (lambda (expr inject compare)
            (let ((body (cdr expr)))
                `(lambda (,(inject 'env) ,(inject 'cont) ,(inject 'err)) ,@body)))))

(define (vaquero-compile code)
    (if (and (pair? code) (list? code))
        (case (car code)
            ((def)      (vaquero-compile-def code))
            ((quote)    (vaquero-compile-quote code))
            ((if)       (vaquero-compile-if code))
            ((seq)      (vaquero-compile-seq code))
            ((op)       (vaquero-compile-op code))
            ((lambda)   (vaquero-compile-lambda code))
            ((proc)     (vaquero-compile-proc code))
            ((let)      (vaquero-compile-let code))
            ((wall)     (vaquero-compile-wall code))
            ((gate)     (vaquero-compile-gate code))
            ((capture)  (vaquero-compile-capture code))
            ((guard)    (vaquero-compile-guard code))
            ((fail)     (vaquero-compile-fail code))
            ((export)   (vaquero-compile-export code))
            (else       (vaquero-compile-application code)))
        (vaquero-compile-atom code)))

(define (vaquero-compile-atom code)
    (define pass (frag (cont code)))
    (if (symbol? code)
        (if (keyword? code)
            pass
            (case code
                ((true) (frag (cont #t)))
                ((false) (frag (cont #f)))
                ((null) pass)
                ((env) (frag (cont env)))
                (else
                    (if (vaquero-global? code)
                        (frag
                            (cont (glookup code)))
                        (frag
                            (lookup
                                env
                                code
                                (lambda (v)
                                    (if (eq? not-found v)
                                        (err (vaquero-error-object 'undefined-symbol code "Name not defined.") cont)
                                        (cont v)))
                                err))))))
        pass))

(define (vaquero-compile-def code)
   (define name (cadr code))
   (define val (caddr code))
   (define val-c (vaquero-compile val))
   (frag
      (val-c
         env
         (lambda (v)
            (mutate!
               env
               (lambda (null) (cont v))
               err
               name
               v))
         err)))

(define (vaquero-compile-quote code)
    (frag
        (cont (cadr code))))

(define (vaquero-compile-if code)
    (define pred (vaquero-compile (cadr code)))
    (define if-true (vaquero-compile (caddr code)))
    (define if-false (vaquero-compile (cadddr code)))
    (frag
        (pred
            env
            (lambda (b)
                (vaquero-bool
                    b
                    (lambda (is-true)
                        (if is-true
                            (if-true env cont err)
                            (if-false env cont err)))
                    err))
            err)))

(define (vaquero-compile-seq code)
   (define seq (cdr code))
   (if (pair? seq)
      (vaquero-seq-subcontractor seq #t)
      (vaquero-error 'bare-seq code "Empty sequences are forbidden!")))

(define (vaquero-seq-subcontractor xs prep?)
    (define head (car xs))
    (define tail (cdr xs))
    (let ((head-c (vaquero-compile head)))
        (if (pair? tail)
            (let ((tail-c (vaquero-seq-subcontractor tail #f)))
                (if prep?
                    (frag
                        (prep-defs
                            xs
                            env
                            (lambda (null)
                                (head-c
                                    env
                                    (lambda (h) (tail-c env cont err))
                                    err))
                            err))
                    (frag
                        (head-c
                            env
                            (lambda (h) (tail-c env cont err))
                            err))))
            head-c)))

(define (check-formals formals)
   (if (pair? formals)
      (let loop ((f (car formals)) (fs (cdr formals)))
         (if (holy? f)
            (begin
               (vaquero-error 'blasphemy formals (blasphemy f))
               #f)
            (if (pair? fs)
               (loop (car fs) (cdr fs))
               #t)))
      #t))

(define (make-vaquero-lambda code env formals body)
    (define arity (length formals))
    (define bodies-c (vaquero-seq-subcontractor body #f))
    (if (check-formals formals)
        (let ((p
            (vaquero-proc
                code
                env 
                (lambda (args opts cont err)
                    (if (not (= arity (length args)))
                        (err (list 'arity code (sprintf "This lambda requires ~A arguments. Given: " arity) args) cont)
                        (let* ((fargs (if (pair? args) (take args arity) '())))
                               (extend
                                    env 
                                    formals
                                    fargs
                                    (lambda (noob)
                                        (bodies-c noob cont err))
                                    err)))))))
            (hts! p 'type 'lambda)
            p)
        (vaquero-error 'bad-formals-in-lambda code "Bad formals!")))

(define (vaquero-compile-lambda code)
    (let ((formals (cadr code)) (bodies (cddr code)))
        (frag
            (cont (make-vaquero-lambda code env formals bodies)))))

(define (make-vaquero-proc code env formals bodies)
    (define arity (length formals))
    (define bodies-c (vaquero-seq-subcontractor bodies #t))
    (if (check-formals formals)
        (vaquero-proc
            code
            env 
            (lambda (args opts cont err)
                (if (< (length args) arity)
                    (err (list 'arity code (sprintf "This procedure requires at least ~A arguments. Given: " arity) args) cont)
                    (let* ((fargs (if (pair? args) (take args arity) '()))
                           (the-rest (if (pair? args) (drop args arity) '()))
                           (returner cont))
                           (extend
                                env 
                                (append formals '(opt rest return))
                                (append fargs (list opts the-rest returner))
                                (lambda (noob)
                                    (bodies-c noob cont err))
                                err)))))
        (vaquero-error 'bad-formals-in-proc code "Bad formals!")))

(define (vaquero-compile-proc code)
    (define is-named (symbol? (cadr code)))
    (if is-named
        (vaquero-compile `(def ,(cadr code) (proc ,(caddr code) ,@(cdddr code))))
        (let ((formals (cadr code)) (bodies (cddr code)))
            (frag
                (cont (make-vaquero-proc code env formals bodies))))))

(define (vaquero-compile-op code)
    (define name (cadr code))
    (define formals (caddr code))
    (define bodies (cdddr code))
    (frag
        (let ((thing (make-vaquero-proc code env formals bodies)))
            (hts! thing 'type 'op)
            (mutate!
                env
                (lambda (null)
                    (cont thing))
                err
                name
                thing))))

(define (vaquero-compile-new-env code wall-off)
   (define args (cadr code))
   (define arg-pairs
      (if (null? args)
         (cons '() '())
         (let loop ((name (car args)) (expr (cadr args)) (others (cddr args)) (names '()) (exprs '()))
            (define nu-names (cons name names))
            (define nu-exprs (cons expr exprs))
            (if (null? others)
               (cons (reverse nu-names) (reverse nu-exprs))
               (loop (car others) (cadr others) (cddr others) nu-names nu-exprs)))))
   (define let-vals-c
      (let ((vals (cdr arg-pairs)))
         (if (null? args)
            (lambda (env k e) (k '()))
            (vaquero-compile-list (cdr arg-pairs)))))
   (define exprs (cddr code))
   (define expr-c (vaquero-seq-subcontractor exprs #t))
   ; create new env and assign args
   (frag
      (let-vals-c
         env
         (lambda (vals)
            (define noob (vaquero-environment (if wall-off 'null env)))
            (if (null? args)
               (expr-c noob cont err)
               (vaquero-send noob 'def!
                  (lambda (def!)
                     (define var-names (car arg-pairs))
                     (define let-pairs (zip var-names vals))
                     (let loop ((p (car let-pairs)) (ps (cdr let-pairs)))
                        (define arg (car p))
                        (define val (cadr p))
                        (vaquero-apply def! (list arg val) 'null
                           (lambda (v)
                              (if (pair? ps)
                                 (loop (car ps) (cdr ps))
                                 (expr-c noob cont err)))
                           err)))
                       err)))
         err)))

(define (vaquero-compile-let code)
   (vaquero-compile-new-env code #f))

(define (vaquero-compile-wall code)
   (vaquero-compile-new-env code #t))

(define (vaquero-compile-gate code)
    (define exprs (cdr code))
    (define expr-c (vaquero-seq-subcontractor exprs #t))
    (frag
        (cont
            (expr-c env identity err))))

(define (vaquero-compile-capture code)
    (define name (cadr code))
    (define lamb (cons 'proc (cons (list name) (cddr code))))
    (define lamb-c (vaquero-compile lamb))
    (frag
        (lamb-c
            env
            (lambda (funk)
                (vaquero-apply
                    funk
                    (list (lambda (k-val) (cont k-val)))
                    'null
                    top-cont
                    err))
            err)))

(define (vaquero-compile-guard code)
    (define handler (cadr code))
    (define exprs (cddr code))
    (define handler-c (vaquero-compile handler))
    (define expr-c (vaquero-seq-subcontractor exprs #t))
    (frag
        (handler-c
            env
            (lambda (handler-fn)
                (define (new-err-cont e k)
                    (vaquero-apply handler-fn (list e k) 'null cont err))
                (expr-c env cont new-err-cont))
            err)))

(define (vaquero-compile-fail code)
    (define errobj (cadr code))
    (define erob-c (vaquero-compile errobj))
    (frag
        (erob-c
            env
            (lambda (e)
                (err e cont))
            err)))

(define (vaquero-compile-export code)
   (define names (cdr code))
   (frag
      (mutate! env (lambda (null) (cont 'null)) err 'vaquero-internal-exports names)))

(define (vaquero-compile-list xs)
    (if (pair? xs)
        (let ((head (vaquero-compile (car xs))) (tail (vaquero-compile-list (cdr xs))))
            (frag
                (head
                    env
                    (lambda (h)
                        (tail
                            env
                            (lambda (t) (cont (cons h t)))
                            err))
                    err)))
        (frag (cont '()))))

(define (vaquero-compile-application code)
    (define fn-c (vaquero-compile (car code)))
    (define args-opts (prepare-vaquero-args (cdr code)))
    (define args-c (vaquero-compile-list (car args-opts)))
    (define opts-c (vaquero-compile-list (cdr args-opts)))
    (frag
        (fn-c
            env
            (lambda (f) 
                (args-c
                    env
                    (lambda (args)
                        (opts-c
                            env
                            (lambda (opts)
                                (vaquero-apply f args (prep-options opts) cont err))
                            err))
                    err))
            err)))


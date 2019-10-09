
(define (check-vaquero-syntax prog)
   (if (and (pair? prog) (list? prog))
      (check-vaquero-form prog)
      #t))

(define (check-vaquero-form form)
   (if (list? form)
      (if (eq? form '())
         #t
         (let ((head (car form)))
            (case head
               ((def)      (check-vaquero-def form))
               ((quote)    (check-vaquero-quote form))
               ((if)       (check-vaquero-if form))
               ((seq)      (check-vaquero-seq form))
               ((lambda)   (check-vaquero-lambda form))
               ((proc)     (check-vaquero-proc form))
               ((wall)     (check-vaquero-wall form))
               ((gate)     (check-vaquero-gate form))
               ((capture)  (check-vaquero-capture form))
               ((guard)    (check-vaquero-guard form))
               ((fail)     (check-vaquero-fail form))
               ((use)      (check-vaquero-use form))
               ((export)   (check-vaquero-export form))
               (else       (check-vaquero-application form)))))
      #t))

(define (say x)
   (display x)
   (newline))

(define (syntax-error code e usage)
   (say "Syntax error:")
   (say code)
   (say e)
   (say usage)
   (newline)
   #f)

(define (check-vaquero-list form)
   (define bool-party (map check-vaquero-form form))
   (define true? (lambda (x) (eq? x #t)))
   (every true? bool-party))

(define (check-vaquero-def form)
   (define usage '(def <name> <value>))
   (if (< (length form) 3)
      (syntax-error form "def: too few arguments." usage))
      (let ((name (cadr form)) (value (caddr form)))
         (cond
            ((> (length form) 3)
               (syntax-error form "def: too many arguments" usage))
            ((not (symbol? name))
               (syntax-error form "def requires a symbol as its first argument." usage))
            ((holy? name)
               (syntax-error form (string-join `("def:" ,(blasphemy name)) " ") usage))
            (else (check-vaquero-form value)))))

(define (check-vaquero-quote form)
   (define usage '(quote <s-expression>))
   (if (not (eq? (length form) 2))
      (syntax-error form "quote takes one argument." usage)
      #t))

(define (check-vaquero-if form)
   (define usage '(if <predicate> <consequent> <alternative>))
   (if (< (length form) 4)
      (syntax-error form "if: too few arguments" usage)
      (if (> (length form) 4)
         (syntax-error form "if: too many arguments" usage)
         (and
            (check-vaquero-form (cadr form))
            (check-vaquero-form (caddr form))
            (check-vaquero-form (cadddr form))))))

(define (check-vaquero-seq form)
   (define usage '(seq <form> ...))
   (if (< (length form) 2)
      (syntax-error form "seq: empty sequences are forbidden." usage)
      (check-vaquero-list (cddr form))))

(define (check-vaquero-op form)
   (define usage '(op <name> (<arg> ...) <body> ...))
   (if (< (length form) 4)
      (syntax-error form "op requires at least three arguments." usage))
   (let ((name (cadr form)))
      (cond
         ((not (symbol? (cadr form)))
            (syntax-error form "op requires a symbol as its first argument." usage))
         ((not (list? (caddr form)))
            (syntax-error form "op: second argument must be a list of formals." usage))
         ((< (length form) 4)
            (syntax-error form "op: at least one body form is required." usage))
         ((holy? name)
            (syntax-error form (string-join `("op:" ,(blasphemy name)) " ") usage))
         (else
            (check-vaquero-list (cdddr form))))))

(define (check-vaquero-lambda form)
   (define usage '(lambda (<arg> ...) <body>))
   (if (not (list? (cadr form)))
      (syntax-error form "lambda: second argument must be a list of formals." usage)
      (if (not (= (length form) 3))
         (syntax-error form "lambda: one body form is required; only one is allowed." usage)
         (check-vaquero-list (cddr form)))))

(define (check-vaquero-proc form)
   (define usage '(proc <name?> (<arg> ...) <body> ...))
   (define arg1 (cadr form))
   (if (symbol? arg1)
      (let ((name arg1) (args (caddr form)))
         (if (holy? name)
            (syntax-error form (string-join `("proc:" ,(blasphemy name)) " ") usage)
            (if (not (list? args))
               (syntax-error form "named proc: third argument must be a list of formals." usage)
               (if (< (length form) 4)
                  (syntax-error form "proc: at least one body form is required." usage)
                  (check-vaquero-list (cdddr form))))))
      (let ((args arg1))
         (if (not (list? args))
            (syntax-error form "anon proc: second argument must be a list of formals." usage)
            (if (< (length form) 3)
               (syntax-error form "proc: at least one body form is required." usage)
               (check-vaquero-list (cddr form)))))))

(define (check-vaquero-let form)
   (define usage '(let (<name> <value> ...) <body> ...))
   (if (not (list? (cadr form)))
      (syntax-error form "let: second argument must be a list of alternating names and values." usage)
      (if (< (length form) 3)
         (syntax-error form "let: one body form is required." usage)
         (check-vaquero-list (cddr form)))))

(define (check-vaquero-wall form)
   (define usage '(wall (<name> <value> ...) <body> ...))
   (if (not (list? (cadr form)))
      (syntax-error form "wall: second argument must be a list of alternating names and values." usage)
      (if (< (length form) 3)
         (syntax-error form "wall: one body form is required." usage)
         (check-vaquero-list (cddr form)))))

(define (check-vaquero-gate form)
   (define usage '(gate <body> ...))
   (if (< (length form) 2)
      (syntax-error form "gate: too few arguments." usage)
      (check-vaquero-list (cdr form))))

(define (check-vaquero-capture form)
   (define usage '(capture <name> <body> ...))
   (if (< (length form) 3)
      (syntax-error form "capture: too few arguments." usage)
      (if (not (symbol? (cadr form)))
         (syntax-error form "capture requires a symbol as its first argument." usage)
         (check-vaquero-list (cddr form)))))

(define (check-vaquero-guard form)
   (define usage '(guard (proc (error restart) <body> ...) <body> ...))
   (if (< (length form) 3)
      (syntax-error form "guard: too few arguments." usage)
      (check-vaquero-list (cddr form))))

(define (check-vaquero-fail form)
   (define usage '(fail <object>))
   (if (< (length form) 2)
      (syntax-error form "fail: too few arguments." usage)
      (if (> (length form) 2)
         (syntax-error form "fail: too many arguments." usage)
         (check-vaquero-list (cdr form)))))

(define (check-vaquero-use form)
   (define usage '(use <package-name> <source>))
   (if (< (length form) 3)
      (syntax-error form "use: too few arguments." usage)
      (let ((name (cadr form)) (uri (caddr form)))
         (if (not (symbol? name))
            (syntax-error form "use: package-name must be a symbol." usage)
            (if (not (or (symbol? uri) (string? uri)))
               (syntax-error form "use: source must be a symbol or string." usage)
               #t)))))

(define (check-vaquero-import code)
   (define usage '(import <package-name> <import-name> ...))
   (if (< (length code) 3)
      (syntax-error code "import: too few arguments." usage)
      (let ((package-name (cadr code)) (import-names (cddr code)))
         (if (not (symbol? package-name))
            (syntax-error code "import: package-name must be a symbol." usage)
            (if (not (every symbol? import-names))
               (syntax-error code "import: import-names must be symbols." usage)
               #t)))))

(define (check-vaquero-export code)
   (define usage '(syntax <name> <name> ...))
   (define (check x)
      (if (not (symbol? x))
         (syntax-error code "export: exported names must be symbols.")
         #t))
   (let ((rest (cdr code)))
      (if (not (pair? rest))
         (syntax-error code "export: must have at least 1 argument." usage)
         (let loop ((x (car rest)) (xs (cdr rest)))
            (if (check x)
               (if (pair? xs)
                  (loop (car xs) (cdr xs))
                  #t)
               #f)))))

(define check-vaquero-application check-vaquero-list)



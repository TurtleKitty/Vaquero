
(define-record-type vaq-env
   (vaquero-env vars parent)
   vaquero-env?
   (vars vaquero-env-vars vaquero-env-set-vars!)
   (parent vaquero-env-parent vaquero-env-set-parent!))

(define (vaquero-environment parent)
   (vaquero-env (vaquero-table) (if parent parent 'null)))

(define vaquero-send-env-vtable
   (let ()
      (define undefineds (list not-found will-exist 'null))

      (method answers?
         (cont (lambda (msg) (hte? vaquero-send-env-vtable msg))))

      (method autos
         (cont '(view to-text to-bool keys values pairs)))

      (method messages
         (cont (htks vaquero-send-env-vtable)))

      (method to-bool
         (vaquero-send-table (vaquero-env-vars obj) 'to-bool cont err))

      (method type
         (cont 'env))

      (method to-text
         (vaquero-view obj))

      (method get
         (vaquero-send-table (vaquero-env-vars obj) 'get cont err))

      (method has?
         (vaquero-send-table (vaquero-env-vars obj) 'has? cont err))

      (method env-keys
         (vaquero-send-table (vaquero-env-vars obj) 'keys cont err))

      (method env-values
         (vaquero-send-table (vaquero-env-vars obj) 'values cont err))

      (method pairs
         (vaquero-send-table (vaquero-env-vars obj) 'pairs cont err))

      (method env-def!
         (define vars (vaquero-env-vars obj))
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

      (method env-lookup
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

      (method env-merge!
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
                         (if (not (vaquero-env? other-env))
                            (arg-fail '(env.merge! WAT))
                            (let ((def-this (vaquero-send-atomic obj 'def!))
                                  (other-vars (hash-table->alist (vaquero-env-vars other-env))))
                               (if (not (pair? other-vars))
                                  (cont obj)
                                  (let loop ((v (car other-vars)) (vs (cdr other-vars)) (flat '()))
                                     (define nu-flat (cons (car v) (cons (cdr v) flat)))
                                     (if (pair? vs)
                                        (loop (car vs) (cdr vs) nu-flat)
                                        (vaquero-apply def-this nu-flat 'null (lambda (x) (cont obj)) err))))))))))))

      (method env-extend
         (cont
            (vaquero-proc
               primitive-type
               'env
               (lambda (args opts cont err)
                   (let loop ((names '()) (vals '()) (left args))
                      (if (eq? '() left)
                         (extend obj names vals cont err)
                         (loop (cons (car left) names) (cons (cadr left) vals) (cddr left))))))))

      (method env-parent
         (cont (vaquero-env-parent obj)))

      (method vaq-env-eval
         (cont
            (lambda (code)
               (vaquero-eval code obj))))

      (method vaq-env-expand
         (cont
            (lambda (code)
               (vaquero-expand code obj))))

      (method vaq-env-load
         (cont
            (lambda (stream)
               (vaquero-eval (vaquero-read-file stream) obj))))

      (method env-default
         (cont
            (lookup obj msg
               (lambda (val)
                  (if (eq? val not-found)
                     (err (vaquero-error-object 'message-not-understood `(send ,obj ,msg) "Message not understood.") cont)
                     val))
               err)))

      (alist->hash-table
         `((answers?   . ,answers?)
           (autos      . ,autos)
           (messages   . ,messages)
           (to-bool    . ,to-bool)
           (to-text    . ,to-text)
           (type       . ,type)
           (view       . ,to-text)
           (get        . ,get)
           (has?       . ,has?)
           (keys       . ,env-keys)
           (values     . ,env-values)
           (pairs      . ,pairs)
           (def!       . ,env-def!)
           (lookup     . ,env-lookup)
           (merge!     . ,env-merge!)
           (extend     . ,env-extend)
           (parent     . ,env-parent)
           (eval       . ,vaq-env-eval)
           (expand     . ,vaq-env-expand)
           (load       . ,vaq-env-load)
           (default    . ,env-default)))))

(define (local-env)
   (vaquero-environment 'null))

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
            (vaquero-apply preset! (list (car p) (cdr p)) 'null top-cont top-err))
         (map setem! fs))
      (define vaquero-make-vector
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
      (define vaquero-make-table
         (vaquero-proc
            primitive-type
            'global
            (lambda (args opts cont err)
               (cont (apply vaquero-table args)))))
      (define vaquero-make-object
         (vaquero-proc
            primitive-type
            'global
            (lambda (args opts cont err)
               (define autos (vaquero-send-atomic opts 'auto))
               (define fwd (vaquero-send-atomic opts 'forward))
               (define default ((vaquero-send-atomic opts 'get) 'default))
               (if (eq? autos 'null) (set! autos #f) #f)
               (if (eq? fwd 'null) (set! fwd #f) #f)
               (if (eq? default 'null) (set! default #f) #f)
               (cont (vaquero-object args autos fwd default)))))
      (define vaquero-math-object
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
               'tan tan)
            #f
            #f
            #f))
      (define vaquero-physics-object
         (vaquero-object
            (list
               'c   299792458
               'g   9.80665
               'G   6.67408e-11
               'h   6.626070040e-34
               'Na  6.022140857e23)
            #f
            #f
            #f))
      (define vaquero-send-global
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
      (define vaquero-cat-global
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
      (define primitives
         (list
             (cons 'stdin  (current-input-port))
             (cons 'stdout (current-output-port))
             (cons 'stderr (current-error-port))
             (cons 'read
                (lambda ()
                   (vaquero-read (current-input-port))))
             (cons 'write
                (lambda (out)
                    (vaquero-write out (current-output-port))
                    'null))
             (cons 'print
                (lambda (out)
                    (vaquero-print out (current-output-port))
                    'null))
             (cons 'say
                (lambda (out)
                    (vaquero-print out (current-output-port))
                    (newline (current-output-port))
                    'null))
             (cons 'log
                (lambda (out)
                    (define stderr (current-error-port))
                    (vaquero-write out stderr)
                    (newline stderr)
                    'null))
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
             (cons 'pair cons)
             (cons 'pair? pair?)
             (cons 'list list)
             (cons 'list? list?)
             (cons 'option? keyword?)
             (cons 'syntax-ok? (lambda (form) (check-vaquero-syntax (list form))))
             (cons 'vector vaquero-make-vector)
             (cons 'vector? vector?)
             (cons 'text? string?)
             (cons 'rand random)
             (cons 'table vaquero-make-table)
             (cons 'object vaquero-make-object)
             (cons 'send vaquero-send-global)
             (cons 'math vaquero-math-object)
             (cons 'physics vaquero-physics-object)
             (cons 'gensym vaquero-gensym)
             (cons 'uuid uuid-v4)
             (cons 'cat vaquero-cat-global)
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
   (define cpath cached-global-prelude-path)
   (define is-cached (file-exists? cpath))
   (define expanded-prelude
      (if is-cached
         (with-input-from-file
            cpath
            (lambda () (read)))
         (let ((expanded
                (vaquero-expand
                   (vaquero-read-file
                      (open-input-string global-prelude-text))
                   (local-env))))
            (with-output-to-file
               cpath
               (lambda () (write expanded)))
            expanded)))
   (define prelude-c
      (vaquero-compile expanded-prelude))
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
            (let ((mom (vaquero-env-parent env)))
               (if (and mom (not (eq? mom 'null)))
                  (lookup mom x cont err)
                  (cont not-found)))))
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
         (vaquero-apply def! args 'null cont err))
      err))

(define (glookup x)
   (if (g-has? x)
      (g-get x)
      not-found))


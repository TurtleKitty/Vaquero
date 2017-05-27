
(define (vaquero-repl)
   (define stdin (current-input-port))
   (define stdout (current-output-port))
   (define stderr (current-error-port))
   (define (loop env)
      (define repl-err
         (lambda (ex continue)
            (debug 'runtime-error
               (if (and (hash-table? ex) (eq? (vaquero-send-atomic ex 'type) 'error))
                  (map (lambda (f) (vaquero-view (vaquero-send-atomic ex f))) '(name form to-text))
                  (vaquero-view ex)))
            (loop env)))
      (display "vaquero> ")
      (handle-exceptions exn
         (begin
            (vaquero-write exn (current-error-port))
            (newline (current-error-port))
            (read-line stdin) ; let's just do away with that
            (loop env))
         (let ((expr (vaquero-read stdin)))
            (if (eof-object? expr)
               (exit)
               (let ((expanded (vaquero-expand expr (vaquero-environment env))))
                  (define check? (check-vaquero-syntax expanded))
                  (if check?
                     (let ((compiled (vaquero-compile expanded)))
                        (compiled
                           env
                           (lambda (v)
                              (define noob   (local-env))
                              (define mom    (htr env 'parent))
                              (define evars  (htr env 'vars))
                              (define mvars  (htr mom 'vars))
                              (vaquero-send-table mvars 'merge
                                 (lambda (fn)
                                    (define nuvars (fn evars))
                                    (define print-me (if (eof-object? v) 'EOF v))
                                    (hts! mom  'vars nuvars)
                                    (hts! noob 'parent mom)
                                    (vaquero-write print-me stdout)
                                    (newline)
                                    (loop noob))
                                repl-err))
                         repl-err))
                     (begin
                        (display "Syntax error!\n")
                        (loop env))))))))
   (newline)
   (display "Welcome to the Vaquero Read-Eval-Print Loop.  Press Ctrl-D to exit.")
   (newline)
   (newline)
   (loop (vaquero-environment (cli-env))))


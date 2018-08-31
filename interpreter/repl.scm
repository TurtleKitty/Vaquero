
(define (vaquero-repl)
   (define stdin (current-input-port))
   (define stdout (current-output-port))
   (define stderr (current-error-port))
   (define (loop env)
      (define repl-err
         (lambda (ex continue)
            (debug 'runtime-error
               (if (vaquero-error? ex)
                  (map (lambda (f) (vaquero-view (vaquero-send-atomic ex f))) '(name form to-text))
                  (vaquero-view ex)))
            (loop env)))
      (display "vaquero> ")
      (handle-exceptions exn
         (begin
            (write 'REPL-FAIL (current-error-port))
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
                              (define mom    (vaquero-env-parent env))
                              (define evars  (vaquero-env-vars env))
                              (define mvars  (vaquero-env-vars mom))
                              (define nuvars (hash-table-merge evars mvars))
                              (define print-me (if (eof-object? v) 'EOF v))
                              (vaquero-env-set-vars! mom nuvars)
                              (vaquero-env-set-parent! noob mom)
                              (vaquero-write print-me stdout)
                              (newline)
                              (loop noob))
                         repl-err))
                     (begin
                        (display "Syntax error!\n")
                        (loop env))))))))
   (newline)
   (display "Welcome to the Vaquero Read-Eval-Print Loop.  Press Ctrl-D to exit.")
   (newline)
   (newline)
   (loop (vaquero-environment (cli-env))))


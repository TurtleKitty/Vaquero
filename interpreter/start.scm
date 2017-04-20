
(define (start)
    (define args (command-line-arguments))
    (define (fname)
        (if (pair? (cdr args))
            (cadr args)
            (usage)))
    (define (prep-dir path)
        (if (not (directory? path))
            (create-directory path #t)
            #f))
    (define (locate-path)
        (find-file (make-module-absolute-path (fname))))
    (prep-dir vaquero-mod-dir)
    (prep-dir vaquero-expanded-dir)
    (prep-dir vaquero-compiled-dir)
    (add-global-prelude (global-env))
    (if (not (pair? args))
        (usage)
        (let ((cmd (string->symbol (car args))))
            (case cmd
                ((repl) (vaquero-repl))
                ((eval) 
                    (let ((code-str (fname)))
                        (define code
                            (vaquero-read-file
                                (open-input-string code-str)))
                        (define expanded
                            (vaquero-expand code (cli-env)))
                        (if (check-vaquero-syntax expanded)
                            (let ((linked (vaquero-link expanded)))
                               (vaquero-run linked))
                            (exit))))
                ((run)
                    (let* ((expanded (read-expand-cache-prog (fname) (cli-env))) (fpath (locate-path)) (cpath (get-vaquero-compiled-path fpath)))
                        (if (check-vaquero-syntax expanded)
                            (let ((is-cached (and (file-exists? cpath) (file-newer? cpath fpath))))
                               (if is-cached
                                  (vaquero-run
                                     (call-with-input-file
                                        cpath
                                        vaquero-read))
                                  (let ((linked (vaquero-link expanded)) (save-file (open-output-file cpath)))
                                     (vaquero-write linked save-file)
                                     (close-output-port save-file)
                                     (vaquero-run linked))))
                            (exit))))
                ((check)
                    (let ((its-good (check-vaquero-syntax (cdr (read-expand-cache-prog (fname) (cli-env))))))
                        (display "Syntax check complete: ")
                        (say (if its-good 'ok 'FAIL))))
                ((clean)
                    (let ((cached (append (glob "~/.vaquero/expanded/*") (glob "~/.vaquero/compiled/*") (glob "~/.vaquero/modules/*") (list "~/.vaquero/global.vaq"))))
                        (let loop ((f (car cached)) (fs (cdr cached)))
                            (delete-file* f)
                            (if (eq? fs '())
                                (display "Cache cleared.\n")
                                (loop (car fs) (cdr fs))))))
                ((compile)
                    (let ((expanded (read-expand-cache-prog (fname) (cli-env))))
                        (debug "Wrote compiled file to " (get-vaquero-expanded-path (locate-path))))) ; FIXME precompile too
                ((expand)
                    (begin
                        (pp
                            (vaquero-view
                                (read-expand-cache-prog (fname) (cli-env))))
                        (newline)))
                (else (printf "Unknown command: ~A~%" cmd))))))


(define (vaquero-internal-prep-dir path)
   (if (not (directory? path))
      (create-directory path #t)
      #f))

(define (vaquero-internal-locate-path path)
   (find-file (make-module-absolute-path path)))

(define (vaquero-internal-start-check filename)
   (define its-good (check-vaquero-syntax (cdr (read-expand-cache-prog filename (cli-env)))))
   (display "Syntax check complete: ")
   (say (if its-good 'ok 'FAIL)))

(define (vaquero-internal-start-clean)
   (define cached
      (append
         (glob (vaquero-cache-dir "expanded/*"))
         (glob (vaquero-cache-dir "compiled/*"))
         (glob (vaquero-cache-dir "modules/*"))
         (list cached-global-prelude-path)))
   (let loop ((f (car cached)) (fs (cdr cached)))
      (delete-file* f)
      (if (eq? fs '())
         (display "Cache cleared.\n")
         (loop (car fs) (cdr fs)))))

(define (vaquero-internal-start-eval code-text)
   (define code
      (vaquero-read-file
         (open-input-string code-text)))
   (define expanded
      (vaquero-expand code (cli-env)))
   (if (check-vaquero-syntax expanded)
      (let ((linked (vaquero-link expanded)))
        (vaquero-run linked))
      (exit)))

(define (vaquero-internal-start-run filename)
   (define expanded (read-expand-cache-prog filename (cli-env)))
   (define fpath (vaquero-internal-locate-path filename))
   (define cpath (get-vaquero-compiled-path fpath))
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
      (exit)))

(define (vaquero-internal-start-compile filename)
   (define expanded (read-expand-cache-prog filename (cli-env)))
   (define linked (vaquero-link expanded))
   (define cpath (get-vaquero-compiled-path (vaquero-internal-locate-path filename)))
   (define save-file (open-output-file cpath))
   (vaquero-write linked save-file)
   (close-output-port save-file)
   (debug "Wrote compiled file to " cpath))

(define (vaquero-internal-start-expand filename)
   (define expanded (read-expand-cache-prog filename (cli-env)))
   (pp (vaquero-view expanded))
   (newline))

(define (start)
   (define args (command-line-arguments))
   (define (fname)
      (if (pair? (cdr args))
         (cadr args)
         (usage)))
   (vaquero-internal-prep-dir vaquero-mod-dir)
   (vaquero-internal-prep-dir vaquero-expanded-dir)
   (vaquero-internal-prep-dir vaquero-compiled-dir)
   (add-global-prelude (global-env))
   (if (not (pair? args))
      (usage)
      (let ((arg (car args)))
         (define cmd (string->symbol arg))
         (case cmd
            ((repl)    (vaquero-repl))
            ((eval)    (vaquero-internal-start-eval    (fname)))
            ((run)     (vaquero-internal-start-run     (fname)))
            ((check)   (vaquero-internal-start-check   (fname)))
            ((clean)   (vaquero-internal-start-clean))
            ((clear)   (vaquero-internal-start-clean))
            ((compile) (vaquero-internal-start-compile (fname)))
            ((expand)  (vaquero-internal-start-expand  (fname)))
            (else
               (if (file-exists? arg)
                  (vaquero-internal-start-run arg)
                  (else (printf "Unknown command: ~A~%" cmd))))))))


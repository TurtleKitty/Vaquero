
(define global-arg-pair
   (vaquero-cli-args (command-line-arguments)))

(define shell-exec
   (lambda (cmd)
      (string-chomp (read-string #f (process cmd)))))

(define sys-env
   (let ((get-env-ht (lambda () (alist->hash-table (get-environment-variables)))))
      (vaquero-object
         (list
            'type     '(operating-system-interface)
            'view      (lambda () (vaquero-view (get-env-ht)))
            'uname     (system-information)
            'hostname  (get-host-name)
            'to-record (lambda () (get-env-ht))
            'get
               (lambda (x)
                  (define envt (get-environment-variables))
                  (define (try z)
                     (define p (assoc z envt))
                     (if p (cdr p) 'null))
                  (if (symbol? x)
                     (let ((y (symbol->string x)))
                        (try y))
                     (try x)))
            'set!
               (lambda (k v)
                  (if (symbol? k)
                     (set-environment-variable! (symbol->string k) v)
                     (set-environment-variable! k v))
                     v)
            'del!
               (lambda (k)
                  (if (symbol? k)
                     (unset-environment-variable! (symbol->string k))
                     (unset-environment-variable! k))
                  'null)
         )
         '(to-record) #f #f)))

(define sys-fs
   (vaquero-object
      (list
         'type     '(operating-system-interface)
         'rm        (lambda (f) (delete-file* f) 'null)
         'cp        (lambda (old new) (copy-file old new))
         'mv        (lambda (old new) (move-file old new))
         'ln        (lambda (old new) (create-symbolic-link old new) 'null)
         'tmp       (lambda () (create-temporary-file))
         'pwd       (lambda () (current-directory))
         'ls        (lambda (dir) (directory dir #t))
         'cd        (lambda (dir) (change-directory dir) 'null)
         'chroot    (lambda (dir) (set-root-directory! dir) 'null)
         'mkdir     (lambda (dir) (create-directory dir #t) 'null)
         'rmdir     (lambda (dir) (delete-directory dir #t) 'null)
         'tmp-dir   (lambda () (create-temporary-directory))
         'home      (shell-exec "ls -d ~")
         'stat      file-stat
         'exists?   file-exists?
         'dir?      directory?
         'symlink?  symbolic-link?
         'listen    vaquero-fs-listener
         'connect
            (lambda (path)
               (define-values (in out) (unix-connect path))
               (vaquero-fs-socket path in out))
         'socket-pair
            (lambda ()
               (define-values (in1 out1 in2 out2) (unix-pair))
               (cons (vaquero-fs-socket '? in1 out1) (vaquero-fs-socket '? in2 out2)))
         'read-from
            (vaquero-proc
               primitive-type
               'sys
               (lambda (args opts cont err)
                  (define len (length args))
                  (if (< len 1)
                     (err (vaquero-error-object 'bad-arguments `(sys.fs.read-from ,@args) "sys.fs.read-from requires a filename."))
                     (if (hte? opts 'with)
                        (let ((with (htr opts 'with)))
                           (if (not (= 1 (vaquero-send-atomic with 'arity)))
                              (err (vaquero-error-object 'wrong-arity `(sys.fs.read-from ,@args with: ...) "sys.fs.read-from : with: procedures must accept a stream argument."))
                              (cont
                                 (call-with-input-file (car args)
                                    (lambda (f)
                                       (vaquero-apply with (list f) 'null top-cont err))))))
                        (cont (open-input-file (car args)))))))
         'write-to
            (vaquero-proc
               primitive-type
               'sys
               (lambda (args opts cont err)
                  (define len (length args))
                  (if (< len 1)
                     (err (vaquero-error-object 'bad-arguments `(sys.fs.write-to ,@args) "sys.fs.write-to requires a filename."))
                     (if (hte? opts 'with)
                        (let ((with (htr opts 'with)))
                           (if (not (= 1 (vaquero-send-atomic with 'arity)))
                              (err (vaquero-error-object 'wrong-arity `(sys.fs.write-to ,@args with: ...) "sys.fs.write-to : with: procedures must accept a stream argument."))
                              (cont
                                 (call-with-output-file (car args)
                                    (lambda (f)
                                       (vaquero-apply with (list f) 'null top-cont err))))))
                        (cont (open-output-file (car args)))))))
      )
      '(pwd socket-pair tmp tmp-dir) #f #f))

(define sys-http
   (vaquero-object
      (list
         'type '(operating-system-interface)
         'get (lambda (uri)
                (define got (get-uri uri))
                (if (eq? got not-found)
                   'null
                   got)))
      #f #f #f))

(define sys-tcp
   (vaquero-object
      (list
         'type    '(operating-system-interface)
         'connect (lambda (host port)
                     (define-values (in out) (tcp-connect host port))
                     (vaquero-tcp-socket in out))
         'listen  (lambda (host port)
                     (vaquero-tcp-listener host port))
      )
      #f #f #f))

(define sys-signal
   (vaquero-object
      (list
         'type '(operating-system-interface)
         'hup   1
         'int   2
         'quit  3
         'ill   4
         'abrt  6
         'fpe   8
         'kill  9
         'usr1 10
         'segv 11
         'usr2 12
         'pipe 13
         'alrm 14
         'term 15
         'chld 17
         'cont 18
         'stop 19
         'send    (lambda (pid sig) (process-signal pid sig))
         'mask    (lambda (sig) (signal-mask! sig) 'null)
         'masked? (lambda (sig) (signal-masked? sig))
         'unmask  (lambda (sig) (signal-unmask! sig) 'null)
         'handler (lambda (sig) (signal-handler sig))
         'handle  (lambda (sig fn)
                     (set-signal-handler!
                        sig
                        (lambda (sig)
                           (vaquero-apply fn (list sig) 'null top-cont top-err)))
                     'null)
      )
      #f #f #f))

(define vaquero-spawn-process
   (vaquero-proc
      primitive-type
      'sys
      (lambda (args opts cont err)
         (let ((len (length args)))
            (if (= len 0)
               (err (vaquero-error-object 'bad-arguments `(sys.proc.spawn ,@args) "Shell command required.") cont)
               (let ((cmd (car args)))
                  (define env (if (hte? opts 'env) (hash-table->alist (htr opts 'env)) '()))
                  (define-values (stdout stdin pid stderr) (process* cmd '() env))
                  (cont
                     (vaquero-object
                        (list
                           'type   '(process)
                           'pid    pid
                           'stdin  stdin
                           'stdout stdout
                           'stderr stderr)
                        #f #f #f))))))))

(define sys-proc
   (vaquero-object
      (list
         'type '(operating-system-interface)
         'pid   (lambda () (current-process-id))
         'uid   (lambda () (current-user-id))
         'gid   (lambda () (current-group-id))
         'parent-pid   (lambda () (parent-process-id))
         'process-gid  (lambda (pid) (process-group-id pid))
         'program-name (lambda () running-program-name)
         'run   (lambda (cmd) (process-run cmd))
         'sleep (lambda (s) (sleep s) s)
         'spawn vaquero-spawn-process
         'fork  (lambda (thunk)
                   (process-fork
                      (lambda ()
                         (vaquero-apply thunk '() 'null top-cont top-err))))
         'exit exit
      )
      '(pid uid gid parent-pid program-name) #f #f))

(define (norris-day)
   (inexact->exact (current-seconds)))

(define (fire-missile)
   (define (alert n)
      (display "Launching in ")
      (display n)
      (display "...")
      (newline)
      (sleep 1))
   (display "Are you sure you want to do that, cowboy?")
   (newline)
   (let ((response (read)))
      (let ((r (string-ref (symbol->string response) 0)))
         (if (or (eq? r #\y) (eq? r #\Y))
            (begin
               (display "Ok, mad hacker.  Hope you have a fallout shelter.")
               (newline)
               (let loop ((n 5))
                  (alert n)
                  (if (eq? n 1)
                     (begin
                        (display "Good luck...")
                        (newline)
                        (sleep 7)
                        'KABOOM)
                     (loop (- n 1)))))
            (begin
               (display "Wise man.")
               (newline)
               'null)))))

(define sys
   (vaquero-object
      (list
         'opt     (cdr global-arg-pair)
         'rest    (car global-arg-pair)
         'type    '(operating-system-interface)
         'env     sys-env
         'fs      sys-fs
         'signal  sys-signal
         'proc    sys-proc
         'net     (vaquero-object (list 'type '(operating-system-interface) 'http sys-http 'tcp sys-tcp) #f #f #f)
         'time    norris-day
         'ts      norris-day
         'srand
            (lambda (v)
               (set-pseudo-random-seed! v)
               'null)
         'shell   shell-exec
         '64764 (lambda () (display "\n   **** COMMODORE 64 BASIC V2 ****\n\n 64K RAM SYSTEM  38911 BASIC BYTES FREE\n\n") 'READY.)
         'launch-the-missile fire-missile)
      '(ts 64764 launch-the-missile)
      #f
      #f))



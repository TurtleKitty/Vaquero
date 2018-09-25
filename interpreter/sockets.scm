
(define (vaquero-fs-listener path)
   (define l (unix-listen path))
   (vaquero-object
      (list
         'type   '(fs listener)
         'view   (list 'fs-listener path)
         'to-bool #t
         'path   path
         'ready? (lambda () (unix-accept-ready? l))
         'accept (lambda ()
                  (let-values (((in out) (unix-accept l)))
                     (vaquero-fs-socket path in out)))
         'close  (lambda () (unix-close l) 'null)
      )
      '(ready? accept close)
      #f
      #f))

(define (vaquero-fs-socket path in out)
   (vaquero-object
      (list
         'type   '(fs socket source sink stream)
         'view   (list 'socket path)
         'to-bool #t
         'path path
         'close (lambda ()
                  (close-input-port in)
                  (close-output-port out)
                  'null)
      )
      '(read read-char read-line read-lines read-text ready? nl close)
      (list
         (list in 'read 'read-char 'peek-char 'read-line 'read-lines 'read-text 'ready?
                  'skip-n 'skip-while 'skip-until 'read-n 'read-while 'read-until)
         (list out 'write 'print 'say 'nl))
      #f))

(define (vaquero-tcp-listener host port)
   (define l (tcp-listen port 100 host))
   (vaquero-object
      (list
         'type   '(tcp listener)
         'view   (list 'tcp-listener host port)
         'to-bool #t
         'port   (tcp-listener-port l)
         'ready? (lambda () (tcp-accept-ready? l))
         'accept (lambda ()
                  (let-values (((in out) (tcp-accept l)))
                     (vaquero-tcp-socket in out)))
         'close  (lambda () (tcp-close l) 'null)
      )
      '(ready? accept close)
      #f
      #f))

(define (vaquero-tcp-socket in out)
   (define-values (l-addr r-addr) (tcp-addresses in))
   (define-values (l-port r-port) (tcp-port-numbers in))
   (vaquero-object
      (list
         'type   '(tcp socket source sink stream)
         'view   (list 'socket l-addr l-port '-> r-addr r-port)
         'to-bool #t
         'local-addr l-addr
         'local-port l-port
         'remote-addr r-addr
         'remote-port r-port
         'close (lambda ()
                  (close-input-port in)
                  (close-output-port out)
                  'null)
      )
      '(read read-seq read-char peek-char read-line read-lines read-text nl ready? close)
      (list
         (list in 'read 'read-seq 'read-char 'peek-char 'read-line 'read-lines 'read-text 'ready?
                  'skip-n 'skip-while 'skip-until 'read-n 'read-while 'read-until)
         (list out 'write 'print 'say 'nl))
      #f))


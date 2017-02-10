
(define (vaquero-write obj port)
    (write (vaquero-view obj) port))

(define (vaquero-print obj port)
    (display (vaquero-view obj) port))

(define (vaquero-read port)
    (define first-rune (peek-char port))
    (if (eof-object? first-rune)
        first-rune
        (vaquero-parse (vaquero-reader port))))

(define (vaquero-reader port)
    (define token (peek-char port))
    (if (char-whitespace? token)
        (let ((_ (read-char port)) (next (peek-char port)))
            (if (eof-object? next)
                next
                (vaquero-reader port)))
        (case token
            ((#\()
                (let ((t (read-char port)) (peek-a-boo (peek-char port)))
                    (cond
                        ((eq? peek-a-boo #\))
                            (read-char port)
                            '())
                        (else
                            (let ((head (vaquero-reader port)))
                                (if (keyword? head)
                                    (let ((kw (keyword->symbol head)))
                                        (case kw
                                            ((rune)     (vaquero-rune-literal port))
                                            ((vector)   (vaquero-read-vector port))
                                            ((record)   (vaquero-read-record port))
                                            ((text)     (vaquero-read-text port))
                                            ((template) (vaquero-read-template port))
                                            ((doc)      (vaquero-read-rem port) (vaquero-reader port))
                                            ((rem)      (vaquero-read-rem port) (vaquero-reader port))
                                            (else       (cons head (vaquero-read-list port)))))
                                    (cons head (vaquero-read-list port))))))))
            ((#\)) (vaquero-error "read error: unexpected \")\"!\n"))
            ((#\') (vaquero-read-quote port))
            ((#\` #\%) (vaquero-read-quasiquote port))
            ((#\, #\$) (vaquero-read-unquote port))
            ((#\@) (vaquero-read-unquote-splicing port))
            ((#\\) (vaquero-read-rune port))
            ((#\#) (vaquero-read-matrix port))
            ((#\;) (vaquero-read-comment port) (vaquero-read port))
            ((#\|) (vaquero-read-funky port))
            (else (read port)))))

(define (vaquero-read-pair port)
    (define xs (vaquero-read-list port))
    (cons (car xs) (cadr xs)))

(define (vaquero-read-list port)
    (let loop ((token (peek-char port)) (acc '()))
        (cond
            ((eof-object? token)
                (vaquero-error "read error: unexpected EOF in unterminated list!\n"))
            ((char-whitespace? token)
                (read-char port)
                (loop (peek-char port) acc))
            ((eq? token #\.)
                (read-char port)
                (let ((x (car (vaquero-read-list port))))
                    (if (null? acc)
                        x
                        (let loop ((head (car acc)) (tail (cdr acc)) (end x))
                            (if (null? tail)
                                (cons head end)
                                (loop (car tail) (cdr tail) (cons head end)))))))
            ((eq? token #\))
                (read-char port)
                (reverse acc))
            ((eq? token #\;)
                (vaquero-read-comment port)
                (loop (peek-char port) acc))
            (else
                (let ((new-acc (cons (vaquero-reader port) acc)))
                    (loop (peek-char port) new-acc))))))

(define (vaquero-read-vector port)
    (list->vector (vaquero-read-list port)))

(define (vaquero-read-matrix port)
    (read-char port)
    'matrix)

(define (vaquero-read-record port)
    (apply vaquero-record (vaquero-read-list port)))

(define (vaquero-read-text port)
    (let loop ((token (peek-char port)) (depth 0) (acc '()))
        (cond
            ((eof-object? token)
                (vaquero-error "read error: unexpected EOF in text literal!\n"))
            ((eq? token #\()
                (let ((new-acc (cons (read-char port) acc)))
                    (loop (peek-char port) (+ depth 1) new-acc)))
            ((eq? token #\))
                (if (zero? depth)
                    (begin
                        (read-char port)
                        (string-trim-both (list->string (reverse acc))))
                    (begin
                        (let ((new-acc (cons (read-char port) acc)))
                            (loop (peek-char port) (- depth 1) new-acc)))))
            (else
                (let ((new-acc (cons (read-char port) acc)))
                    (loop (peek-char port) depth new-acc))))))

(define (vaquero-read-template port)
    (define (get-str xs)
        (list->string (reverse xs)))
    (define (read-interpol port)
        (let loop ((token (peek-char port)) (acc '()))
            (cond
                ((eof-object? token)
                    (vaquero-error "read error: unexpected EOF in template literal!\n"))
                ((eq? token #\})
                    (read-char port)
                    (if (eq? #\} (peek-char port))
                        (begin
                            (read-char port)
                            (vaquero-reader (open-input-string (get-str acc))))
                        (let ((t (read-char port)))
                            (loop (peek-char port) (cons t (cons #\} acc))))))
                (else
                    (let ((t (read-char port)))
                        (loop (peek-char port) (cons t acc)))))))
    (define depth 0)
    (let loop ((t (peek-char port)))
        (if (char-whitespace? t)
            (begin
                (read-char port)
                (loop (peek-char port)))
            #f))
    (let loop ((token (peek-char port)) (acc '()) (texts '()))
        (cond
            ((eof-object? token)
                (vaquero-error "read error: unexpected EOF in template literal!\n"))
            ((eq? token #\{)
                (read-char port)
                (if (eq? #\{ (peek-char port))
                    (begin
                        (read-char port)
                        (let ((txt (get-str acc))
                              (symbol (read-interpol port)))
                            (loop (peek-char port) '() (cons symbol (cons txt texts)))))
                    (let ((t (read-char port)))
                        (loop (peek-char port) (cons t (cons #\{ acc)) texts))))
            ((eq? token #\()
                (set! depth (+ depth 1))
                (let ((t (read-char port)))
                    (loop (peek-char port) (cons t acc) texts)))
            ((eq? token #\))
                (if (= depth 0)
                    (begin
                        (read-char port)
                        (cons 'cat (reverse (cons (string-trim-right (get-str acc)) texts))))
                    (begin
                        (set! depth (- depth 1))
                        (let ((t (read-char port)))
                            (loop (peek-char port) (cons t acc) texts)))))
            (else
                (let ((t (read-char port)))
                    (loop (peek-char port) (cons t acc) texts))))))
        

(define (vaquero-read-quote port)
    (read-char port)
    (list 'quote (vaquero-reader port)))

(define (vaquero-read-quasiquote port)
    (read-char port)
    (list 'qq (vaquero-reader port)))

(define (vaquero-read-unquote port)
    (read-char port)
    (list 'unq (vaquero-reader port)))

(define (vaquero-read-unquote-splicing port)
    (read-char port)
    (list 'unqs (vaquero-reader port)))

(define (vaquero-read-rune port)
    (read-char port)
    (let ((next (peek-char port)))
        (case next
            ((#\( #\)) (read-char port))
            (else
                (if (char-alphabetic? next)
                    (let ((sym (read port)))
                        (case sym
                            ((lf) #\newline)
                            ((cr) #\return)
                            ((space) #\space)
                            ((tab) #\tab)
                            (else (string-ref (symbol->string sym) 0))))
                    (read-char port))))))

(define (vaquero-rune-literal port)
    (define xs (vaquero-read-list port))
    (define rune (car xs))
    (if (= 1 (string-length rune))
        (string-ref rune 0)
        (cond
            ((equal? rune "space") #\space)
            ((equal? rune "tab") #\tab)
            ((equal? rune "lf") #\newline)
            ((equal? rune "cr") #\return))))

(define (vaquero-read-comment port)
    (read-line port)
    'null)

(define (vaquero-read-rem port)
    (vaquero-read-list port)
    'null)

(define (vaquero-read-funky port) ; hackity-hack
    (read-char port)
    (let ((next (peek-char port)))
        (if (eq? next #\\)
            (begin
                (read-char port)
                (let ((rune (read-char port)))
                    (read-char port)
                    rune))
            (vaquero-reader port))))

(define (vaquero-parse form)
	(define (desc form mt)
		(descend form (car mt) (cdr mt)))
    (define order
        (list
            (doterator)))
	(define atomized
		(let loop ((f form) (fns order))
			(if (eq? fns '())
				f
				(loop (desc f (car fns)) (cdr fns)))))
	atomized)

(define (warp form match? transform)
	(if (match? form)
		(let ((changed (transform form)))
			(if (equal? form changed)
				changed
				(begin 
					;(display form) (display " -> ") (display changed) (newline) (newline)
					changed)))
		form))

(define (descend form match? transform)
	(define (curses x) (descend x match? transform))
	(define newform (warp form match? transform))
	(if (pair? newform)
		(cons (curses (car newform)) (curses (cdr newform)))
		newform))

(define (doterator)
    ; foo.bar.baz.bax -> (send (send (send foo 'bar) 'baz) 'bax)
    (define (match? x)
        (and
            (symbol? x)
            (let ((s (symbol->string x)))
                (and
                    (not (string-contains s "/"))
                    (string-contains s ".")))))
    (define (transform x)
        (define (sym-or-num x)
            (define the-num (string->number x))
            (if the-num
                the-num
                (string->symbol x)))
        (let* (
            (str (symbol->string x))
            (words (string-split str ".")))
            (let loop ((this (sym-or-num (car words))) (left (cdr words)))
                (if (eq? left '())
                    this
                    (loop (list 'send this `(quote ,(sym-or-num (car left)))) (cdr left))))))
    (cons match? transform))


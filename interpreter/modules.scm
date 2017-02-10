
(define vaquero-modules (mkht))

(define (def-vaquero-module path)
    (define has? (hte? vaquero-modules path))
    (if has?
        #f
        (let ((expanded (read-expand-cache-prog path (local-env))))
            (hts! vaquero-modules path 'loading)
            (vaquero-eval-module expanded path))))

(define (vaquero-eval-module program path)
    (define nop (lambda args 'null))
    (if (pair? program)
        (let ((mods (find-modules program)))
            (map def-vaquero-module mods)
            (hts!
                vaquero-modules
                path
                (vaquero-compile program)))
        (hts! vaquero-modules path nop)))

(define (find-modules prog)
    (define (finder prog xs)
        (let loop ((form (car prog)) (rest (cdr prog)) (mods xs))
            (if (pair? form)
                (case (car form)
                    ((quote)
                        (if (pair? rest)
                            (finder rest mods)
                            mods))
                    ((use)
                        (let ((numods (cons (caddr form) mods)))
                            (if (pair? rest)
                                (loop (car rest) (cdr rest) numods)
                                numods)))
                    (else
                        (let ((numods (finder form mods)))
                            (if (pair? rest)
                                (finder rest numods)
                                numods))))
                (if (pair? rest)
                    (loop (car rest) (cdr rest) mods)
                    mods))))
    (delete-duplicates (finder prog '())))


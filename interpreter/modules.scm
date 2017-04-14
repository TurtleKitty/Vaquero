
(define vaquero-modules (mkht))

(define (def-vaquero-module path)
    (define has? (hte? vaquero-modules path))
    (if has?
        #f
        (let ((expanded (read-expand-cache-prog path (local-env))))
            (hts! vaquero-modules path will-exist)
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
   (define (finder form rest mods)
      (define (finish modz)
         (if (pair? rest)
            (finder (car rest) (cdr rest) modz)
            modz))
      (if (pair? form)
         (case (car form)
            ((quote)
               (finish mods))
            ((use)
               (let ((numods (cons (caddr form) mods)))
                  (finish numods)))
            (else
               (let ((numods (finder (car form) (cdr form) mods)))
                  (finish numods))))
         (finish mods)))
    (delete-duplicates (finder (car prog) (cdr prog) '())))


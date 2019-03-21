
; CHICKEN!

(define usage-text #<<END

Usage:

vaquero repl
vaquero eval "<code string>"
vaquero run <filename>
vaquero check <filename>
vaquero expand <filename>
vaquero compile <filename>
vaquero clean

vaquero <filename> is shorthand for vaquero run <filename>

END
)

(define (usage)
   (display usage-text)
   (newline)
   (exit))

(include "runtime")
(include "repl")
(include "start")

(start)


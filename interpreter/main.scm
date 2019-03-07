
; CHICKEN!

(define (##sys#pathname-resolution x f) (f x))  ; workaround for currently-broken unix sockets

(use srfi-1)
(use srfi-13)
(use srfi-69)

(use http-client)
(use medea)
(use numbers)
(use openssl)
(use posix)
(use symbol-utils)
(use tcp)
(use unix-sockets)
(use utf8)
(use utils)
(use uuid)
(use vector-lib)

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


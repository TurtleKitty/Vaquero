
; CHICKEN!

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


; start

(define usage-text #<<END

Usage:

vaquero repl
vaquero eval "<code string>"
vaquero run <filename>
vaquero check <filename>
vaquero expand <filename>
vaquero compile <filename>
vaquero clean

END
)

(define (usage)
    (display usage-text)
    (newline)
    (exit))

(define top-cont identity)
(define top-err
    (lambda (ex continue)
        (debug 'runtime-error
            (if (and (hash-table? ex) (eq? (vaquero-send-atomic ex 'type) 'error))
                (map (lambda (f) (vaquero-view (vaquero-send-atomic ex f))) '(name form to-text))
                (vaquero-view ex)))
        (exit)))

(define *cwd* (current-directory))
(define *use-cache* #t)
(define vaquero-use-symbols   "~/.vaquero/symbols.vaq")
(define vaquero-mod-dir       "~/.vaquero/modules")
(define vaquero-expanded-dir  "~/.vaquero/expanded")
(define vaquero-compiled-dir  "~/.vaquero/compiled")

(define genv #f)
(define g-has? #f)
(define g-get  #f)

(define load-symbols-env #f)
(define loaded-module-symbols)

(include "read_expand_cache")
(include "utils")
(include "reader")
(include "syntax_checker")
(include "objects")
(include "send")
(include "macro_expander")
(include "eval_apply")    
(include "compiler")
(include "sys")
(include "env")
(include "modules")
(include "repl")
(include "start")

(start)



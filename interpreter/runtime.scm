
(import-for-syntax (chicken io))

(import srfi-1)
(import srfi-13)
(import srfi-69)

(import http-client)
(import openssl)
(import posix-groups)
(import r7rs)
(import symbol-utils)
(import system-information)
;(import unix-sockets)
(import tcp6)
(import udp6)
(import utf8)
(import vector-lib)

(import (chicken bitwise))
(import (chicken condition))
(import (chicken file))
(import (chicken file posix))
(import (chicken format))
(import (chicken io))
(import (chicken irregex))
(import (chicken keyword))
(import (chicken pathname))
(import (chicken pretty-print))
(import (chicken process))
(import (chicken process signal))
(import (chicken process-context))
(import (chicken process-context posix))
(import (chicken random))
(import (chicken sort))
(import (chicken string))
(import (chicken time))


(define top-cont identity)

(define top-err
   (lambda (ex continue)
      (define runtime-error
         (if (vaquero-error? ex)
            (map (lambda (f) (vaquero-view (vaquero-send-atomic ex f))) '(name form to-text))
            (vaquero-view ex)))
      (vaquero-error 'runtime-error runtime-error "Error while executing program.")))

(define *cwd* (current-directory))
(define *use-cache* #t)
(define user-home-dir (vector-ref (user-information (current-user-id) #t) 5))

(define (vaquero-cache-dir dir)
   (string-join (list user-home-dir ".vaquero" dir) "/"))

(define cached-global-prelude-path (vaquero-cache-dir "prelude.vaq"))
(define vaquero-mod-dir            (vaquero-cache-dir "modules"))
(define vaquero-expanded-dir       (vaquero-cache-dir "expanded"))
(define vaquero-compiled-dir       (vaquero-cache-dir "compiled"))

(define genv #f)
(define g-has? (lambda (name) #f))
(define g-get  (lambda (name) not-found))
(define running-program-name 'null)

(include "read_expand_cache")
(include "utils")
(include "reader")
(include "syntax_checker")
(include "objects")
(include "send")
(include "macro_expander")
(include "eval_apply")
(include "compiler")
(include "modules")
(include "sockets")
(include "sys")


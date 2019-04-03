
(define-record-type vaq-obj
   (vaquero-udf fields autos forwards default)
   vaquero-object?
   (fields vaquero-obj-fields)
   (autos vaquero-obj-autos)
   (forwards vaquero-obj-forwards)
   (default vaquero-obj-default vaquero-obj-set-default!))

(define (vaquero-object-get-message fields forwards msg)
   (if (hte? fields msg)
      (htr fields msg)
      (if (hte? forwards msg)
         (htr forwards msg)
         #f)))

(define (default-udf-type this fields forwards)
   (define type (vaquero-object-get-message fields forwards 'type))
   (if type
      (if (and (list? type) (every symbol? type))
         'cool
         (vaquero-warning 'improper-type `(object 'type ,type) "An object type must be a list of symbols."))
      (hts! fields 'type '(object))))

(define (default-udf-view this fields forwards)
   (define autos (vaquero-obj-autos this))
   (define has-view (vaquero-object-get-message fields forwards 'view))
   (if has-view
      (let ((view-obj has-view))
         (define view-obj-messages (vaquero-send-atomic view-obj 'messages))
         (define has-apply  (member 'apply view-obj-messages))
         (define has-arity  (member 'arity view-obj-messages))
         (define (wrong)
            (vaquero-warning 'view-must-be-a-thunk `(object 'view ,(vaquero-view view-obj)) "The 'view message of a user-defined object must be a thunk."))
         (if (and has-apply has-arity)
            (let ((zero-arity (= 0 (vaquero-send-atomic view-obj 'arity))))
               (if zero-arity
                  'cool
                  (wrong)))
            (wrong)))
      (let ()
         (define type (car (htr fields 'type)))
         (define messages (append '(answers? autos default messages to-bool to-text view) (htks fields) (htks forwards)))
         (hts! fields 'view (lambda () (apply vector (cons type messages))))))
   (hts! autos 'view #t))

(define (default-udf-idk this)
   (vaquero-proc
      primitive-type
      'object
      (lambda (args opts cont err)
         (idk this (car args) cont err))))

(define (vaquero-set-udf-defaults this)
   (define fields   (vaquero-obj-fields this))
   (define forwards (vaquero-obj-forwards this))
   (define default  (vaquero-obj-default this))
   (default-udf-type this fields forwards)
   (default-udf-view this fields forwards)
   (vaquero-obj-set-default! this (or default (default-udf-idk this))))

(define (vaquero-object args autos forwards initial)
   (define fields (mkht))
   (define delegates (mkht))
   (define autoexec (mkht))
   (define (fset! k v)
      (hts! fields k v))
   (define (aset! k)
      (hts! autoexec k #t))
   (define (rset! k v)
      (hts! delegates k v))
   (define (set-forward! rlist)
      (let ((delegate (car rlist)) (msgs (cdr rlist)))
         (map (lambda (msg) (rset! msg delegate)) msgs)))
   (for-pairs fset! args)
   (if forwards
      (map set-forward! forwards))
   (if autos
      (map aset! autos))
   (let ((this (vaquero-udf fields autoexec delegates initial)))
      (vaquero-set-udf-defaults this)
      this))


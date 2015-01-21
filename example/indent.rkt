'(a b
  (a b
   c))

'((1) 2 3
  (3)
  4 5)

{a h
 b '(a x
     s (x y
        x v))}

`(Part ()
  (PartNumber ()
   ,part)
  (ETag ()
   ,etag))

`((,(x)
   ,y))

(foobar x
        x
        y)

(dict-set a
          b
          c)

(let ([x 0])
  x)

(cond [1 2]
      [3 4])

(with-handlers ([x y])
  a b c)

#(a b
  c d)

{a b
 c d}

(#:x y
 #:y x)

;;; Bug #50

'((x
   y) A
  z
  (x
   y) A
  z)

(match args
  [(list x) (x
             y)] ...
  [(list x) (x y)] ...
  [(list x) (x y)] ...)

(define-syntax (fstruct stx)
  (syntax-parse stx
    [(_ id:id (field:id ...))
     (with-syntax ([(accessor ...)
                    (for/list ([fld (in-list (syntax->list #'(field ...)))])
                      (format-id stx "~a-~a" (syntax->datum #'id) fld))])
       #'(serializable-struct
          id (field ...) #:transparent
          #:property prop:procedure
          (lambda (self . args)
            (match args
              [(list 'field) (accessor self)] ...
              [(list (list 'field)) (accessor self)] ...
              [(list (list-rest 'field fields)) ((accessor self) fields)] ...
              [(list-rest 'field f args)
               (struct-copy id self
                            [field (apply f (accessor self) args)])] ...
              [(list-rest (list 'field) f args)  ;<-- THIS SEXPR IS INDENTED TOO FAR
               (struct-copy id self
                            [field (apply f (accessor self) args)])] ...
              [(list-rest (list-rest 'field fields) args)
               (struct-copy id self
                            [field (apply (accessor self) fields args)])] ...))))]))

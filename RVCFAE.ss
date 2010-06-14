;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:20/lang/reader)
(define-type CFAES
  (num (n number?))
  (add (lhs CFAES?) (rhs CFAES?))
  (sub (lhs CFAES?) (rhs CFAES?))
  (id (name symbol?))
  (fun (param symbol?) (body CFAES?))
  (refun (param symbol?) (body CFAES?))  
  (app (fun-expr CFAES?) (arg-expr CFAES?))
  (if0 (condition CFAES?) (yes CFAES?) (no CFAES?))
  (set (var symbol?) (value CFAES?))
  (seq (exprs (listof CFAES?))))

(define-type CFAES-Value
  (numV (n number?))
  (closureV (param symbol?)
            (body CFAES?)
            (ds Env?))
  (refclosV (param symbol?)
            (body CFAES?)
            (ds Env?)))

(define-type Env
  (mtSub)
  (aSub (id symbol?)
        (value number?)
        (ds Env?)))

(define-type Store
  (mtSto)
  (aSto (location number?)
        (value CFAES-Value?)
        (store Store?)))

(define-type Value*Store
  (v*s (value CFAES-Value?) (store Store?)))

(define lookup
  (lambda (id ds)
    (type-case Env ds
       (mtSub () (error 'lookup "identifier ~s not found" id))
       (aSub (bound-id bound-value ds)
             (if (symbol=? id bound-id)
                 bound-value
                 (lookup id ds))))))

(define store-lookup
  (lambda (loc-index sto)
    (type-case Store sto
      (mtSto () (error 'store-lookup "no value at location"))
      (aSto (location value rest-store)
            (if (= location loc-index)
                value
                (store-lookup loc-index rest-store))))))

(define (next-location store)
  (type-case Store store
             (mtSto () 1 )
             (aSto (location value stor)
                   (+ 1 location))))

(define (parse-exprs lof-expr)
  (if (empty? lof-expr) empty
      (cons (parse (first lof-expr)) (parse-exprs (cdr lof-expr)))))

(define (parse sexp)
  (cond
    ((number? sexp) (num sexp))
    ((symbol? sexp) (id sexp))    
    ((list? sexp)
     (case (first sexp)
       ((+) (add (parse (second sexp))
                 (parse (third sexp))))
       ((-) (sub (parse (second sexp))
                 (parse (third sexp))))
       ((if0) (if0 (parse (second sexp))
                  (parse (third sexp))
                  (parse (fourth sexp))))
       ((fun) (fun (first (second sexp))
                   (parse (third sexp))))     
       ((refun) (refun (first (second sexp))
                       (parse (third sexp))))
       ((with) (app (fun (first (second sexp))
                         (parse (third sexp)))
                    (parse (second (second sexp)))))
       ((set) (set (cadr sexp) (parse (caddr sexp))))
       ((seq) (seq (parse-exprs (cdr sexp))))
       (else (app (parse (first sexp)) (parse (second sexp))))))))

(define (sub-numbers l r)
  (numV (- (numV-n l) (numV-n r))))

(define (add-numbers l r)
  (numV (+ (numV-n l) (numV-n r))))

(define (interp expr env store)
  (type-case CFAES expr
     (num (n) (v*s (numV n) store))
     (add (l r) 
          (type-case Value*Store (interp l env store)
                     (v*s (l-value l-store)
                          (type-case Value*Store (interp r env l-store)
                                     (v*s (r-value r-store)
                                          (v*s (add-numbers l-value r-value) r-store))))))
     (sub (l r) 
          (type-case Value*Store (interp l env store)
                     (v*s (l-value l-store)
                          (type-case Value*Store (interp r env l-store)
                                     (v*s (r-value r-store)
                                          (v*s (sub-numbers l-value r-value) r-store))))))
     (id (v) (v*s (store-lookup (lookup v env) store) store))
     (if0 (c t e) (type-case Value*Store (interp c env store)
                             (v*s (if-value if-store)
                                  (type-case Value*Store (interp t env if-store)
                                             (v*s (t-value t-store)
                                                  (type-case Value*Store (interp e env if-store)
                                                             (v*s (e-value e-store)
                                                                  (if (= (numV-n if-value) 0) t-value e-value))))))))
     (fun (bound-id bound-body) (v*s (closureV bound-id bound-body env) store))
     (refun (bound-id bound-body) (v*s (refclosV bound-id bound-body env) store))
     (app (fun-expr arg-expr)
          (type-case Value*Store (interp fun-expr env store)          
                     (v*s (fun-value fun-store)
                          (type-case CFAES-Value fun-value
                                     (closureV (cl-param cl-body cl-env)
                                               (type-case Value*Store (interp arg-expr env fun-store)
                                                          (v*s (arg-value arg-store)
                                                               (local ((define new-loc (next-location arg-store)))
                                                                 (interp cl-body
                                                                         (aSub cl-param
                                                                               new-loc
                                                                               cl-env)
                                                                         (aSto new-loc
                                                                               arg-value
                                                                               arg-store))))))
                                     (refclosV (cl-param cl-body cl-env)
                                               (local ((define arg-loc (lookup (id-name arg-expr) env)))
                                                 (interp cl-body
                                                         (aSub cl-param
                                                               arg-loc
                                                               cl-env)
                                                         fun-store)))
                                     (numV (_) (error 'interp "trying to apply a number"))))))
;     (app (fun-expr arg-expr)
;          (type-case Value*Store (interp fun-expr env store)
;                     (v*s (fe-value fe-store)
;                          (type-case Value*Store (interp arg-expr env fe-store)
;                                     (v*s (ae-value ae-store)
;                                          (local ((define new-loc (next-location ae-store)))
;                                            (interp (closureV-body fe-value)
;                                                    (aSub (closureV-param fe-value)
;                                                          new-loc
;                                                          (closureV-ds fe-value))
;                                                    (aSto new-loc
;                                                          ae-value
;                                                          ae-store))))))))
     (set (var value)
          (type-case Value*Store (interp value env store)
                     (v*s (value-value value-store)
                          (local ((define the-loc (lookup var env)))
                            (v*s value-value
                                 (aSto the-loc value-value value-store))))))
     (seq (lof-expr) 
          (if (= 1 (length lof-expr))
              (interp (first lof-expr) env store)
              (type-case Value*Store (interp (first lof-expr) env store)
                     (v*s (e1-value e1-store)
                          (interp (seq (cdr lof-expr)) env e1-store)))))))
          

          
          


(define eval-CFAES
  (lambda (expr)
    (type-case Value*Store (interp (parse expr) (mtSub) (mtSto))
      (v*s (return-value final-store)
           return-value))))

;(test (interp (parse '5)) 5)
;(test (interp (parse '{+ 5 5})) 10)
;(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) 20)
;(test (interp (parse '{with {x 5} {+ x x}})) 10)
;(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
;(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
;(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
;(test (interp (parse '{with {x 5} {with {y x} y}})) 5)
;(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
;(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
;(test (interp (parse '{with {x 5} {with {x x} x}})) 5)

(interp (parse (read)) (mtSub) (mtSto))
;(eval-CFAES (read))
;(interp (parse (read)))


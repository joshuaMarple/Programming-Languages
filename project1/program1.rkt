#lang eopl
(require srfi/1)

(define (empty-env) '())

(define (extend-env vars vals e) (cons (cons vars vals) e))

(define (apply-env e v)
  (when (pair? e)
   (let ((vars (caar e)) (vals (cdar e)) (env (cdr e)))
     (if (memq v vars)
         (list-ref vals (list-index (lambda (x) (equal? v x)) vars))
         (apply-env env v)))))

(define (list-of v?)
  (lambda (l) (every v? l)))

(define-datatype expression expression?
  (const-exp (num number?))
  (var-exp (id symbol?))
  (lambda-exp (id (list-of symbol?)) (body expression?))
  (app-exp (rator expression?) (rand (list-of expression?)))
  (prim-app-exp (prim primitive?) (rand (list-of expression?))))
  
(define-datatype primitive primitive?
  (add1-prim)
  (minus-prim)
  (plus-prim)
  (sub-prim)
  (mult-prim)
  (div-prim))

(define (parse-prim x)
  (cond ((eq? x 'add1) (add1-prim))
        ((eq? x 'minus) (minus-prim))
        ((eq? x '+) (plus-prim))
        ((eq? x '-) (sub-prim))
        ((eq? x '*) (mult-prim))
        ((eq? x '/) (div-prim))))

(define (parse-expression x)
  (cond ((number? x) (const-exp x))
        ((symbol? x) (var-exp x))
        ((and (pair? x) (eq? (car x) 'lambda))
         (lambda-exp (cadr x) (parse-expression (caddr x))))
        ((and (pair? x) (primitive? (parse-prim (car x))))
         (prim-app-exp (parse-prim (car x)) (map parse-expression (cdr x))))
        ((pair? x)
         (app-exp (parse-expression (car x)) (map parse-expression (cdr x))))
        (else (eopl:error "Error in parse-expression"))))

(define(value-of x e)
  (cases expression x
    (lambda-exp (id body)
                (lambda (val) (value-of body(extend-env id val e))))
    (app-exp (rator rand)
             ((value-of rator e) (map (lambda(n) (value-of n e)) rand )))
    (const-exp (n) n)
    (var-exp (id) (apply-env e id))
    (prim-app-exp (prim rand)
                  (apply (value-of-prim prim) (map (lambda(n) (value-of n e)) rand)))
    (else (eopl:error 'ap-eval "Error in value-of"))))

(define (add1 n) (+ n 1))
(define (minus n) (- n))

(define(value-of-prim p)
  (cases primitive p
    (add1-prim () add1)
    (minus-prim () minus)
    (plus-prim () +)
    (sub-prim () -)
    (mult-prim () *)
    (div-prim () /)))

(value-of (parse-expression '((lambda (n) (add1 ((lambda (x) (+ x 5)) n))) 10)) '())
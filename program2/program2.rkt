#lang eopl
(require srfi/1)
(require racket/trace)

(define empty-env '())

(define (extend-env vars vals e) (cons (cons vars vals) e))

(define(extend-env-rec vars bound-varss bodies env)
  (define e(lambda(v) (if(memq v vars)
           (procedure(list-ref bound-varss(-(length vars)(length(memq v vars))))
                     (list-ref bodies(-(length vars)(length(memq v vars))))e)
           (extend-env v))))
  e)

(define (apply-env e v)
  (if (null? e)
      (eopl:error 'ap-eval "Error in apply-env")
      (let ((vars (caar e)) (vals (cdar e)) (env (cdr e)))
        (if (eq? v vars)
            vals
            (apply-env env v)))))

(define (list-of v?)
  (lambda (l) (every v? l)))

(define init-env
    (extend-env
     'i +i
     (extend-env
      'pi 3.14159265359
      (extend-env
       'e (exp 1)
       empty-env))))

(define grammar 
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (symbol) var-exp)
    (expression ("proc" "(" (separated-list symbol ",") ")" expression) proc-exp)
    (expression ("dy-proc" "(" (separated-list symbol ",") ")" expression) dy-proc-exp)
    (expression ("(" expression (arbno expression) ")" ) app-exp)
    (expression (primitive "("(separated-list expression ",") ")") prim-app-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" symbol "=" expression "in" expression) let-exp)
    (expression ("letrec"
                (arbno symbol "(" (separated-list symbol ",") ")" "=" expression )"in" expression) letrec-exp)
    (primitive ("add1") add1-prim)
    (primitive ("minus") minus-prim)
    (primitive ("+") plus-prim)
    (primitive ("-") sub-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("zero?") zero?-prim)
    (primitive ("=") equal-prim)
    (primitive ("<") <-prim)
    (primitive (">") >-prim)
    (primitive ("^") ^-prim)
    (primitive ("proc?") proc?-prim)
    (primitive ("number?") number?-prim)
    (primitive ("boolean?") bool?-prim)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (symbol (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(sllgen:make-define-datatypes scanner-spec grammar)
(define string-parser (sllgen:make-string-parser scanner-spec grammar))

(define(procedure ids body e)

  (lambda(dy-e)

   (lambda vals(value-of body(extend-env ids vals e)))))

(define(value-of x e)
  (cases expression x
    #|(lambda-exp (id body)
                (lambda (val) (value-of body (extend-env id val e))))|#
    (app-exp (rator rand)
             ((value-of rator e) (map (lambda(n) (value-of n e)) rand )))
    (const-exp (n) n)
    (var-exp (id) (apply-env e id))
    (proc-exp(ids body)(procedure ids body e))
    (dy-proc-exp (ids body)(dy-procedure ids body e))
    ;(call-exp(rator rands)(apply((value-of rator e)e)(value-of-list rands e)))
    ;(app-exp (rator rands)(apply((value-of rator e)e)(value-of-list rands e)))
    (prim-app-exp (prim rand)
                  (apply (value-of-prim prim) (map (lambda(n) (value-of n e)) rand)))
    (if-exp (w y z) (if (value-of w e) (value-of y e) (value-of z e)))
    (let-exp (symbol exp stuff) (value-of stuff (extend-env symbol (value-of exp e) e)))
    (letrec-exp(fid id body1 body)
             (value-of body(extend-env-rec fid id body1 e)))
    (else (eopl:error 'ap-eval "Error in value-of"))))

(define (minus n) (- n))
(define (add1 n) (+ n 1))

(define (value-of-prim p)
  (cases primitive p
    (add1-prim () add1)
    (minus-prim () minus)
    (plus-prim () +)
    (sub-prim () -)
    (mult-prim () *)
    (div-prim () /)
    (zero?-prim () zero?)
    (equal-prim () =)
    (<-prim () <)
    (>-prim () >)
    (^-prim () expt)
    (number?-prim () number?)
    (bool?-prim () boolean?)
    (proc?-prim () procedure?)))

(define (value-of-program p)
  (cases program p (a-program (x) (value-of x 
                                            init-env))))

(define(dy-procedure ids body static-e)
  (lambda(dy-e)
    (lambda vals (value-of body(extend-env ids vals dy-e)))))


(define (run s) (value-of-program (string-parser s)))

;; examples
(run "let x = 1 in +(x, 1)")
(run "let Y=proc(ff) (proc(h) (h h) proc(g) (ff proc(x) ((g g) x)) )
in 
let fact = (Y proc(f) proc(n) if zero?(n)
then 1
else *(n, (f-(n,1))))
in (fact 5)")


(run
 "letrec fac(x) = if zero?(x) then 1 else *(x,(fac -(x,1)))
               in (fac 5)")



#|(run
 "letrec fac(x) = x
               in (fac 5)")



(run "let fac = proc(g,n) if zero?(n)
                     then 1
                     else *(n,(g g -(n,1)))
               in (fac fac 5)")

(run "let Y = proc(ff)(proc(h)(h h)
                       proc(g)(ff proc(x)((g g) x)))
              in
                  let fac = (Y proc(f) proc(n) if zero?(n)
                                               then 1
                                               else *(n,(f -(n,1))))
                       in (fac 5)"
) |#
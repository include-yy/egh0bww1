#lang plait

(define-type Exp
  [numE (n : Number)]
  [boolE (b : Boolean)]
  [varE (s : Symbol)]
  [plusE (left : Exp) (right : Exp)]
  [cndE (test : Exp) (then : Exp) (else : Exp)]
  [let1E (var : Symbol) (value : Exp) (body : Exp)]
  [lamE (var : Symbol) (body : Exp)]
  [appE (fun : Exp) (arg : Exp)])

(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)]
  [funV (var : Symbol) (body : Exp) (nv : Env)])

(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [else (error 'if "expects conditional to evaluate to a boolean")]))

(define (interp e nv)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(lamE v b) (funV v b nv)]
    [(cndE c t e) (if (boolean-decision (interp c nv))
                      (interp t nv)
                      (interp e nv))]
    [(appE f a) (let ([fv (interp f nv)]
                      [av (interp a nv)])
                  (type-case Value fv
                    [(funV v b nv)
                     (interp b (extend nv v av))]
                    [else (error 'app "didn't get a function")]))]
    [(let1E var val body)
     (let ([new-env (extend nv
                            var
                            (interp val nv))])
       (interp body new-env))]))

(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty))

(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error s "not bound")]
    [(some v) v]))

(define (extend old-env new-name value)
  (hash-set old-env new-name value))

(define (parse s)
  (cond
    [(s-exp-number? s)
     (numE (s-exp->number s))]
    [(s-exp-boolean? s)
     (boolE (s-exp->boolean s))]
    [(s-exp-symbol? s)
     (varE (s-exp->symbol s))]
    [(s-exp-list? s)
     (let* ([l (s-exp->list s)]
            [fst (first l)])
       (if (s-exp-symbol? (first l))
           (cond
             [(symbol=? '+ (s-exp->symbol fst))
              (plusE (parse (second l)) (parse (third l)))]
             [(symbol=? 'lam (s-exp->symbol fst))
              (lamE (s-exp->symbol (second l)) (parse (third l)))]
             [(symbol=? 'let1 (s-exp->symbol fst))
              (let1E (s-exp->symbol (first (s-exp->list (second l))))
                     (parse (second (s-exp->list (second l))))
                     (parse (third l)))]
             [(symbol=? 'if (s-exp->symbol fst))
              (cndE (parse (second l))
                    (parse (third l))
                    (parse (fourth l)))]
             [(= (length l) 2)
              (appE (parse (first l))
                    (parse (second l)))]
             [else (error 'parse "list not a valid expression")])
           (if (= (length l) 2)
               (appE (parse (first l))
                     (parse (second l)))
               (error 'parse "application expression not correct"))))]))
         
(define (my-eval exp)
  (interp (parse exp) mt-env))

(print-only-errors #true)

(test (parse `{lam x {+ x x}})
      (lamE 'x (plusE (varE 'x) (varE 'x))))
(test (interp (let1E 'x (numE 1)
                     (let1E 'f (lamE 'y (varE 'x))
                            (let1E 'x (numE 2)
                                   (appE (varE 'f) (numE 10)))))
              mt-env)
      (numV 1))
(test (interp (appE (let1E 'x (numE 3)
			   (lamE 'y (plusE (varE 'x) (varE 'y))))
		    (numE '4))
	      mt-env)
      (numV 7))
(test (my-eval `(let1 (x 1)
                  (let1 (f (lam y x))
                    (let1 (x 2)
                      (f 10)))))
      (numV 1))
(test (my-eval `{let1 {x 1} {+ x x}}) (numV 2))
(test (my-eval `{let1 {x 1}
                  {let1 {y 2}
                    {+ x y}}})
      (numV 3))
(test (my-eval `{let1 {x 1}
                  {let1 {y 2}
                    {let1 {x 3}
                      {+ x y}}}})
      (numV 5))
(test (my-eval `{let1 {x 1}
                  {+ x
                     {let1 {x 2} x}}})
      (numV 3))
(test (my-eval `{let1 {x 1}
                  {+ {let1 {x 2} x}
                     x}})
      (numV 3))
(test (my-eval `{let1 {f {lam x {+ x x}}}
                  {f 3}})
      (numV 6))
(test (my-eval `{let1 {x 3}
                  {let1 {f {lam y {+ x y}}}
                    {f 3}}})
      (numV 6))
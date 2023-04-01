From posting-system@google.com Fri Sep 19 06:39:19 2003
Date: Thu, 18 Sep 2003 23:39:02 -0700
From: oleg@pobox.com (oleg@pobox.com)
Newsgroups: comp.lang.scheme
Subject: Self-application as the fixpoint of call/cc
Message-ID: <7eb8ac3e.0309182239.5a64b3b1@posting.google.com>
Status: OR

We shall show in which way (call/cc call/cc) is equivalent to (lambda
								  (x) (x x)). We shall show how to express Y via call/cc. The result has
a distinctive feature: Y can be expressed *without* resorting to an
explicit self-application. We shall also illustrate the use of
syntax-rules in proving theorems.

Theorem 1.
Expression
- ((call/cc ... (call/cc call/cc)) p)
- ((call/cc ... (call/cc (call/cc id))) p)
- ((lambda (x) (x x)) p)

(where p is a value) have beta-equivalent CBV CPS transforms.

Here (call/cc ...) signify zero or more applications of call/cc, and
id is the identity function (lambda (x) x).

In other words,

- (lambda (p)  ((call/cc ... (call/cc call/cc)) p))
- (lambda (p)  ((call/cc ... (call/cc (call/cc id))) p))
- (lambda (p)  ((lambda (x) (x x)) p))

are CBV observationally equivalent.


Illustration (using the higher-order syntax):

(define (p x) (if (eq? x p) '(p p) `(p ,x)))

((lambda (x) (x x)) p)
evaluates===> (p p)
((call/cc call/cc) p)
evaluates===> (p p)
((call/cc (call/cc call/cc)) p)
evaluates===> (p p)
((call/cc (call/cc (call/cc (call/cc (call/cc (lambda (x) x)))))) p)
evaluates===> (p p)

Catchy phrase 1.
self-application is the fixpoint of call/cc

Corollary 1. Y combinator via call/cc -- Y combinator without an
explicit self-application.

(define (Y f)
  ((lambda (u) (u (lambda (x) (lambda (n) ((f (u x)) n)))))
   (call/cc (call/cc (lambda (x) x)))))

Here, we used a fact that
((lambda (u) (u p)) (call/cc call/cc))
and
((lambda (u) (u p)) (lambda (x) (x x)))
are observationally equivalent.


Corollary 2. factorial via call/cc

(define fact
  (lambda (f) (lambda (n)
		(if (<= n 0) 1 (* n (f (- n 1)))))))

then, given the above definition of Y,
(list ((Y fact) 5) ((Y fact) 6) ((Y fact) 7))
does return
'(120 720 5040)

Additional reference:
http://google.com/groups?selm=7eb8ac3e.0302131759.4735faf3%40posting.google.com


To avoid possible misunderstandings, it's important to realize what
the theorem says, and specifically what it does not say. The theorem
says that ((call/cc call/cc) p) and ((lambda (x) (x x)) p) are
observationally equivalent in CBV provided that p is a value. The
theorem emphatically does <em>not</em> say that (call/cc call/cc) and
(lambda (x) (x x)) are observationally equivalent -- because they are
generally not. The equivalence holds only in specific contexts. One of
them is an an application to a value. The lemmas below should make
that clear. Here are a few examples:

(define (foo self)
  (if (procedure? self) (self #f)
      (begin (display "!") (newline))))

(define (exp1)
  (let ((f (call/cc call/cc)))
    (f (begin (display "OK") foo))))


(define (exp2)
  (let ((f (lambda (x) (x x))))
    (f (begin (display "OK") foo))))


(define (exp3)
  (let ((f (lambda (y) ((call/cc call/cc) y))))
    (f (begin (display "OK") foo))))

Evaluating (exp1) prints OKOK! whereas evaluating (exp2) and (exp3)
prints just OK!. Clearly, (exp1) can't be equivalent to (exp2).

That is, eta-expansion is not a sound operation when applied to
(call/cc call/cc). First-class continuations can be tricky. That's why
it helps to be formal.



Proof of Theorem 1.

We will be using the CPS transformation, the equations for which are taken
verbatim from Danvy and Filinski "Abstracting Control" (p. 2). CPS is
a source-to-source translation. Therefore, it behooves us to use
Scheme's syntax-transformers.

(define-syntax CPS
  (syntax-rules (lambda call/cc p)
    ((CPS (?e1 ?e2)) 			; application
     (lambda (k) ((CPS ?e1) (lambda (f) ((CPS ?e2) (lambda (a) ((f a) k)))))))
    ((CPS (lambda (x) ?e))		; abstraction
     (lambda (k) (k (lambda (x) (CPS ?e)))))
    ((CPS call/cc)
     (lambda (k0)
       (k0 (lambda (p) (lambda (k)
			 ((p (lambda (a) (lambda (k1) (k a))))
			  k))))))
    ((CPS p)     ; skolem constant for *any* value
     (lambda (k) (k pv)))
    ((CPS ?x)
     (lambda (k) (k ?x)))))

We will be using Petite Chez Scheme, which provides a form (expand e)
to macro-expand an expression e.

For example,

> (expand '(CPS (lambda (x) (x x))))
(lambda (#:k)
  (#:k (lambda (#:x)
         (lambda (#:k)
           ((lambda (#:k) (#:k #:x))
            (lambda (#:f)
              ((lambda (#:k) (#:k #:x))
               (lambda (#:a) ((#:f #:a) #:k)))))))))


We can see the problem -- there are too many lambdas, some of which
could be reduced. They are administrative lambdas. Because all our
expressions here will assuredly terminate, we can use the full
normalization, see Appendix.  The Appendix indeed defines the
normal-order lambda calculator. The particular algorithm
(normalization as a yacc-style parsing) is described in great detail
in
http://pobox.com/~oleg/ftp/Haskell/Lambda_calc.lhs

> (expand `(NORM ,(expand '(CPS (lambda (x) (x x))))))
(lambda (#:k)
  (#:k (lambda (#:x) (lambda (#:k) (#:x #:x #:k)))))

which is far smaller a term to look at. It indeed matches the
derivation by hand.

However, this nested `expand' isn't nice, let alone cumbersome to
type. We can integrate NORM into the CPS itself (doing essentially
						       deforestation).

(define-syntax CPS
  (syntax-rules (lambda call/cc p)
    ((CPS (?e1 ?e2) . args) 			; application
     (NORM (lambda (k) ((CPS ?e1) (lambda (f) ((CPS ?e2) (lambda (a)
							   ((f a) k)))))) . args))
    ((CPS (lambda (x) ?e) . args)		; abstraction
     (NORM (lambda (k) (k (lambda (x) (CPS ?e)))) . args))
    ((CPS call/cc . args)
     (NORM (lambda (k0)
	     (k0 (lambda (p) (lambda (k)
			       ((p (lambda (a) (lambda (k1) (k a)))) k)))))
	   . args))
    ((CPS p . args)     ; skolem constant for *any* value
     (NORM (lambda (k) (k pv)) . args))
    ((CPS ?x . args)
     (NORM (lambda (k) (k ?x)) . args))))

(define-syntax NORM
  (syntax-rules (lambda CPS)
    ((NORM t) (NORM t () ()))
    ((NORM (CPS e) env stack) (CPS e env stack))
    ((NORM (lambda (x) e) env ())
     (let-syntax ((ren (syntax-rules ()
			 ((ren ?x ?e ?env) (lambda (x) (NORM ?e ((?x () x) . ?env) ()))))))
       (ren x e env)))
    ((NORM (lambda (x) b) env ((enve e) . stack))
     (NORM b ((x enve e) . env) stack))
    ((NORM (e1 e2) env stack)
     (NORM e1 env ((env e2) . stack)))
    ((NORM x () ()) x)
    ((NORM x () ((enve e) ...))
     (x (NORM e enve ()) ...))
    ((NORM x env stack)
     (let-syntax
	 ((find
	   (syntax-rules (x)
	     ((find ?x ((x ?envs ?es) . _) ?stack) (NORM ?es ?envs ?stack))
	     ((find ?x (_ . ?env) ?stack) (NORM ?x ?env ?stack)))))
       (find x env stack)))
    ))

> (expand `(CPS (lambda (x) (x x))))
(lambda (#:k)
  (#:k (lambda (#:x) (lambda (#:k) (#:x #:x #:k)))))

Now we can see the results in any Scheme system that has even the
second-class macro-expander (e.g., Scheme48). We have added one rule
to NORM, to force unexpanded CPS...

Incidentally, we could distinguish between administrative and
real lambdas. The administrative are the ones introduced by the CPS
macro. We may choose a different name for those lambdas. The
normalizer will reduce only those, and treat real lambdas as black
boxes. But we will not do that here, because for our theorem, we need
to reduce some `serious' lambdas as well. We use this CPS as a proof
assistant anyway: if it takes too long, there is always Control-C.



Lemma 1.
CPS transform of ((lambda (x) (x x)) p) is (lambda (k) (pv pv k))
Proof:
> (expand '(CPS ((lambda (x) (x x)) p)))
(lambda (#:k) (pv pv #:k))

Lemma 2.
CPS transform of (call/cc call/cc) is
(lambda (k) (k (lambda (a) (lambda (k1) (k a)))))
Proof:
> (expand '(CPS (call/cc call/cc)))
(lambda (#:k)
  (#:k (lambda (#:a) (lambda (#:k1) (#:k #:a)))))

It indeed matches the derivation by hand.

Lemma 3.
CPS transform of (call/cc (call/cc call/cc)) is the same as
that of (call/cc call/cc).
Proof:
> (expand '(CPS (call/cc (call/cc call/cc))))
(lambda (#:k)
  (#:k (lambda (#:a) (lambda (#:k1) (#:k #:a)))))

Lemma 4.
CPS transform of (call/cc (call/cc id)) is the same as
that of (call/cc call/cc).
Proof:
> (expand '(CPS (call/cc (call/cc (lambda (u) u)))))
(lambda (#:k)
  (#:k (lambda (#:a) (lambda (#:k1) (#:k #:a)))))

Lemma 5.
CPS transform of ((call/cc call/cc) p) is the same as
that of ((lambda (x) (x x)) p)
Proof:
> (expand '(CPS ((call/cc call/cc) p)))
(lambda (#:k) (pv pv #:k))

and see Lemma 1.

Additional pairs of equivalences:

(expand '(CPS (lambda (u) ((lambda (x) (x x)) u))))
(expand '(CPS (lambda (u) ((call/cc call/cc) u))))

The common transform is:
(lambda (#:k)
  (#:k (lambda (#:u) (lambda (#:k) (#:u #:u #:k)))))


(expand '(CPS ((lambda (u) (u p)) (call/cc call/cc))))
(expand '(CPS ((lambda (u) (u p)) (lambda (x) (x x)))))

The common transform is:
(lambda (#:k) (pv pv #:k))


Proof of the main theorem.
Let p be a term, M be ((call/cc call/cc) p) and
N be ((lambda (x) (x x)) p).

By Plotkin's simulation theorem,

CPS_transform_v{eval_v{M}} = eval_v{CPS_transform{M} (lambda (x) x)}

where '=' means observational equivalence. By Lemma 5,
CPS_transform{M} is identical to CPS_transform{N}. Therefore,

CPS_transform_v{eval_v{M}}  = CPS_transform_v{eval_v{N}}
QED.

The proof of the `fixpoint' follows from Lemmas 2-4 under trivial
induction.

In more detail about the observational equivalence: if C[] is a CBV
context, and N is a term and C[N] -*>v V (term C[N] reduces to a value
					       in CBV), then (CPS_transform{C[N]} \x.x) evaluates to a value under any
strategy. Conversely, if C[N] does not evaluate to a value under CBV,
neither will (CPS_transform{C[N]} \x.x) evaluate to one under any
strategy (see Theorem 2 of Danvy/Filinski). Note that
CPS_transform{C[N]} = CPS_transform{N} k
(see the equation at the end of page 9 and beginning page 10, Section
     2.5 of Danvy/Filinski). So, terms that have identical CPS transforms
are CBV observationally equivalent (that is, have the same terminating
					 behavior when plugged into any CBV context).



----

Appendix. The Lambda-calculator as a simple Scheme syntax-rule macro.


The first attempt:

					;(define-syntax NORM
					;  (syntax-rules ()
					;    ((NORM . args)
					;      (mtrace (NORM1 . args)))))

(define-syntax NORM
  (syntax-rules (lambda)
    ((NORM e) (NORM e ()))
    ((NORM (lambda (x) e) ())
     (lambda (x) (NORM e ())))
    ((NORM t ((lambda x) . stack)) ; lambda as a high-priority application
     (NORM (lambda (x) t) stack))
    ((NORM (lambda (x) b) (e . stack)) ; redex
     (letrec-syntax
	 ((ren
	   (syntax-rules ()
	     ((ren ynew y z ?x ?e ?stack)
	      (let-syntax
	          ((alpha
		    (syntax-rules ()
		      ((alpha y ??e)
		       (beta z ?x ??e ((lambda y) . ?stack))))))
		(alpha ynew ?e)))))
	  (beta
	   (syntax-rules (lambda x)
	     ((beta x ?x ?e ?stack) (NORM ?e ?stack))
	     ((beta (lambda (x) z) ?x ?e ?stack)
	      (NORM (lambda (?x) z) ?stack))
	     ((beta (lambda (y) z) ?x ?e ?stack)
	      (let-syntax
	          ((gensym (syntax-rules ()
			     ((gensym . args) (ren y . args)))))
		(gensym y z ?x ?e ?stack)))
	     ((beta (?e1 ?e2) ?b ?e ?stack)
	      (NORM (((lambda (x) ?e1) ?e)
	             ((lambda (x) ?e2) ?e)) ?stack))
	     ((beta y ?x ?e ?stack) (NORM y ?stack)))))
       (beta b x e stack)))
    ((NORM (e1 e2) stack)
     (NORM e1 (e2 . stack)))
    ((NORM x ()) x)
    ((NORM x stack) (unwind x stack))
    ))


(define-syntax unwind
  (syntax-rules ()
    ((unwind t (t1 ...))
     (t (NORM t1 ()) ...))))



Some tests:

,expand (NORM ((lambda (x) (x x)) (lambda (y) (y z))))
==> '(z z)

,expand (NORM ((lambda (x) (lambda (x) x)) 1))
==> (lambda (x) x)

,expand (NORM ((lambda (x) (lambda (y) (((f x) y) y))) (g y)))
==> (lambda (z) (f (g y) z z))


,expand (NORM (lambda (a) ((lambda (x) (lambda (a) (a x))) (a x))))
==> (lambda (a) (lambda (a1) (a1 (a x))))
,expand (NORM (lambda (a) ((lambda (x) (lambda (a) (x a))) a)))
==>	   (z^z),
,expand (NORM (lambda (a) ((lambda (x) (lambda (b) (x a))) a)))
=>          (a^b^a#a)


,expand (NORM ((((lambda (a) (lambda (x) (lambda (a) (a x)))) (a x)) 1) 2))
==> (2 1)
,expand (NORM (((lambda (a) (lambda (x) (a (x a)))) 1) 2))
==> (1 (2 1))
,expand (NORM (((lambda (a) ((lambda (x)(lambda (a) (a x))) (a x))) 1) list))
==> (list (1 x))

A good test of hygiene
,expand (NORM (((lambda (a) ((lambda (x) (lambda (a) (x a))) a)) list) 1))
==> (list 1) and evaluates to (1)
Show it is really a normal-order evaluator
,expand (NORM (((lambda (f) (lambda (x) x))
		((lambda (x) (x x)) (lambda (x) (x x)))) 1))
==> 1
,expand (NORM ((lambda (c) (lambda (f) (lambda (x) (f ((c f) x)))))
	       (lambda (f) f)))

,expand (NORM ((((lambda (c) (lambda (f) (lambda (x) (f ((c f) x)))))
		 (lambda (f) f)) list) 0))

(((NORM ((lambda (c) (lambda (f) (lambda (x) (f ((c f) x)))))
	 (lambda (f) f))) (lambda (u) (+ 1 u))) 0)
==> 2


That normalizer is sound. However, the lazy substitution
(((lambda (x) ?e1) ?e) ((lambda (x) ?e2) ?e))
replicates 'e' and builds bigger and bigger terms. Chances are, ?e1
may have no occurrence of 'x', in that case, carrying '?e' around is
just a waste. As it turns out, for complex expressions, the normalizer
quickly runs out of memory. The problem is not that we're lazy in
doing substitutions. The problem is that we are not lazy enough.

The second attempt:

(define-syntax NORM
  (syntax-rules (lambda)
    ((NORM t) (NORM t () ()))
    ((NORM (lambda (x) e) env ())
     (let-syntax ((ren (syntax-rules ()
			 ((ren ?x ?e ?env) (lambda (x) (NORM ?e ((?x () x) . ?env) ()))))))
       (ren x e env)))
    ((NORM (lambda (x) b) env ((enve e) . stack))
     (NORM b ((x enve e) . env) stack))
    ((NORM (e1 e2) env stack)
     (NORM e1 env ((env e2) . stack)))
    ((NORM x () ()) x)
    ((NORM x () ((enve e) ...))
     (x (NORM e enve ()) ...))
    ((NORM x env stack)
     (let-syntax
	 ((find
	   (syntax-rules (x)
	     ((find ?x ((x ?envs ?es) . _) ?stack) (NORM ?es ?envs ?stack))
	     ((find ?x (_ . ?env) ?stack) (NORM ?x ?env ?stack)))))
       (find x env stack)))
    ))

We notice that NORM has two 'reductions':
- expressions are reduced with respect to the stack
- variables are reduced with respect to the environment

We can see that we indeed:
- find the left-most innermost context for each redex
- beta reductions are done correctly (bound variables
					    are effectively renamed)




Now, let's investigate (shift shift), etc. We define shift to have the
same signature as call/cc:
(define (shift* p) (shift f (p f)))



(define-syntax CPS
  (syntax-rules (lambda call/cc shift*)
    ((CPS (?e1 ?e2) . args) 			; application
     (NORM (lambda (k) ((CPS ?e1) (lambda (f) ((CPS ?e2) (lambda (a)
							   ((f a) k)))))) . args))
    ((CPS (lambda (x) ?e) . args )		; abstraction
     (NORM (lambda (k) (k (lambda (x) (CPS ?e)))) . args))
    ((CPS call/cc . args)
     (NORM (lambda (k0)
	     (k0 (lambda (p) (lambda (k)
			       ((p (lambda (a) (lambda (k1) (k a)))) k)))))
	   . args))
    ((CPS shift* . args)
     (NORM
      (lambda (k0)
	(k0 (lambda (p) (lambda (c)
			  ((p (lambda (x) (lambda (c2) (c2 (c x)))))
			   (lambda (v) v))))))
      . args))
    ((CPS ?x . args)
     (NORM (lambda (k) (k ?x)) . args))))

> (expand '(CPS (shift* shift*)))
(lambda (#:k)
  (#:k (lambda (#:x) (lambda (#:c2) (#:c2 #:x)))))
> (expand '(CPS ((shift* shift*) p)))
(lambda (#:k) (#:k p))
> (expand '(CPS ((lambda (x) x) p)))
(lambda (#:k) (#:k p))


So, (shift* shift*) is the identity function.

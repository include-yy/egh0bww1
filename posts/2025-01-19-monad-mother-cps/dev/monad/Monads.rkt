#lang racket

(module Monad racket
  (provide (except-out (all-defined-out) Monad0<%>))
  (define-syntax doit
    (syntax-rules (<- =)
      [(_ exp) exp]
      [(_ var <- mexp rest ...)
       (mexp . >>= . (λ (var) (doit rest ...)))]
      [(_ (v1 v2 ...) = exp rest ...)
       (let-values ([(v1 v2 ...) exp])
         (doit rest ...))]
      [(_ var = exp rest ...)
       (let ([var exp]) (doit rest ...))]
      [(_ mexp rest ...)
       (mexp . >>= . (λ (_) (doit rest ...)))]))
  (define Monad0<%> (interface ()))
  (define Monad/c (is-a?/c Monad0<%>))
  (define Monad<%>
    (interface (Monad0<%>)
      [return (->m any/c Monad/c)]
      [bind (->m (-> any/c Monad/c) Monad/c)]))
  (define (return m a) (send m return a))
  (define (>>= m fun) (send m bind fun))
  (define (>=> f g) (λ (m) ((m . >>= . f) . >>= . g)))
  (define ((fmap f) xs)
    (xs . >>= . (λ (x) (return xs (f x)))))
  (define (join mm) (mm . >>= . identity))
  (define (liftM f) (fmap f))
  (define (liftM2 f)
    (λ (m1 m2)
      (doit x1 <- m1 x2 <- m2
            (return m1 (f x1 x2)))))
  (define (liftM3 f)
    (λ (m1 m2 m3)
      (doit x1 <- m1 x2 <- m2
            x3 <- m3
            (return m1 (f x1 x2 x3)))))
  (define (liftM4 f)
    (λ (m1 m2 m3 m4)
      (doit x1 <- m1 x2 <- m2
            x3 <- m3 x4 <- m4
            (return m1 (f x1 x2 x3 x4)))))
  (define (liftM5 f)
    (λ (m1 m2 m3 m4 m5)
      (doit x1 <- m1 x2 <- m2
            x3 <- m3 x4 <- m4 x5 <- m5
            (return m1 (f x1 x2 x3 x4 x5)))))
  (define ((ap mf) ma)
    (doit f <- mf a <- ma
          (return (f a)))))

(module Cont racket
  (require (submod ".." Monad))
  (define Cont%
    (class* object% (Monad<%>)
      (super-new)
      (init-field value)
      (define/public (return v) (Rc v))
      (define/public (bind f)
        (cont (λ (c) (Gc this (λ (a) (Gc (f a) c))))))
      (define/public (get f) (value f))))
  (define Cont/c (is-a?/c Cont%))
  (define (cont f) (instantiate Cont% [f]))
  (define (Gc m f) (send m get f))
  (define (Rc x) (cont (λ (k) (k x))))
  (define (Kc x) (λ (k) (k x)))
  (define (Cc m f) (>>= m f))
  (define runCont Gc)
  (define (callCC f)
    (cont (λ (k) (Gc (f (λ (v) (cont (λ (_k) (k v))))) k))))
  (provide
   (contract-out
    [Cc (-> Cont/c (-> any/c Cont/c) Cont/c)]
    [Gc (-> Cont/c (-> any/c any/c) any/c)]
    [callCC (((any/c . -> . Cont/c) . -> . Cont/c) . -> . Cont/c)])
   Kc Rc runCont cont))

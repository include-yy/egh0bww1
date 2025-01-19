#lang racket

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
        (return (f a))))

(provide (except-out (all-defined-out) Monad0<%>))

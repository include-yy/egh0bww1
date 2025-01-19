#lang racket
(require "m.rkt")

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
 (all-from-out "m.rkt")
 (contract-out
  [Cc (-> Cont/c (-> any/c Cont/c) Cont/c)]
  [Gc (-> Cont/c (-> any/c any/c) any/c)]
  [callCC (((any/c . -> . Cont/c) . -> . Cont/c) . -> . Cont/c)])
 Kc Rc runCont cont)

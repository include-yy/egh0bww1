#lang racket

(define Cont%
  (class object%
    (super-new)
    (init value auto)
    ;; CPS transform for simple value
    ;; if we want to encapsulate complex continuation
    ;; we can pass auto with #f to just wrap it
    ;; (auto is just used for call/cc)
    (define val (if (not auto) value (λ (k) (k value))))
    (define/public (run f) (val f))
    (define/public (>>= fm) (val fm))))

(define (Cont0 v) (new Cont% [value v] [auto #t]))
(define (ContF f) (λ args (Cont0 (apply f args))))

;; haskell bind operator
(define (>>= obj f) (send obj >>= f))
(define id identity)
;; call/cc
(define (callCC f)
  (new Cont%
       [value (λ (k)
                (>>=
                 (f (λ (v)
                      (new Cont%
                           [value (λ (_k) (k v))]
                           [auto #f])))
                 k))]
       [auto #f]))

;; ((λ (k) (k 1)) (λ (x) (λ (k) (k (+ x 1)))))
(define a (>>= (Cont0 1) (ContF (λ (x) (+ x 1)))))
(send a run id)
;;=> 2
;; (call/cc (λ (k) (k (+ 2 3))))
(define b (callCC (λ (k) (k (+ 2 3)))))
(send (>>= b (λ (x) (Cont0 x))) run id)
;;=> 5
(define c (callCC (λ (k) (>>= (Cont0 2) (λ (x) (k (* x 3)))))))
(send c run id)
;;=> 6
(define d (callCC (λ (k) (Cont0 (* 1 2)))))
(send d run id)
;;=> 2

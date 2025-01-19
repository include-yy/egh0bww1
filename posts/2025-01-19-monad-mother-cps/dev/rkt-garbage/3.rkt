#lang racket

((λ (k) (k (+ 2 3)))
 (λ (x) x))

(((λ (k) (k (+ 2 3)))
  (λ (v) (λ (k) (k v))))
 (λ (x) x))

((λ (k0)
   (((λ (k) (k (+ 2 3)))
     (λ (v) (λ (k) (k0 v))))
    (λ (v) (+ v 1))))
 identity)
;;=> 5

(define (callCC f)
  (λ (k)
    ((f (λ (v) (λ (_k) (k v))))
     k)))

((callCC
  (λ (k)
    ((k (λ (k) (k (+ 2 3))))
     (λ (v) (λ (k) (k (+ v 1)))))))
 identity)
;;=> 5

(define Cont%
  (class object%
    (super-new)
    (init cps)
    (define val cps)
    (define/public (run f) (val f))
    (define/public (>>= fm) (val fm))))
(define (Cont0 v) (new Cont% [cps (λ (k) (k v))]))
(define (ContF f) (λ args (Cont0 (apply f args))))
(define (callCC f)
  (new Cont%
       [cps (λ (k)
              (send
               (f (λ (v) (new Cont%
                              [cps (λ (_k) (k v))])))
               run
               k))]))
;; haskell bind operator
(define (>>= obj f) (send obj >>= f))
(define (runCont cobj f) (send cobj run f))
(define id identity)

;; ((λ (k) (k 1)) (λ (x) (λ (k) (k (+ x 1)))))
(runCont (>>= (Cont0 1) (ContF (λ (x) (+ x 1)))) id) ;;=> 2
;; (call/cc (λ (k) (k (+ 2 3))))
(runCont (callCC (λ (k) (k (+ 2 3)))) id) ;;=> 5
(runCont (callCC (λ (k) (>>= (Cont0 2) (λ (x) (k (* x 3)))))) id) ;;=> 6
(runCont (callCC (λ (k) (Cont0 (* 1 2)))) id) ;;=> 2

#lang racket

(define stack%
  (class object%
    (super-new)
    (define stack '())
    (define/public (pop) (begin0 (car stack) (set! stack (cdr stack))))
    (define/public (push val) (set! stack (cons val stack)))
    (define/public (empty?) (= (length stack) 0))
    (define/public (get) stack)))

(define (wrap f)
  (λ (stack)
    (let ([p (λ () (send stack pop))])
      (let* ((v (p))) (send stack push (f v))))))
(define (wrap2 f)
  (λ (stack)
    (let ([p (λ () (send stack pop))])
      (let* ([v2 (p)] [v1 (p)])
        (send stack push (f v1 v2))))))
(define (wrap3 f)
  (λ (stack)
    (let ([p (λ () (send stack pop))])
      (let* ([v3 (p)] [v2 (p)] [v1 (p)])
        (send stack push (f v1 v2 v3))))))

(define car@ (wrap car))
(define cdr@ (wrap cdr))
(define cons@ (wrap2 cons))

;; (cdr (car '((1 2) 3)))
(let ((s (new stack%)))
  (send s push '((1 2) 3))
  (car@ s)
  (cdr@ s)
  (send s pop))
;; => '(2)

#|
在这个栈调用模型中，被调函数对它的调用者是一无所知的，它只是从栈上获取参数，执行完毕后将自己的返回值放回栈上。当然我们也可以换一种说法，被调函数和主调函数通过栈完成了交流，栈维护了「函数调用上下文」。我们可以使用如下代码，将嵌套表达式使用调用栈拉平：
|#

(define (flat S)
  (let ((st (new stack%)))
    (let F ([s S])
      (match s
        [(list 'quote v)
         (send st push `(quote ,v))]
        [(cons func args)
         (for-each (λ (x) (F x)) args)
         (send st push func)]
        [val
         (send st push `(quote ,val))]))
    (send st push 'pop)
    (reverse (send st get))))

(flat '(+ (* (+ 1 2) 3) (- (/ 2 3) 1)))
;; => '('1 '2 + '3 * '2 '3 / '1 - + pop)

#|
如你所见，最内层的表达式似乎是最先出现的，而同一嵌套层次的计算顺序选择就比较随意了。在这个实现中，对于同层表达式，我选择了从左到右的计算顺序，在某些语言中这个顺序可能是从右向左（比如 Ocaml），或者在语言层面没有规定顺序（如 C 和 C++）。
|#

#|
练习 1 尝试编写一个函数，它接受 flat 函数返回的列表，并给出这个列表的求值结果。比如：

(calc '('1 '2 + pop)) ;; => 3
|#

(define +@ (wrap2 +))
(define -@ (wrap2 -))
(define *@ (wrap2 *))
(define /@ (wrap2 /))

(let ((s (new stack%)))
  (send s push 1)
  (send s push 2)
  (+@ s)
  (display (send s get))
  (send s push 3)
  (*@ s)
  (display (send s get)))
;; => (3)(9)

(define global-env
  `((+ ,+@) (- ,-@) (* ,*@) (/ ,/@)
            (car ,car@) (cdr ,cdr@) (cons ,cons@)))
(define (getop op)
  (let* ((v (assoc op global-env)))
    (if v (cadr v) (error "not a fn:" op))))
(define (calc S)
  (let ((stack (new stack%))
        (res '()))
    (for-each
     (lambda (s)
       (match s
         [(list 'quote val)
          (send stack push val)]
         ['pop (set! res (send stack pop))]
         [op ((getop op) stack)]))
     S)
    res))

(calc (flat '(+ (* (+ 1 2) 3) (- (/ 6 3) 1))))
;; => 10

#|
练习 2 在练习 1 中，我们首先将表达式展平，随后使用 calc 函数对这个列表求值，我们能否在遍历表达式的过程中直接完成求值？请实现 inter 函数，它接受一个表达式列表并给出它的值。

我们当然可以实现为 (define (inter x) (eval x)) ，但本题的目的是尝试让读者使用显式的栈操作来实现嵌套调用的上下文。
|#

(define (inter S)
  (let ((stack (new stack%)))
    (let eval ([s S])
      (match s
        [(list 'quote v)
         (send stack push v)]
        [(cons f args)
         (for-each eval args)
         ((getop f) stack)]
        [val (send stack push val)]))
    (send stack pop)))

(inter '(+ (* (+ 1 2) 3) (- (/ 6 3) 1)))
;; => 10

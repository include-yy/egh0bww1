#|
通过练习 1 和 2 我们实现了一个简单的基于栈的解释器，在此基础上我们可以加上变量、函数、选择循环控制结构，等等。读者有兴趣的话可以尝试对上面的代码进行扩充。这里给出一个简单的实现，它支持 if 和 let ：
|#

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
      (let ((v (p))) (send stack push (f v))))))
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
(define =@ (wrap2 =))
(define zero?@ (wrap zero?))
(define +@ (wrap2 +))
(define -@ (wrap2 -))

(define global-env
  `((car ,car@) (cdr ,cdr@) (cons ,cons@)
                (= ,=@) (+ ,+@) (- ,-@)))

(define (interprete S)
  (let ((stack (new stack%)))
    (let eval ([s S] [tbl global-env])
      (match s
        [(list 'quote v)
         (send stack push v)]
        [(cons 'λ rest)
         (send stack push (cons 'λ rest))]
        [(list 'if test a b)
         (eval test tbl)
         (let ((res (send stack pop)))
           (if res (eval a tbl)
               (eval b tbl)))]
        [(list 'let (list smb v) body)
         (eval v tbl)
         (eval body (cons `(,smb ,(send stack pop)) tbl))]
        [(cons f args)
         (eval f tbl)
         (let ((F (send stack pop)))
           (for-each (λ (x) (eval x tbl)) args)
           (cond
             [(procedure? F) (F stack)]
             [(and (cons? F) (eq? (car F) 'λ))
              (let* ((eargs (map (λ (_) (send stack pop)) args))
                     (paras (cadr F))
                     (env (map list paras eargs))
                     (body (caddr F)))
                (eval body (append env tbl)))]
             [else (error "may not be a function:" f)]))]
        [val
         (if (symbol? val)
             (let ((v (assoc val tbl)))
               (if v (send stack push (cadr v))
                   (error "value not found:" val)))
             (send stack push val))]))
    (send stack pop)))

;; 通过以下代码我们可以计算斐波拉契数列：

(interprete '(let (f (λ (x)
                       (if (= x 0) 0
                           (if (= x 1) 1
                               (+ (f (- x 1))
                                  (f (- x 2)))))))
               (f 12)))
;; => 144

#|
练习 3 考虑 interprete 的代码，请在实现支持同样功能的 flat/calc 版本的基础上，为函数实现闭包，即函数被求值时会确定它的 环境 。当然你可以叫它们 compile/exec 。

（这一题也许有些难，读者可以跳过，不过下面会直接使用这一题的代码来展开某些表达式）
|#

;; -*- lexical-binding: t; -*-

(defalias 'λ 'lambda)
(defmacro Cc (m f)
  `(funcall ,m ,f))
(defmacro Rc (x)
  (let ((k* (gensym "$k")))
    `(λ (,k*) (funcall ,k* ,x))))
(defmacro Rc* (x) (if (atom x) (Rc x) x))
(defmacro +** () `(λ (a) (Rc (λ (b) (Rc (+ a b))))))
(defmacro -** () `(λ (a) (Rc (λ (b) (Rc (- a b))))))
(defmacro *** () `(λ (a) (Rc (λ (b) (Rc (* a b))))))
(defmacro /** () `(λ (a) (Rc (λ (b) (Rc (/ a b))))))
(defmacro my/+0 (a b)
  (let ((x (gensym "x"))
	(y (gensym "y")))
    `(Cc (Cc ,a (λ (,x) (Rc (λ (,y) (Rc (+ ,x ,y)))))) ,b)))
(defmacro my/+ (a b) `(Cc (Cc (Rc* ,a) (+**)) (Rc* ,b)))
(defmacro my/- (a b) `(Cc (Cc (Rc* ,a) (-**)) (Rc* ,b)))
(defmacro my/* (a b) `(Cc (Cc (Rc* ,a) (***)) (Rc* ,b)))
(defmacro my// (a b) `(Cc (Cc (Rc* ,a) (/**)) (Rc* ,b)))

;; test using
;; (macroexpand-all '(my/+ (my/+ (my/* 2 3) 3) (my/- (my// 3 3) 1)))

(my/+ (my/+ (my/* 2 3) 3) (my/- (my// 3 3) 1))
;;=> #[($k529) ((funcall $k529 (+ a b))) ((b . 0) (a . 9))]
(#[(k) ((funcall k (+ a b))) ((b . 0) (a . 9))] #'identity)
;;=> 9

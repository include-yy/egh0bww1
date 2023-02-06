;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'yyhash)

(ert-deftest yyhash-hash-t ()
  "test for hash functions"
  (should-error (yyhash--symbol 1))
  (should-error (yyhash--string '(1 2 3)))
  (should-error (yyhash--float 1))
  (should-error (yyhash--hashfn (make-bool-vector 10 1) 0))
  (should-error (yyhash--hashfn (expt 2 1000) 0))
  (should (zerop (yyhash--hashfn 1.0e+INF 0)))
  (should (zerop (yyhash--hashfn 0.0e+NaN 0)))
  (should (zerop (yyhash--hashfn [] 0))))

(ert-deftest yyhash-make-t ()
  "test for hashtable creation"
  (cl-loop
   for i from 1 to 1000
   do (let ((a (yyhash-make i)))
	(should (cl-every
		 (lambda (x) (= (length x) i))
		 (list (yyhash--s-hash a)
		       (yyhash--s-next a))))
	(should (= (length (yyhash--s-key-and-value a)) (* i 2)))
	(should (= (length (yyhash--s-index a))
		   (yyhash--next-almost-prime
		    (floor (/ i yyhash--rehash-threshold)))))
	(should (= (yyhash--s-count a) 0))
	(should (= (yyhash--s-next-free a) 0))
	(should (cl-every 'null (yyhash--s-hash a)))
	(should (cl-every (lambda (x) (= x -1)) (yyhash--s-index a)))
	(should (cl-every 'null (yyhash--s-key-and-value a)))
	(should (cl-loop for j below (1- i)
			 for k across (yyhash--s-next a)
			 when (not (= (1+ j) k)) return nil
			 finally return t))))
  (should (yyhash-make 0))
  (should (yyhash-make yyhash--biggest))
  (should (yyhash-make (1+ yyhash--biggest))))

(ert-deftest yyhash-getter/setter-t ()
  "test for getter and setter functions"
  (should (fboundp 'yyhash--hash-getv))
  (should (fboundp 'yyhash--hash-setv))
  (should (fboundp 'yyhash--next-getv))
  (should (fboundp 'yyhash--next-setv))
  (should (fboundp 'yyhash--index-getv))
  (should (fboundp 'yyhash--index-setv))
  (should (fboundp 'yyhash--key-and-value-getv))
  (should (fboundp 'yyhash--key-and-value-setv))

  (let ((a (yyhash-make))
	(b (yyhash-make)))
    (cl-loop
     for i below (yyhash-size a)
     do (progn
	  (aset (yyhash--s-hash a) i i)
	  (aset (yyhash--s-next a) i (+ i 1))
	  (aset (yyhash--s-key-and-value a) (* i 2) (+ i 2))
	  (aset (yyhash--s-key-and-value a) (1+ (* i 2)) (+ i 3)))
     do (progn
	  (should (= (yyhash--hash-getv a i) i))
	  (should (= (yyhash--next-getv a i) (+ i 1)))
	  (should (= (yyhash--key-and-value-getv a (* i 2)) (+ i 2)))
	  (should (= (yyhash--key-and-value-getv a (1+ (* i 2))) (+ i 3)))
	  (should (= (yyhash--key a i) (+ i 2)))
	  (should (= (yyhash--value a i) (+ i 3)))))
    (cl-loop
     for i below (yyhash--isize a)
     do (aset (yyhash--s-index a) i i)
     do (should (= (yyhash--index-getv a i) i)))
    (cl-loop
     for i below (yyhash-size b)
     do (progn
	  (yyhash--hash-setv b i i)
	  (yyhash--next-setv b i (+ i 1))
	  (yyhash--key-and-value-setv b (* i 2) (+ i 2))
	  (yyhash--key-and-value-setv b (1+ (* i 2)) (+ i 3))))
    (cl-loop
     for i below (yyhash--isize b)
     do (yyhash--index-setv b i i))
    (should (equal a b))))

(ert-deftest yyhash-copy-t ()
  (should (equal (yyhash-make) (yyhash-copy (yyhash-make)))))

(ert-deftest yyhash-put-t ()
  "test for yyhash-put
about function: yyhash-put, yyhash--putnew
yyhash--lookup, yyhash--maybe-resize"
  (let ((a (yyhash-make 10)))
    ;;109 'a and "a" have smae hash value 109
    (yyhash-put 109 1 a)
    (yyhash-put 'a 2 a)
    (yyhash-put "a" 3 a)
    ;;test for hash, next and k-v slot
    (should (= (yyhash--hash-getv a 0) 109))
    (should (= (yyhash--hash-getv a 1) 109))
    (should (= (yyhash--hash-getv a 2) 109))
    (should (= (yyhash--value a 0) 1))
    (should (= (yyhash--value a 1) 2))
    (should (= (yyhash--value a 2) 3))
    (should (= (yyhash--next-getv a 0) -1))
    (should (= (yyhash--next-getv a 1) 0))
    (should (= (yyhash--next-getv a 2) 1))
    ;;count next-free and isize
    (should (= (yyhash-count a) 3))
    (should (= (yyhash--s-next-free a) 3))
    (should (= (yyhash--isize a) 13))
    (should (= 2 (yyhash--index-getv a (% 109 (yyhash--isize a)))))
    ;; add to 10 entries
    (cl-loop for i below 7
	     do (yyhash-put (+ i 1) i a))
    (should (= (yyhash-count a) 10))
    (should (= (yyhash-size a) 10))
    ;; add 11th entries
    (yyhash-put 10 10 a)
    (should (= (yyhash-size a) 15))
    (should (= (yyhash--isize a) 19))
    ;;test add same same key
    (yyhash-put 109 4 a)
    (yyhash-put 'a 5 a)
    (yyhash-put "a" 6 a)
    (should (= 4 (yyhash--value a 0)))
    (should (= 5 (yyhash--value a 1)))
    (should (= 6 (yyhash--value a 2)))))

(ert-deftest yyhash-lookup-get-t ()
  "test yyhash--lookup"
  (let ((a (yyhash-make 10)))
    (yyhash-put 1 1 a)
    (yyhash-put 2 2 a)
    (yyhash-put 3 3 a)
    (yyhash-put 4 4 a)
    (yyhash-put 5 5 a)
    (yyhash-put 109 6 a)
    (yyhash-put 'a 7 a)
    (yyhash-put "a" 8 a)
    (yyhash-put 1.0 9 a)
    (yyhash-put '(1 2 3) 10 a)
    ;;lookup
    (should (= (yyhash--lookup 1 a) 0))
    (should (= (yyhash--lookup 2 a) 1))
    (should (= (yyhash--lookup 3 a) 2))
    (should (= (yyhash--lookup 4 a) 3))
    (should (= (yyhash--lookup 5 a) 4))
    (should (= (yyhash--lookup 109 a) 5))
    (should (= (yyhash--lookup 'a a) 6))
    (should (= (yyhash--lookup "a" a) 7))
    (should (= (yyhash--lookup 1.0 a) 8))
    (should (= (yyhash--lookup '(1 2 3) a) 9))
    (should (= (yyhash--lookup 1.2 a) -1))
    (should (= (yyhash--lookup 6 a) -1))
    ;;get
    (should (= (yyhash-get 1 a) 1))
    (should (= (yyhash-get 2 a) 2))
    (should (= (yyhash-get 3 a) 3))
    (should (= (yyhash-get 4 a) 4))
    (should (= (yyhash-get 5 a) 5))
    (should (= (yyhash-get 109 a) 6))
    (should (= (yyhash-get 'a a) 7))
    (should (= (yyhash-get "a" a) 8))
    (should (= (yyhash-get 1.0 a) 9))
    (should (= (yyhash-get '(1 2 3) a) 10))
    (should (null (yyhash-get 1.2 a)))
    (should (eq 'wocao (yyhash-get 1.2 a 'wocao)))
    (should (null (yyhash-get 6 a)))))

(ert-deftest yyhash-clr-t ()
  "test clear hashtable"
  (let ((a (yyhash-make 5)))
    (yyhash-put 1 1 a)
    (yyhash-put 2 1 a)
    (yyhash-put 3 1 a)
    (yyhash-put 4 1 a)
    (should (equal (yyhash-make 5) (yyhash-clr a)))))

(ert-deftest yyhash-rem-t ()
  "test remove entry hashtable"
  (let ((a (yyhash-make 10)))
    (yyhash-put 109 1 a)
    (yyhash-put 'a 2 a)
    (yyhash-put "a" 3 a)
    (yyhash-put 1.1 4 a)
    (yyhash-put 1.2 5 a)
    (should (= 5 (yyhash-count a)))
    ;; rem non-exist item
    (yyhash-rem 6 a)
    (should (= 5 (yyhash-count a)))
    (should (= 5 (yyhash--s-next-free a)))
    ;; rem head item
    (yyhash-rem 1.1 a)
    (should (= 3 (yyhash--s-next-free a)))
    (should (= 4 (yyhash-count a)))
    ;; rem tail item
    (yyhash-rem 109 a)
    (should (= 0 (yyhash--s-next-free a)))
    (should (= 3 (yyhash-count a)))
    (yyhash-put 109 1 a)
    (should (= 3 (yyhash--s-next-free a)))
    (should (= 4 (yyhash-count a)))
    ;; rem mid item
    (yyhash-rem 'a a)
    (should (= 1 (yyhash--s-next-free a)))
    (should (= 3 (yyhash-count a)))))

(ert-deftest yyhash-map-t ()
  "test map hashtable"
  (let ((keys (number-sequence 1 1000))
	(vals (number-sequence 2 1001))
	(reg1 0)
	(reg2 0)
	(a (yyhash-make 5)))
    (cl-loop for i in keys
	     for j in vals
	     do (yyhash-put i j a))
    (yyhash-map
     (lambda (x y)
       (incf reg1 x)
       (incf reg2 y))
     a)
    (should (= reg1 (* 1001 500)))
    (should (= reg2 (* 1003 500)))
    (should (= 1000 (- reg2 reg1)))))

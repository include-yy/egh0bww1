;;; yyhash.el --- yy's hashtable -*- lexical-binding: t -*-
(defconst yyhash-rehash-size 1.5)
(defconst yyhash-rehash-threshold 0.8125)
(defconst yyhash-defsize 65)

(defun yyhash-int (o) o)

(defun yyhash-symbol (s)
  (yyhash-string (symbol-name s)))

(defsubst yyhash-combine (x y)
  (+ (lsh x 4) (lsh x -57) y))
(defsubst yyhash-reduce (x)
  (logand
   (logxor x (lsh x -3))
   most-positive-fixnum))
(defun yyhash-string (s)
  (let ((hash 0))
    (cl-loop
     for i across s
     do (cl-loop
	 for x = i then (lsh x -8)
	 do (setq hash (yyhash-combine hash (logand x #xff)))
	 when (<= x #xff) return 0))
    (yyhash-reduce hash)))

(defun yyhash-float2list (m)
  (let* ((sgn (signum m))
	 (am (abs m))
	 (exp-n (truncate (log am 2)))
	 (exp-bias (+ exp-n 1023))
	 (num (/ am (expt 2 exp-n)))
	 (num52 (logand (floor (* num (lsh 1 52)))
			(1- (lsh 1 52))))
	 (num56 (+ num52 (lsh (logand 15 exp-bias) 52)))
	 (num8 (if (= sgn -1) (+ 128 (lsh exp-bias -4)) (lsh exp-bias -4))))
    (cl-values num8 num56)))

(defun yyhash-float (m)
  (if (or (isnan m)
	  (= (abs m) 1.0e+INF))
      0
    (cl-multiple-value-bind (a b) (yyhash-float2list m)
      (let ((hash a))
	(cl-loop
	 for x = b then (lsh x -8)
	 do (setq hash (yyhash-combine hash (logand x #xff)))
	 when (<= x #xff) return (yyhash-reduce hash))))))

(defconst yyhash-max-depth 3)
(defconst yyhash-max-len 7)
(do ((i 0 (+ i 1))) ((> i 7) i) )


(defun yyhash-list (ls depth)
  (let ((hash 0))
    (when (< depth yyhash-max-depth)
      (do ((i 0 (+ i 1)))
	  ((not (and (consp ls) (< i yyhash-max-len))))
	(let ((hash2 (yyhash-hash (car ls) (1+ depth))))
	  (setq hash (yyhash-combine hash hash2))
	  (pop ls))))
    (when (not (null ls))
      (let ((hash2 (yyhash-hash ls (1+ depth))))
	(setq hash (yyhash-combine hash hash2))))
    (yyhash-reduce hash)))

(defun yyhash-vec (v depth)
  (let* ((hash (length v))
	 (n (min yyhash-max-len hash)))
    (cl-loop
     for i below n
     do (setq hash (yyhash-combine
		    hash
		    (yyhash-hash (aref v i) (1+ depth)))))
    (yyhash-reduce hash)))
(defun yyhash-hash (obj depth)
  (if (> depth yyhash-max-depth) 0
    (let ((tp (type-of obj)))
      (cl-case tp
	((integer) (if (fixnump obj)
		       (yyhash-int obj)
		     (error "not a fixnum obj: %d" obj)))
	((symbol) (yyhash-symbol obj))
	((string) (yyhash-string obj))
	((float) (yyhash-float obj))
	((cons) (yyhash-list obj depth))
	((vector) (yyhash-vec obj depth))
	(otherwise (error "unknown type obj: %s" obj))))))

(cl-defstruct (yyhash
	       (:conc-name yyhash--)
	       (:constructor yyhash--create)
	       (:copier nil)
	       (:predicate yyhash-p))
  "yy's hash table struct"
  (hash nil :type vector
	:documentation "vector of hash codes, or nil if the table needs rehashing. If the I-th entry is unused, then hash[I] should be nil")
  (next nil :type vector
	:documentation "vector used to chain entries. If entry I is free, next[I] is the entry number of next free item. If entry I is non-free, next[i] is the index of the next entry in the collision chain, or -1 if there is such entry")
  (index nil :type vector
	 :documentation "Bucket vector. An entry of -1 indicates no item is present, and a nonnegative entry is the index of the first item in a collision chain. This vector's size can be larger than the hash table size to reduce collisions")
  (count nil :type integer
	 :documentation "Number of key/value entries in the table")
  (next-free
   nil
   :type integer
   :documentation "index of first free entry in free list, or -1 if none")
  (key-and-value
   nil
   :type vector
   :documentation "vector of keys and values. The key of item I is found at index 2 * I, the value is found at index 2 * I + 1. "))

(defconst yyhash-biggest 100000)
(defconst yyhash-isize-biggest (/ (* yyhash-biggest 3) 2))

(defun yyhash-next-almost-prime (n)
  (do ((n (logior n 1) (+ n 2)))
      ((and (not (zerop (% n 3)))
	    (not (zerop (% n 5)))
	    (not (zerop (% n 7))))
       n)))
(defun make-yyhash (&optional size)
  (cl-assert (or (null size) (integerp size)))
  (let* ((size (if size (if (<= size 0) 1 size) yyhash-defsize))
	 (h-size (min size yyhash-biggest))
	 (k-v (make-vector (* h-size 2) nil))
	 (next (make-vector h-size -1))
	 (i-size (yyhash-next-almost-prime
		  (floor (/ h-size yyhash-rehash-threshold))))
	 (index (make-vector i-size -1))
	 (hash (make-vector h-size nil)))
    (do ((i 0 (+ i 1)))
	((= i (- h-size 1)))
      (aset next i (+ i 1)))
    (yyhash--create
     :hash hash :next next :index index :key-and-value k-v
     :count 0 :next-free 0)))

(defun copy-yyhash (yh)
  (let ((k-v (copy-sequence (yyhash--key-and-value yh)))
	(hash (copy-sequence (yyhash--hash yh)))
	(next (copy-sequence (yyhash--next yh)))
	(index (copy-sequence (yyhash--index yh))))
    (make-yyhash
     :hash hash :next next :index index :key-and-value k-v
     :count (yyhash--count yh)
     :next-free (yyhash--next-free yh))))

(defun yyhash-count (yh) (yyhash--count yh))
(defun yyhash-size (yh) (length (yyhash--hash yh)))

(defun yyhash--index-val (yh n)
  (aref (yyhash--index yh) n))
(defun yyhash--next-val (yh n)
  (aref (yyhash--next yh) n))
(defun yyhash--key (yh n)
  (aref (yyhash--key-and-value yh) (* n 2)))
(defun yyhash--val (yh n)
  (aref (yyhash--key-and-value yh) (1+ (* n 2))))
(defun yyhash--hash-val (yh n)
  (aref (yyhash--hash yh) n))

(defun getyyhash (key yh &optional default)
  (let* ((hash-code (yyhash-hash key 0))
	 (start-of-bucket (% hash-code (length (yyhash--index yh))))
	 (find-i
	  (do ((i (yyhash--index-val yh start-of-bucket)
		  (yyhash--next-val yh i)))
	      ((or (< i 0)
		   (equal key (yyhash--key yh i)))
	       i))))
    (if (= -1 find-i) default
      (yyhash--val yh find-i))))

(defun clryyhash (yh)
  (when (> (yyhash-count yh) 0)
    (let ((size (yyhash-size yh))
	  do (next (yyhash--next yh))
	  do (index (yyhash--index yh))
	  do (k-v (yyhash--key-and-value yh)))
      (cl-loop for i below size
	       do (aset next i (+ i 1))
	       do (aset k-v (* 2 i) nil)
	       do (aset k-v (1+ (* 2 i)) nil))
      (cl-loop for i below (length index)
	       do (aset index i -1))
      (setf (cl-struct-slot-value 'yyhash 'next-free yh) 0)
      (setf (cl-struct-slot-value 'yyhash 'count yh) 0)))
  yh)

(defun remyyhash (key yh)
  (let* ((hash-code (yyhash-hash key 0))
	 (start-id (% hash-code (length (yyhash--index yh))))
	 (prev -1)
	 (fd (do ((i (yyhash--index-val yh start-id)
		     (yyhash--next-val yh i)))
		 ((equal key (yyhash--key yh i)) i)
	       (setq prev i))))
    (when (> fd 0)
      (if (< prev 0)
	  (aset (yyhash--index yh) start-id (yyhash--next-val yh i))
	(aset (yyhash--next yh) prev (yyhash--next-val yh i)))

      (aset (yyhash--key-and-value yh) (* i 2) nil)
      (aset (yyhash--key-and-value yh) (1+ (* i 2)) nil)
      (aset (yyhash--hash yh) i nil)
      (aset (yyhash--next yh) i (yyhash--next-free yh))
      (setf (cl-struct-slot-value 'yyhash 'next-free yh) i)
      (cl-decf (cl-struct-slot-value 'yyhash 'count yh))
      (cl-assert (>= (yyhash--count yh) 0)))
    nil))

(defun mapyyhash (fn yh)
  (let* ((k-v (yyhash--key-and-value yh))
	 (len (/ (length k-v) 2)))
    (do ((i 0 (+ i 2))
	 (j 1 (+ j 2))
	 (k 0 (+ k 1)))
	((= k len))
      (and (not (null (aref k-v i)))
	   (funcall fn (aref k-v i) (aref k-v j))))))

(defun yyhash--putnew (key val yh hash)
  (yyhash-maybe-resize yh)
  (incf (cl-struct-slot-value 'yyhash 'count yh) 1)
  (let ((i (yyhash--next-free yh))
	(k-v (yyhash--key-and-value yh))
	(hash-v (yyhash--hash yh))
	(next-v (yyhash--next yh))
	(index-v (yyhash--index yh))
	(start-id (% hash (length (yyhash--index yh)))))
    (cl-assert (null (yyhash--hash-val yh i)))
    (cl-assert (null (yyhash--key yh i)))
    (setf (cl-struct-slot-value 'yyhash 'next-free yh)
	  (yyhash--next-val yh i))
    (aset k-v (* i 2) key)
    (aset k-v (1+ (* i 2)) val)
    (aset hash-v i hash)

    (aset next-v i (aref index-v start-id))
    (aset index-v start-id i)
    i))

(defun putyyhash (key val yh)
  (let* ((hash-code (yyhash-hash key 0))
	 (start-id (% hash-code (length (yyhash--index yh))))
	 (find-i (do ((i (yyhash--index-val yh start-id)
			 (yyhash--next-val yh i)))
		     ((or (<= i 0)
			  (equal key (yyhash--key yh i)))
		      i))))
    (if (> find-i 0)
	(aset (yyhash--key-and-value yh) (1+ (* find-i 2)) val)
      (yyhash--putnew key val yh hash-code))
    val))

(defun yyhash--vec-copy (new old len)
  (cl-loop for i below len
	   do (aset new i (aref old i)))
  new)

(defun yyhash-maybe-resize (yh)
  (when (< (yyhash--next-free yh) 0)
    (let* ((old-size (yyhash-size yh))
	   (new-size (floor (* old-size yyhash-rehash-size))))
      (when (> new-size yyhash-biggest)
	(setq new-size yyhash-biggest))
      (when (= old-size yyhash-biggest)
	(error "yyhash is up to biggest"))
      (let* ((next-n (yyhash--vec-copy
		      (make-vector new-size -1)
		      (yyhash--next yh) old-size))
	     (k-v-n (yyhash--vec-copy
		     (make-vector (* 2 new-size) nil)
		     (yyhash--key-and-value yh) (* old-size 2)))
	     (hash-n (yyhash--vec-copy
		      (make-vector new-size nil)
		      (yyhash--hash yh) old-size))
	     (index-size (yyhash-next-almost-prime
			  (floor (/ new-size yyhash-rehash-threshold))))
	     (index-n (make-vector index-size -1)))
	(do ((i old-size (1+ i)))
	    ((= i (1- new-size)) (aset next-n i -1))
	  (aset next-n i (+ i 1)))
	(setf (cl-struct-slot-value 'yyhash 'index yh) index-n)
	(setf (cl-struct-slot-value 'yyhash 'key-and-value yh) k-v-n)
	(setf (cl-struct-slot-value 'yyhash 'hash yh) hash-n)
	(setf (cl-struct-slot-value 'yyhash 'next yh) next-n)
	(setf (cl-struct-slot-value 'yyhash 'next-free yh) old-size)

	(do ((i 0 (1+ i)))
	    ((= i old-size))
	  (let ((hash-code (yyhash--hash-val yh i))
		(start-id (% hash-code index-size)))
	    (aset (yyhash--next yh) i (yyhash--index-val yh start-id))
	    (aset (yyhash--index yh) start-id i)))))))

(setq a (make-yyhash 10))

(putyyhash 1 2 a)

(putyyhash 'a 2 a)

a

(mapyyhash (lambda (x y) (prin1 x) (prin1 y)) a)

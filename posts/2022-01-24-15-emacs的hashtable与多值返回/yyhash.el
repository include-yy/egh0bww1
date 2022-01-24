;;; yyhash.el --- yy's hashtable -*- lexical-binding: t -*-

;; Author: include-yy
;; Maintainer: include-yy
;; Version: 0.1
;; Package-Requires: ((emacs) (cl-lib))
;; Homepage: https://gist.github.com/include-yy/4b30d26e2a8b8bcdd46c1bcd717b3756
;; Keywords: hashtable


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; just for fun, a toy hash table, use the source code of EMACS

;;; Code:

(defconst yyhash--rehash-size 1.5 "rehash size")
(defconst yyhash--rehash-threshold 0.8125 "threshold")
(defconst yyhash--defsize 65 "default size")

(defsubst yyhash--fixnum (n)
  "hash for fixnum"
  n)

(defsubst yyhash--symbol (s)
  "hash for symbol, use string's hash function"
  (yyhash--string (symbol-name s)))

(defsubst yyhash--combine (x y)
  "emacs use 61 bit for fixnum"
  (+ (lsh x 4) (lsh x -57) y))

(defsubst yyhash--reduce (n)
  "reduce to not great than fixnum max value"
  (logand (logxor n (lsh n -3)) most-positive-fixnum))

(defun yyhash--string (s)
  "hash for string"
  (let ((hash 0))
    (cl-loop
     for i across s
     do (cl-loop for x = i then (lsh x -8)
		 do (setq hash (yyhash--combine hash (logand x #xff)))
		 when (<= x #xff) return 0))
    (yyhash--reduce hash)))

(defun yyhash-float2list (m)
  "double to ieee-754 representation
return a list, cadr is 0 to 55th bit, car is 56th to 63th bit"
  (let* ((sgn (cl-signum m))
	 (abm (abs m))
	 (exp-n (truncate (log abm 2)))
	 (exp-bias (+ exp-n 1023))
	 (num (/ abm (expt 2 exp-n)))
	 (num52 (logand (floor (* num (lsh 1 52)))
			(1- (lsh 1 52))))
	 (num56 (+ num52 (lsh (logand 15 exp-bias) 52)))
	 (num8 (if (= sgn -1) (+ 128 (lsh exp-bias -4)) (lsh exp-bias -4))))
    (cl-values num8 num56)))

(defun yyhash--float (m)
  "hash for float"
  (if (or (isnan m)
	  (= (abs m) 1.0e+INF))
      0
    (cl-multiple-value-bind (a b) (yyhash-float2list m)
      (let ((hash a))
	(cl-loop
	 for x = b then (lsh x -8)
	 do (setq hash (yyhash--combine hash (logand x #xff)))
	 when (<= x #xff) return (yyhash--reduce hash))))))

(defconst yyhash--max-depth 3 "max depth for nest struct")
(defconst yyhash--max-len 7 "max length for list and vector")

(defun yyhash--list (ls depth)
  "hash for list"
  (let ((hash 0))
    (when (< depth yyhash--max-depth)
      (do ((i 0 (+ i 1)))
	  ((not (and (consp ls) (< i yyhash--max-len))))
	(let ((hash2 (yyhash--hashfn (car ls) (1+ depth))))
	  (setq hash (yyhash--combine hash hash2))
	  (pop ls))))
    (when (not (null ls))
      (let ((hash2 (yyhash--hashfn ls (1+ depth))))
	(setq hash (yyhash--combine hash hash2))))
    (yyhash--reduce hash)))

(defun yyhash--vec (v depth)
  "hash for vector"
  (let* ((hash (length v))
	 (n (min yyhash--max-len hash)))
    (cl-loop
     for i below n
     do (setq hash (yyhash--combine
		    hash
		    (yyhash--hashfn (aref v i) (1+ depth)))))
    (yyhash--reduce hash)))

(defun yyhash--hashfn (obj depth)
  "hash function"
  (if (> depth yyhash--max-depth) 0
    (let ((tp (type-of obj)))
      (cl-case tp
	((integer) (if (fixnump obj)
		       (yyhash--fixnum obj)
		     (error "not a fixnum obj: %d" obj)))
	((symbol) (yyhash--symbol obj))
	((string) (yyhash--string obj))
	((float) (yyhash--float obj))
	((cons) (yyhash--list obj depth))
	((vector) (yyhash--vec obj depth))
	(otherwise (error "unknown type obj: %s" obj))))))

(cl-defstruct (yyhash
	       (:conc-name yyhash--s-)
	       (:constructor yyhash--s-create)
	       (:copier nil)
	       (:predicate yyhash-p))
  "yy's hash table struct"
  (hash nil :type vector
	:documentation "vector of hash codes.
if entry I is free, hash[i] should be nil")
  (next nil :type vector
	:documentation "vector used to chain entries")
  (index nil :type vector
	 :documentation "Bucket vector.")
  (count 0 :type integer
	 :documentation "Number of key/value entries in the table")
  (next-free 0 :type integer
	     :documentation "index of first free entry in free list,
or -1 if none")
  (key-and-value nil :type vector
		 :documentation "vector of keys and values.
key of item I is at index (* I 2), value is at (1+ (* I 2))"))

(defun yyhash--symbol-concat (&rest names)
  "generate symbol from string"
  (intern (apply 'concat (mapcar 'symbol-name names))))

(defmacro yyhash--s-gen-get/setter (prefix slots)
  "generate vector getter and setter"
  (let ((names-get (mapcar (lambda (x)
			     (yyhash--symbol-concat prefix x '-getv))
			   slots))
	(names-set (mapcar (lambda (x)
			     (yyhash--symbol-concat prefix x '-setv))
			   slots))
	(res nil))
    (cl-loop for i in names-get
	     for j in names-set
	     for k in slots
	     do (push `(defsubst ,i (yh n)
			 (aref (cl-struct-slot-value 'yyhash ',k yh) n))
		      res)
	     do (push `(defsubst ,j (yh n newval)
			 (aset (cl-struct-slot-value 'yyhash ',k yh) n newval))
		      res))
    (cons 'progn res)))

(yyhash--s-gen-get/setter yyhash--
			  (hash
			   next
			   index
			   key-and-value))

(defun yyhash--key (yh n)
  "get entry[n]'s key"
  (yyhash--key-and-value-getv yh (* n 2)))
(defun yyhash--value (yh n)
  "get entry[n]'s value"
  (yyhash--key-and-value-getv yh (1+ (* n 2))))

(defconst yyhash--biggest 100000)

(defun yyhash--next-almost-prime (n)
  "create a pesudo prime number bigger then n"
  (do ((n (logior n 1) (+ n 2)))
      ((and (not (zerop (% n 3)))
	    (not (zerop (% n 5)))
	    (not (zerop (% n 7))))
       n)))

(defun yyhash-make (&optional size)
  "make a hash table. size must be nonnegative integer if provided"
  (cl-assert (or (null size) (and (integerp size) (>= size 0))))
  (let* ((size (if size (if (= size 0) 1 size) yyhash--defsize))
	 (h-size (min size yyhash--biggest))
	 (k-v (make-vector (* h-size 2) nil))
	 (next (make-vector h-size -1))
	 (i-size (yyhash--next-almost-prime
		  (floor (/ h-size yyhash--rehash-threshold))))
	 (index (make-vector i-size -1))
	 (hash (make-vector h-size nil)))
    (do ((i 0 (+ i 1)))
	((= i (1- h-size)))
      (aset next i (+ i 1)))
    (yyhash--s-create
     :hash hash :next next :index index :key-and-value k-v
     :count 0 :next-free 0)))

(defun yyhash-copy (yh)
  "make a copy of hash table"
  (let ((k-v (copy-sequence (yyhash--s-key-and-value yh)))
	(hash (copy-sequence (yyhash--s-hash yh)))
	(next (copy-sequence (yyhash--s-next yh)))
	(index (copy-sequence (yyhash--s-index yh))))
    (yyhash--s-create
     :hash hash :next next :index index :key-and-value k-v
     :count (yyhash--s-count yh)
     :next-free (yyhash--s-next-free yh))))

(defun yyhash-count (yh)
  "get number of entry in hash table"
  (yyhash--s-count yh))
(defun yyhash-size (yh)
  "get size of hash table"
  (length (yyhash--s-hash yh)))
(defun yyhash--isize (yh)
  "get size of bucket vector of hashtable"
  (length (yyhash--s-index yh)))

;;start

(defun yyhash--lookup (key yh &optional cons-pt)
  "look key in yh hastable, set car of cons-pt to hash code if provided"
  (let* ((hash-code (yyhash--hashfn key 0))
	 (start-of-bucket (% hash-code (yyhash--isize yh)))
	 (find-i
	  (do ((i (yyhash--index-getv yh start-of-bucket)
		  (yyhash--next-getv yh i)))
	      ((or (< i 0)
		   (and
		    (equal key (yyhash--key yh i))
		    (= hash-code (yyhash--hash-getv yh i))))
	       i))))
    (when cons-pt (setcar cons-pt hash-code))
    find-i))

(defun yyhash-get (key yh &optional default)
  "get entry's value, or default(nil) if not found"
  (let ((i (yyhash--lookup key yh nil)))
    (if (>= i 0) (yyhash--value yh i) default)))

(defun yyhash-clr (yh)
  "clear headtable"
  (when (> (yyhash-count yh) 0)
    (let ((size (yyhash-size yh))
	  (isize (yyhash--isize yh)))
      (cl-loop for i below size
	       do (yyhash--hash-setv yh i nil)
	       do (yyhash--next-setv yh i (+ i 1))
	       do (yyhash--key-and-value-setv yh (* 2 i) nil)
	       do (yyhash--key-and-value-setv yh (1+ (* 2 i)) nil)
	       finally do (yyhash--next-setv yh (1- size) -1))
      (cl-loop for i below isize
	       do (yyhash--index-setv yh i -1))
      (setf (yyhash--s-next-free yh) 0)
      (setf (yyhash--s-count yh) 0)
      yh)))

(defun yyhash-rem (key yh)
  "rem key's entry from yh, or do nothing if not found"
  (let* ((hash-code (yyhash--hashfn key 0))
	 (start-id (% hash-code (yyhash--isize yh)))
	 (prev -1)
	 (fd (do ((i (yyhash--index-getv yh start-id)
		     (yyhash--next-getv yh i)))
		 ((or (< i 0)
		      (and (equal key (yyhash--key yh i))
			   (= hash-code (yyhash--hash-getv yh i))))
		  i)
	       (setq prev i))))
    (when (>= fd 0)
      (if (< prev 0)
	  (yyhash--index-setv yh start-id (yyhash--next-getv yh fd))
	(yyhash--next-setv yh prev (yyhash--next-getv yh fd)))
      (yyhash--key-and-value-setv yh (* fd 2) nil)
      (yyhash--key-and-value-setv yh (1+ (* fd 2)) nil)
      (yyhash--hash-setv yh fd nil)
      (yyhash--next-setv yh fd (yyhash--s-next-free yh))
      (setf (yyhash--s-next-free yh) fd)
      (cl-decf (yyhash--s-count yh))
      (cl-assert (>= (yyhash-count yh) 0)))
    nil))

(defun yyhash-map (fn yh)
  "use function fn to map hashtable"
  (let* ((k-v (yyhash--s-key-and-value yh))
	 (len (yyhash-size yh)))
    (do ((i 0 (+ i 2))
	 (j 1 (+ j 2))
	 (k 0 (+ k 1)))
	((= k len))
      (and (not (null (aref k-v i)))
	   (funcall fn (aref k-v i) (aref k-v j))))))

(defun yyhash--putnew (key val yh hash)
  "add new item to hashtable"
  (yyhash--maybe-resize yh)
  (incf (yyhash--s-count yh))
  (let ((i (yyhash--s-next-free yh))
	(start-id (% hash (yyhash--isize yh))))
    (cl-assert (null (yyhash--hash-getv yh i)))
    (cl-assert (null (yyhash--key yh i)))
    (setf (yyhash--s-next-free yh) (yyhash--next-getv yh i))
    (yyhash--key-and-value-setv yh (* i 2) key)
    (yyhash--key-and-value-setv yh (1+ (* i 2)) val)
    (yyhash--hash-setv yh i hash)
    (yyhash--next-setv yh i (yyhash--index-getv yh start-id))
    (yyhash--index-setv yh start-id i)
    i))

(defun yyhash-put (key val yh)
  "put k-v entry to hashtable, if key exist's, use val repalce oldval"
  (cl-assert (not (null key)))
  (let* ((hash-c (list nil))
	 (i (yyhash--lookup key yh hash-c))
	 (hash (car hash-c)))
    (if (>= i 0) (yyhash--key-and-value-setv yh (1+ (* i 2)) val)
      (yyhash--putnew key val yh hash))
    val))

(defun yyhash--vec-copy (new old len)
  "copy old vec's values to new vec"
  (cl-loop for i below len
	   do (aset new i (aref old i)))
  new)

(defun yyhash--maybe-resize (yh)
  "extend hashtables' size"
  (when (< (yyhash--s-next-free yh) 0)
    (let* ((old-size (yyhash-size yh))
	   (new-size (floor (* old-size yyhash--rehash-size))))
      (when (> new-size yyhash--biggest)
	(setq new-size yyhash-biggest))
      (when (= old-size yyhash--biggest)
	(error "yyhash is up to biggest"))
      (let* ((next-n (yyhash--vec-copy
		      (make-vector new-size -1)
		      (yyhash--s-next yh) old-size))
	     (k-v-n (yyhash--vec-copy
		     (make-vector (* 2 new-size) nil)
		     (yyhash--s-key-and-value yh) (* old-size 2)))
	     (hash-n (yyhash--vec-copy
		      (make-vector new-size nil)
		      (yyhash--s-hash yh) old-size))
	     (index-size (yyhash--next-almost-prime
			  (floor (/ new-size yyhash--rehash-threshold))))
	     (index-n (make-vector index-size -1)))
	(do ((i old-size (1+ i)))
	    ((= i (1- new-size)) (aset next-n i -1))
	  (aset next-n i (+ i 1)))
	(setf (yyhash--s-index yh) index-n)
	(setf (yyhash--s-key-and-value yh) k-v-n)
	(setf (yyhash--s-hash yh) hash-n)
	(setf (yyhash--s-next yh) next-n)
	(setf (yyhash--s-next-free yh) old-size)

	(do ((i 0 (1+ i)))
	    ((= i old-size))
	  (let* ((hash-code (yyhash--hash-getv yh i))
		 (start-id (% hash-code index-size)))
	    (yyhash--next-setv yh i (yyhash--index-getv yh start-id))
	    (yyhash--index-setv yh start-id i)))))))

(provide 'yyhash)

;;; yyhash.el ends here

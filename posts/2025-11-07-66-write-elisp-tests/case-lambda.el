(defun case-lambda--pure-list-p (x)
  (do ((x x (cdr x)))
      ((not (consp x)) (null x))))

(defmacro case-lambda--n-regular (x)
  `(caar ,x))
(defmacro case-lambda--restp (x)
  `(cdar ,x))
(defmacro case-lambda--restvar (x)
  `(cdar ,x))
(defmacro case-lambda--arglist-body (x)
  `(cdr ,x))
(defmacro case-lambda--arglist (x)
  `(car (case-lambda--arglist-body ,x)))
(defmacro case-lambda--body (x)
  `(cdr (case-lambda--arglist-body ,x)))
(defun case-lambda--make-clause (n-regular restvar arglist body)
  (cons (cons n-regular restvar) (cons arglist body)))

(defun case-lambda--analyze (clauses)
  "convert clauses litteral to internal structure"
  (let ((res nil))
    (dolist (c clauses (nreverse res))
      (or (consp c)
          (case-lambda--pure-list-p c)
          (case-lambda--pure-list-p (car c))
        (error "Bad case-lambda clauses"))
      (let* ((arglist (car c))
             (restp (member '&rest arglist))
             (len-after-rest (and restp (length restp)))
             (n-regular (if (not restp)
                          (length arglist)
                          (- (length arglist) len-after-rest))))
        (and restp (/= 2 len-after-rest)
          (error "Bad case-lambda clauses"))
        (push (case-lambda--make-clause
                n-regular (cadr restp) (cl-subseq arglist 0 n-regular) (cdr c))
              res)))))

(defun case-lambda--remove-unreachable (clauses)
  "remove unreachable clauses and return reversed result"
  (let ((res nil)
        (max-n-regular -1)
        (restp nil))
    (do ((cs clauses (cdr cs)))
        ((null cs) res)
      (let ((c (car cs)))
        (or restp
          (cond ((case-lambda--restp c)
                 (setq restp t)
                 (push c res))
                ((> (case-lambda--n-regular c) max-n-regular)
                 (setq max-n-regular (case-lambda--n-regular c))
                 (push c res))))))))

(defmacro case-lambda--build-bind-if-tree ()
  ;; only used in case-lambda, rely on side effect
  ;; don't ever edit this unless understanding how is it going
  `(let ((n-regular (case-lambda--n-regular c))
         (nc-n-regular (if nextc (case-lambda--n-regular nextc) 0)))
     (do ((i (- n-regular 1) (- i 1)))
         ((< i nc-n-regular))
       (setq resform
         `(if (null ,input)
            ,(if (= i (- n-regular 1))
               `(let (,@(cl-mapcar #'list (case-lambda--arglist c) regular-tmps))
                  ,@(case-lambda--body c))
               error-form)
            ,resform))
       (when (and restp (= i rest-at))
          (setq resform
           `(let ((,rest-var ,input))
              ,resform)))
       (setq resform
         `(let ((,(nth i regular-tmps) (car ,input)))
            (setq ,input (cdr ,input))
            ,resform))
       (when (and restp (<= i rest-at))
         (setq error-form '(error "case-lambda matching failure"))))))

(defun case-lambda--main-pattern (clauses)
  ;; don't ever edit this unless understanding how is it going
  ;; note that clauses is current reversed
  (let* ((restp (case-lambda--restp (car clauses)))
         (rest-clause (car clauses))
         (rest-clause-n-regular
           (and restp (case-lambda--n-regular rest-clause)))
         (first-clause-with-only-regulars-n-regular
           (if restp (case-lambda--n-regular (cadr clauses))
                     (case-lambda--n-regular (car clauses))))
         (max-n-regular
           (if restp
             (max rest-clause-n-regular
                  first-clause-with-only-regulars-n-regular)
             first-clause-with-only-regulars-n-regular))
         (regular-tmps
           (let ((res nil))
             (dotimes (i max-n-regular res)
               (push (gensym) res))))
         (rest-var (and restp (case-lambda--restvar rest-clause)))
         (input (gensym))
         (rest-at (and restp (- rest-clause-n-regular 1)))
         (rest-clause-form 
           `(let (,@(cl-mapcar #'list (case-lambda--arglist rest-clause)
                                 regular-tmps)
                   ,@(if (and restp (= max-n-regular rest-clause-n-regular))
                       `((,rest-var ,input)) nil))
              ,@(case-lambda--body rest-clause)))
         (error-form
           (if restp rest-clause-form '(error "case-lambda matching failure")))
         (resform error-form))
    ;; main loop
    (do ((cs clauses (cdr cs)))
        ((null (cdr cs)) (setq clauses cs))
      (let ((c (car cs))
            (nextc (cadr cs)))
        (case-lambda--build-bind-if-tree)))
    ;; handle the begining
    (if (= 0 (case-lambda--n-regular (car clauses)))
      (setq resform
        `(if (null ,input)
           ,@(case-lambda--body (car clauses))
           ,resform))
      (let ((c (car clauses))
            (nextc nil))
        (case-lambda--build-bind-if-tree)
        (setq resform
          `(if (null ,input)
             ,(if (and restp (= 0 rest-clause-n-regular))
                rest-clause-form
                error-form)
             ,resform))))
    (and restp (= 0 rest-clause-n-regular)
      (setq resform
        `(let ((,rest-var ,input))
           ,resform)))
    `(lambda (&rest ,input)
       ,resform)))

(defmacro case-lambda (&rest clauses)
  "return a anoymous function that dispatch on arguments number.
(case-lambda (formals body ...) ...)
where formals = (id ... [&rest id])"
  (let ((clauses (case-lambda--remove-unreachable
                   (case-lambda--analyze clauses))))
    (cond ((null clauses) '(lambda () nil))
          ((null (cdr clauses))
           (cons 'lambda (case-lambda--arglist-body (car clauses))))
          (t (case-lambda--main-pattern clauses)))))

; (macroexpand '(case-lambda ((x y) (+ x y)) ((x y z) (- x y z))))
; (macroexpand '(case-lambda ((x y) (+ x y)) ((w x y z) (- w x y z))))
; (macroexpand '(case-lambda ((x y z) (- x y z)) ((&rest xs) (cons 1 xs))))
; (macroexpand '(case-lambda ((x y z) (- x y z)) ((x &rest xs) (list 1 x xs))))
; (macroexpand '(case-lambda
;                 ((x y z) (- x y z))
;                 ((a b c d e &rest xs) (list a b c d e xs))))

(defmacro case-defun (name &rest xs)
  "define a function named NAME that dispatch on arguments number.
(case-defun name (formals body ...) ...)
where formals = (id ... [&rest id])"
  `(setf (symbol-function ',name)
     (case-lambda ,@xs)))

(defmacro case-defmacro (name &rest xs)
  "define a macro named NAME that dispatch on arguments number.
(case-defmacro name (formals body ...) ...)
where formals = (id ... [&rest id])"
  (let ((x (gensym)))
    `(defmacro ,name (&rest ,x)
       (apply (case-lambda ,@xs) ,x))))

; (case-defun range
;   ((x) (range 0 x 1))
;   ((x y) (range x y 1))
;   ((x y z)
;    (loop for i from x below y by z collect i)))

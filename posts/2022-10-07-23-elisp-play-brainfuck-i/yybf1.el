;; yybf1.el brainfuck implemented in elisp -*- lexical-binding:t; -*-

(defun ☉-charp (x)
  "判断字符是否是八个字符之一
用在 tokenize 阶段"
  (and (char-or-string-p x)
       (memq x '(?> ?< ?+ ?- ?. ?, ?\[ ?\]))))

(defun ☉-token (s)
  "将字符串拆分为 token 序列，去掉注释，添加字符的位置信息 (行,列)"
  (let ((current-char-idx 1)
	(current-line-idx 1))
    (cl-loop for c across s
	     do (cond
		 ((= c ?\n)
		  (setq current-char-idx 1)
		  (cl-incf current-line-idx))
		 (t
		  (cl-incf current-char-idx)))
	     if (☉-charp c)
	     collect (list c current-line-idx current-char-idx)
	     end)))

(defun ☉-parse (tos)
  "检查 [] 配对情况，根据位置信息报错
最后得到指令序列表"
  (let* ((round-cnt 0)
	 (errmsg1 "too many ] at line %s, col %s")
	 (errmsg2 "too many [")
	 (pas
	  (cl-loop for c on tos
		   do
		   (let* ((c0 (car c))
			  (ch (car c0))
			  (row (cadr c0))
			  (col (caddr c0)))
		     (cond
		      ((= ch ?\[) (cl-incf round-cnt))
		      ((= ch ?\]) (if (not (zerop round-cnt))
				      (cl-decf round-cnt)
				    (user-error errmsg1 row col)))))
		   collect (caar c)
		   finally do
		   (if (not (zerop round-cnt))
		       (user-error errmsg2)))))
    pas))

(defun ☉-parser (str)
  "解析函数"
  (☉-parse (☉-token str)))

(defun ☉-ast2link (ast)
  "将指令序列由 a -> b -> c 转换为 a <- b -> c (当前在 b)
方便指令指针前后移动
表的 car 为当前指令，cadr 为当前到表头的表，caddr 为当前到表尾的表"
  (list (car ast) nil (cdr ast)))

(defun ☉-> (link)
  "将指令表向右移动一格
返回新的指令表"
  (let ((a (car link))
	(left (cadr link))
	(right (caddr link)))
    (cl-assert right)
    (list (car right) (cons a left) (cdr right))))

(defun ☉-< (link)
  "将指令表向左移动一格
返回新的指令表"
  (let ((a (car link))
	(left (cadr link))
	(right (caddr link)))
    (cl-assert left)
    (list (car left) (cdr left) (cons a right))))

(defun ☉-next-br (link)
  "将指令表移动到对应当前 [ 的 ]
返回新的指令表"
  (cl-assert (= (car link) ?\[))
  (let ((round-cnt 0)
	(found nil)
	(curr-link link))
    (while (not found)
      (cond
       ((and (= (car curr-link) ?\])
	     (= round-cnt 1))
	(setq found t))
       ((= (car curr-link) ?\[)
	(cl-incf round-cnt))
       ((= (car curr-link) ?\])
	(cl-decf round-cnt)))
      (unless found
	(setq curr-link (☉-> curr-link))))
    curr-link))

(defun ☉-prev-br (link)
  "将指令表移动当当前 ] 对应的 [
返回新的指令表"
  (cl-assert (= (car link) ?\]))
  (let ((round-cnt 0)
	(found nil)
	(curr-link link))
    (while (not found)
      (cond
       ((and (= (car curr-link) ?\[)
	     (= round-cnt 1))
	(setq found t))
       ((= (car curr-link) ?\])
	(cl-incf round-cnt))
       ((= (car curr-link) ?\[)
	(cl-decf round-cnt)))
      (unless found
	(setq curr-link (☉-< curr-link))))
    curr-link))

(defun ☉-input ()
  (read-char "brainfuck>"))
(defun ☉-output (c)
  (write-char c))

(defun ☉-interpreter (ast &optional i-fn o-fn len)
  "解释器部分，接收 ast 并执行
接受 i-fn, o-fn 替换掉默认的输入/输出函数
接受 len 指定数据数组的长度，默认为 30000
返回修改后的数据数组"
  (setq i-fn (or i-fn '☉-input))
  (setq o-fn (or o-fn '☉-output))
  (setq len (or len 30000))
  (let* ((ir (☉-ast2link ast))
	 (ar 0)
	 (v (make-vector len 0))
	 (vl (length v))
	 (exit (null ast)))
    (while (not exit)
      (cl-case (car ir)
	((?+) (if (< (aref v ar) most-positive-fixnum)
		  (cl-incf (aref v ar))
		(error "overflow most positive fixnum")))
	((?-) (if (> (aref v ar) most-negative-fixnum)
		  (cl-decf (aref v ar))
		(error "underflow most negative fixnum")))
	((?>) (if (< ar vl) (cl-incf ar)
		(error "overflow array index")))
	((?<) (if (> ar 0) (cl-decf ar)
		(error "underflow array index")))
	((?.) (funcall o-fn (aref v ar)))
	((?,) (setf (aref v ar) (funcall i-fn))))
      (cl-case (car ir)
	((?\[) (if (zerop (aref v ar))
		   (setq ir (☉-next-br ir))
		 (setq ir (☉-> ir))))
	((?\]) (if (not (zerop (aref v ar)))
		   (setq ir (☉-prev-br ir))
		 (if (null (caddr ir))
		     (setq exit t)
		   (setq ir (☉-> ir)))))
	(t
	 (if (null (caddr ir))
	     (setq exit t)
	   (setq ir (☉-> ir))))))
    v))

(defun ☉-execute (str &optional ifn ofn len)
  "接口函数，接收字符串并执行"
  (☉-interpreter (☉-parser str) ifn ofn len))

(defun ☉-reader-gen (str)
  "生成基于字符串的输入函数"
  (let ((idx 0)
	(len (length str)))
    (lambda ()
      (if (>= idx len)
	  0
	(prog1
	    (aref str idx)
	  (cl-incf idx))))))

(provide 'yybf1)

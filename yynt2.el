;; yynt2.el -- yynt.el 的改进版 -*- lexical-binding: t; -*-

;;;; comment

;; 相比于 yynt.el 使用 advice 修改 ox-html 而言使用了新的后端，不影响 ox-html 默认行为
;; 添加了一些功能：
;; - 路径运算
;; - 生成 rss
;; - 区别于 org-publish 的项目工具
;; - ...

;; 请使用 `eval-buffer' 加载 yynt，且保证 yynt.el 在项目根目录上
;; 在 emacs 29 中，可以通过 C-c C-e 调用 `elisp-eval-region-or-buffer'

;;; requests
;; 加载自带的 htmlize
;; 来自 https://github.com/hniksic/emacs-htmlize
(require 'htmlize (expand-file-name "./htmlize.el"))
;; 加载魔改版 html 后端
(load-file (expand-file-name "./ox-yyhtml.el"))
;; 加载 ox-org，方便调试
;; C-c C-e O O 导出 org 文件到 org，主要用于宏展开
(require 'ox-org)
;;; 一些语言 mode，高亮需要
(require 'bnf-mode nil t)
(require 'haskell-mode nil t)

;;;; code

;; 项目根目录
(defvar yynt-basedir (expand-file-name "./"))

;; 一些全局资源目录
;; 如果有新的路径，在此添加
(defconst yynt-d-css "css")
(defconst yynt-d-img "img")
(defconst yynt-d-js  "js")

;; 一些扩展名，使用变量以免写错
(defconst yynt-HTML "html")
(defconst yynt-HTM  "htm")
(defconst yynt-ORG  "org")
(defconst yynt-XML  "xml")
(defconst yynt-JS   "js")
(defconst yynt-CSS  "css")
(defconst yynt-PPT  "pptx?")
(defconst yynt-JPG  "je?pg")
(defconst yynt-PNG  "png")
(defconst yynt-SVG  "svg")

;; 一些全局的插入函数
(defun yynt-imgattr ()
  "插入用于头图和尾图的 CSS"
  (interactive)
  (insert "#+ATTR_HTML: :class top-down-img"))
(defun yynt-headimg ()
  "插入头部图片所需内容"
  (interactive)
  (insert "{{{begin_noscript}}}\n")
  (insert "#+NAME: headimg\n")
  (yynt-imgattr)
  (insert "\n\n{{{end_noscript}}}"))
(defun yynt-tailimg ()
  "插入尾部图片所需内容"
  (interactive)
  (insert "{{{begin_noscript}}}\n")
  (insert "#+NAME: tailimg\n")
  (yynt-imgattr)
  (insert "\n\n{{{end_noscript}}}"))
(defun yynt-details ()
  "插入 <details> 和 <summary> 标签表示"
  (interactive)
  (insert "{{{begin_details}}}\n")
  (insert "{{{summary()}}}\n\n")
  (insert "{{{end_details}}}"))

;;; file api
;; 提供一些方便使用的文件操作 api
;; 参考了 f.el: https://github.com/rejeep/f.el
(defun yynt-fabsp (fname)
  "判断路径是否为绝对路径"
  (file-name-absolute-p fname))
(defun yynt-frelap (fname)
  "判断路径是否为相对路径"
  (not (yynt-fabsp fname)))
(defun yynt-frela (fname &optional dir)
  "获取路径的相对路径名
内部调用了 `file-relative-name'"
  (file-relative-name fname dir))
(defun yynt-fexpand (path &optional dir)
  "将路径根据 `dir' 展开
内部调用了 `expand-file-name'"
  (let (file-name-handler-alist)
    (expand-file-name path dir)))
(defun yynt-fjoin (&rest args)
  "根据参数合成路径
若某个参数为绝对路径，那么它会舍弃绝对路径前的参数"
  (let (path
        (relative (yynt-frelap (car args))))
    (mapc
     (lambda (arg)
       (setq path (cond ((not path) arg)
                        ((yynt-fabsp arg)
                         (progn
                           (setq relative nil)
                           arg))
                        (t (yynt-fexpand arg path)))))
     args)
    (if relative (yynt-frela path) path)))
(defun yynt-fbase (path)
  "返回 PATH 的文件名，不含扩展名和目录"
  (file-name-base path))
(defun yynt-fsame-p (path-a path-b)
  "若两个目录指向同一文件则返回 t"
  (equal (file-truename (directory-file-name (yynt-fexpand path-a)))
	 (file-truename (directory-file-name (yynt-fexpand path-b)))))
(defun yynt-fname (path)
  "返回文件名
对目录也适用，可以返回路径的最后一个目录"
  (file-name-nondirectory (directory-file-name path)))
(defun yynt-fdir (path)
  "返回目录名，不带 /
对目录也适用，返回除最后目录外的其他部分"
  (let ((parent (file-name-directory
                 (directory-file-name (yynt-fexpand path default-directory)))))
    (unless (yynt-fsame-p path parent)
      (if (yynt-frelap path)
          (directory-file-name (yynt-frela parent))
        (directory-file-name parent)))))
(defun yynt-fno-ext (path)
  "返回不含扩展名的文件路径"
  (file-name-sans-extension path))
(defun yynt-fswap-ext (path ext)
  "替换文件扩展名"
  (cl-assert (and (stringp ext) (not (string= "" ext))))
  (concat (yynt-fno-ext path) "." ext))

;; 一些基于 f.el 的函数扩充
(defun yynt-get-fullpath (path)
  "根据相对于根目录的路径获取绝对路径"
  (yynt-fjoin yynt-basedir path))

(defun yynt-get-cdpath (s-path t-path)
  "计算从 `s-path' cd 到 `t-path' 所需的路径
要求为绝对路径，不含文件名"
  (yynt-frela t-path s-path))

(defun yynt-rela-cdpath (s-path t-path)
  "s-path 和 t-path 会使用 `yynt-get-fullpath' 扩充然后调用 `yynt-get-cdpath'
不要含文件名"
  (yynt-get-cdpath
   (yynt-get-fullpath s-path)
   (yynt-get-fullpath t-path)))


;;; 一些 html 代码生成函数
(defun yynt-html-gen-css (path)
  "生成 css 引用"
  (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">" path))
(defun yynt-html-gen-ico (path)
  "生成 icon 引用"
  (format "<link rel=\"icon\" type=\"image/x-icon\" href=\"%s\">" path))
(defun yynt-html-gen-js (path &optional async)
  "生成 javascript 文件引用"
  (format "<script %ssrc=\"%s\"></script>"
	  (if async "async " "") path))
(defun yynt-html-gen-img (path &optional alt)
  "生成图片引用"
  (format "<img src=\"%s\" alt=\"%s\">"
	  path (or alt "Load failed")))
(defun yynt-html-gen-a (link content &optional others)
  "生成链接"
  (format "<a href=\"%s\" %s>%s</a>"
	  link (or others "") content))

(defun yynt-html-google-font-roboto ()
  "生成 google 的 roboto 字体引用"
  "\
<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
<link href=\"https://fonts.googleapis.com/css2?family=Roboto&display=swap\" rel=\"stylesheet\">")

(defun yynt-html-concat-newline (&rest args)
  "将所有字符串连接起来"
  (mapconcat 'identity args "\n"))

(defvar yynt--rela-current-path nil
  "用于生成相对路径的临时变量")
(defun yynt-html-r (t-dir name &optional prefix)
  "生成到 `t-dir' 中的 `name' 文件的相对路径，以 `yynt--rela-current-path' 为基准"
  (cl-assert (stringp yynt--rela-current-path))
  (yynt-fjoin (or prefix "")
	      (yynt-rela-cdpath yynt--rela-current-path
				t-dir)
	      name))

;;; 提供一些通用的函数
;;; 同时针对单层目录和双层目录做一下抽象，分为 p1 和 p2 系列函数
;;; - 单层目录指的是目录下有很多 org 文件
;;; - 多层目录指目录下有很多子目录，子目录内有 index.org 作为主文件

(defun yynt-get-file-info (filename infos)
  "获取文件头的一些以 #+NAME: value 为格式的属性
此处只取文件的 4096 字节数据，毕竟标题和 TAG 写在文件的最前面
感觉 1024 也许就足够了"
  (cl-assert (file-exists-p filename))
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents filename nil 0 4096)
      (let (res)
	(dolist (a infos)
	  (goto-char (point-min))
	  (when (search-forward-regexp
		 (concat "^\\#\\+" a ":[ ]*\\(.+\\)$") nil t)
	    (push (cons a (match-string 1)) res)))
	res))))

(defvar yynt--match-anything ".*"
  "匹配任何字符串的正则")
(defvar yynt--match-nothing "^$"
  "不匹配任何字符串的正则")
(defun yynt--always-return-true (&rest args)
  "总是返回 t 的函数"
  t)

;; 对于二级目录的子项，可以认为主要内容位于各二级目录内
;; 而一级目录用来存储少许汇总性信息，比如文章列表，tag 汇总，等等
;; 也可以是一些静态的资源文件
(defun yynt-p2-getdirs (path regexp-or-pred pred)
  "对于双层目录，获取满足条件的二次目录
regexp-or-pred 用来匹配目录，pred 用来判断二级目录是否符合条件，比如是否含有某个文件
若非 `nil'，pred 的返回值会被作为 yynt-p2-getdirs 的一部分
返回相对目录和二次目录文件的绝对路径组成的表
((dirname . fullpath) ...)
这里的相对目录可以认为是一层目录下的 UUID"
  (let* ((files (directory-files
		 path t
		 directory-files-no-dot-files-regexp))
	 (dirs (seq-filter (lambda (x)
			     (and (file-directory-p x)
				  (if (stringp regexp-or-pred)
				      (string-match-p regexp-or-pred (yynt-fbase x))
				    (funcall regexp-or-pred (yynt-fbase x)))))
			   files))
	 res)
    (dolist (a dirs)
      (when-let ((p (funcall pred a)))
	(push (cons (yynt-fbase a) p) res)))
    (reverse res)))

;; 加个更加强力的 `yynt-p2-getdirs' 版本......
(defun yynt-p2-getdirs-ex (path rop rop2)
  "相比于 `yynt-p2-getdirs' ，该函数可以由 rop2 获取多个文件并添加到列表中
若还按照 `yynt-p2-getdirs' 则此时目录无法作为 UUID 使用，因为可能存在同一目录下的多个文件
故此时取文件而不是目录
与 `yynt-p2-getdirs' 不同的是，它的 rop 和 rop2 针对的 *都* 是相对目录而非绝对目录
且此时 rop2 为 pred 时只需返回布尔值即可"
  (let* ((files (directory-files
		 path t
		 directory-files-no-dot-files-regexp))
	 (dirs (seq-filter (lambda (x)
			     (and (file-directory-p x)
				  (if (stringp rop)
				      (string-match-p rop (yynt-fbase x))
				    (funcall rop (yynt-fbase x)))))
			   files))
	 res)
    (dolist (a dirs)
      (let* ((base-a (yynt-fbase a))
	     (fs (directory-files
		  a nil
		  directory-files-no-dot-files-regexp))
	     (r (seq-filter (lambda (x)
			      (if (stringp rop2)
				  (string-match-p rop2 x)
				(funcall rop2 x)))
			    fs)))
	(while r
	  (let ((pp (pop r)))
	    (push (cons (yynt-fjoin base-a pp)
			(yynt-fjoin a pp))
		  res)))))
    (reverse res)))

;; 对于博客而言，我使用 time-title 来作为文件夹名
(defun yynt-get-dated-2xxx-under-dir (dirname)
  "获取某目录下的所有以 2xxx 开头的目录，以及其中的 index.org|html 文件绝对路径"
  (yynt-p2-getdirs
   (yynt-get-fullpath dirname) "^2"
   (lambda (x)
     (when-let ((org (yynt-fjoin x "index.org"))
		(htm (yynt-fjoin x "index.htm"))
		(ret (cl-find-if 'file-exists-p (list org htm))))
       ret))))

;; 对于一级目录的子项，所有项都在这一层目录里，相比二级目录比较简单
;; 可能存在一些统一存放资源的文件夹，以及少数汇总式文件
(defun yynt-p1-getfiles (path regexp-or-pred)
  "与 yynt-p2-getdirs 类似，不过只针对一层
regexp-or-pred 用来匹配文件，若为函数，则非 `nil' 表示匹配
返回文件名（无扩展名）和文件的绝对路径组成的表
((barename . fullpath) ...)
文件名可以认为是 UUID"
  (let* ((all (directory-files
		 path t
		 directory-files-no-dot-files-regexp))
	 (files (seq-filter (lambda (x)
			     (and (file-regular-p x) ;; f-file-p
				  (if (stringp regexp-or-pred)
				      (string-match-p regexp-or-pred (yynt-fname x))
				    (funcall regexp-or-pred (yynt-fname x)))))
			    all)))
    (mapcar (lambda (x) (cons (yynt-fbase x) x)) files)))

(defun yynt-p-get-info (list klist)
  "根据 klist 中的关键字从 list 中的文件中获取信息
list 的格式为 ((dir . fullname)...)
返回值为 (((dir . fullname) . ((p1 . v1) (p2 . v2))) ...)"
  (cl-mapcar 'cons list
	     (mapcar (lambda (x)
		       (yynt-get-file-info (cdr x) klist))
		     list)))

(defun yynt-p-lift (fn)
  "将接受 (name path alist) 为参数的函数提升为能处理 ((name . path) . alist)
的单参函数，让我们能够方便地使用一系列 map, reduce 函数"
  (pcase-lambda (`((,name . ,path) . ,alist))
    (funcall fn name path alist)))

(defun yynt-p-highorder-gen (fn)
  "生成能处理多参版本的 map 或 reduce 系列函数"
  (lambda (f &rest args)
    (apply fn (yynt-p-lift f) args)))

(defalias 'yynt-p-mapcar (yynt-p-highorder-gen 'mapcar)
  "mapcar 的 yynt 版本，接受的函数参数列表为 (name path alist)")

(defalias 'yynt-p-mapconcat (yynt-p-highorder-gen 'mapconcat)
  "mapconcat 的 yynt 版本")

(defalias 'yynt-p-remove-if (yynt-p-highorder-gen 'cl-remove-if)
  "cl-remove-if 的 yynt 版本")

(defalias 'yynt-p-remove-if-not (yynt-p-highorder-gen 'cl-remove-if-not)
  "cl-remove-if-not 的 yynt 版本")

;;;; 区别于 org-publish 的构建功能

;;; 添加一个构建信息 logger，不使用 message
(defvar yynt--logger "*yynt*"
  "用于输出 log 消息的 buffer 名")
(defun yynt--create-logger ()
  "创建 logger buffer ，且提供一些快捷键"
  (let ((buf (get-buffer-create yynt--logger)))
    (with-current-buffer buf
      (messages-buffer-mode))
    buf))
(defun yynt-gen-log (str)
  "向 logger buffer 写入 log 信息"
  (with-current-buffer (yynt--create-logger)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str "\n"))))
(defun yynt-show-log ()
  (interactive)
  (display-buffer (get-buffer-create yynt--logger)))

;;; 简单的 sqlite3 操作函数，用来读取和写入各个文件的构建时间

;; 我们可以读取文件的访问时间（atime）和修改时间（ctime），但无法获取
;; 文件的构建时间。这里我不需要 makefile 那样复杂的依赖关系描述，就学
;; 着 org 的构建系统写一个保存构建时间的数据库
;; API 请参考 Emacs 文档：
(defvar yynt-b-sqlite-file
  (yynt-fjoin yynt-basedir "build.sqlite3")
  "用于记录各文件构建时间的 sqlite3 数据库文件")
(defvar yynt-b-sqlite-obj nil
  "用于保存构建时间的 sqlite3 数据库对象")
(defun yynt-b-open-sqlite ()
  "打开 build 数据库，如果它不存在则创建一个
返回得到的 sqlite3 对象"
  (unless (sqlitep yynt-b-sqlite-obj)
    (setq yynt-b-sqlite-obj
	  (sqlite-open yynt-b-sqlite-file)))
  yynt-b-sqlite-obj)
(defun yynt-b-close-sqlite ()
  "关闭数据库对象，并设置 `yynt-b-sqlite-obj' 为 nil
如果确实关闭了数据库则返回 t，否则 nil"
  (when (sqlitep yynt-b-sqlite-obj)
    (prog1 t
      (sqlite-close yynt-b-sqlite-obj)
      (setq yynt-b-sqlite-obj nil))))
(defun yynt-b-sqlite-initialized-p ()
  "判断数据库是否完成了初始化。 `yynt-b-sqlite-obj' 必须是正确的 sqlite3 对象
判断依据是数据库中是否存在 name 为 YYNT 的空表"
  (cl-assert (sqlitep yynt-b-sqlite-obj))
  (= 1 (caar (sqlite-select yynt-b-sqlite-obj "
SELECT count(*) FROM sqlite_master WHERE type='table' AND name='YYNT'"))))

(defun yynt-b-sqlite-init ()
  "对构建时间数据库进行初始化"
  (cl-assert (sqlitep yynt-b-sqlite-obj))
  (sqlite-execute yynt-b-sqlite-obj "
CREATE TABLE YYNT(YY TEXT NOT NULL);")
  (sqlite-execute yynt-b-sqlite-obj "
CREATE TABLE BUILD(
  YYPATH TEXT PRIMARY KEY NOT NULL,
  BUTIME TIMESTAMP DEFAULT CURRENT_TIMESTAMP);")
  (sqlite-execute yynt-b-sqlite-obj "
CREATE TRIGGER UpdateTime
AFTER UPDATE ON BUILD
FOR EACH ROW
BEGIN
  UPDATE BUILD
  SET BUTIME = CURRENT_TIMESTAMP
  WHERE rowid = NEW.rowid;
END;"))

;; 以下代码能在首次执行 yynt2.el 时打开并尝试首次初始化数据库
;; 这是幂等操作，不会因 yynt2.el 重复执行而打开多个数据库
(unless (progn (yynt-b-open-sqlite)
	       (yynt-b-sqlite-initialized-p))
  (yynt-b-sqlite-init))

(defun yynt-b-sqlite-insert (str)
  "向 sqlite 数据库中添加主键"
  (cl-assert (stringp str))
  (= (sqlite-execute yynt-b-sqlite-obj (format "
INSERT INTO BUILD (YYPATH)
VALUES(\"%s\")" str)) 1))
(defun yynt-b-sqlite-delete (str)
  "从 sqlite 数据库中删除一条数据"
  (cl-assert (stringp str))
  (= (sqlite-execute yynt-b-sqlite-obj (format "
DELETE FROM BUILD
WHERE YYPATH = \"%s\"" str)) 1))
(defun yynt-b-sqlite-update (str)
  "更新 sqlite 数据库中的某一条数据的时间"
  (cl-assert (stringp str))
  (= (sqlite-execute yynt-b-sqlite-obj (format "
UPDATE BUILD
SET YYPATH = YYPATH
WHERE YYPATH = \"%s\"" str)) 1))
(defun yynt-b-sqlite-query (str)
  "从 sqlite 数据库查询某一条数据的时间，返回 UTC+0 时间字符串
若查询不到，则返回 nil"
  (cl-assert (stringp str))
  (caar (sqlite-select yynt-b-sqlite-obj (format "
SELECT BUTIME FROM BUILD
WHERE YYPATH = \"%s\"" str))))
(defun yynt-b-sqlite-getall ()
  "获取 sqlite 数据库中所有的数据
格式为 ((path time) ...)"
  (sqlite-select yynt-b-sqlite-obj "
SELECT * FROM BUILD"))

(defun yynt-get-file-ctime (filepath)
  "获取文件的修改时间，UTC+0 格式"
  (format-time-string
   "%Y-%m-%d %T"
   (nth 5 (file-attributes filepath)) t))

(defun yynt-time-less-p (st1 st2)
  "比较第一参数的时间是否小于第二参数
此处的时间格式为 \"YYYY-MM-DD hh:mm:ss"
  (time-less-p (date-to-time st1)
	       (date-to-time st2)))

(defun yynt-gen-file-gen (fn)
  "使用 fn 构建某个文件，文件名应为绝对路径
fn 应至少接受一个文件路径（不限于文件），并返回表明是否成功生成的布尔值
该函数会对文件进行检查，仅生成位于 yynt 目录下的文件"
  (lambda (file &rest args)
    (if (and (file-exists-p file) ; 文件是否存在
	     (file-in-directory-p file yynt-basedir) ; 判断是否位于项目内
	     (apply fn file args)) ; fn 在成功时应返回 t
	(prog1 t (yynt-gen-log (format "generate: %s" file)))
      (prog1 nil (yynt-gen-log (format "skip    : %s" file))))))

(defun yynt-gen-cache-gen (fn)
  "`yynt-gen-file-gen' 的升级版，可以检测生成的文件是否过时
使用它生成的函数可以接受一个额外的 `force' 参数来表明强制生成"
  (yynt-gen-file-gen
   (lambda (file &optional force)
     (let ((filename (yynt-frela file yynt-basedir)))
       (if-let ((dbtime (yynt-b-sqlite-query filename)) ; 获取数据库时间
		(ctime (yynt-get-file-ctime file))) ; 获取文件 ctime
	   ;; 若能在数据库中找到时间
	   (when (or force (yynt-time-less-p dbtime ctime))
	     (when (funcall fn file)
	       (prog1 t (yynt-b-sqlite-update filename))))
	 ;; 数据库中无记录
	 (when (funcall fn file)
	   (prog1 t (yynt-b-sqlite-insert filename))))))))

(defun yynt-gen-fn-org (file)
    "使用 ox-yyhtml 后端构建某个 org 文件，并原地生成 html"
  (when (string= (file-name-extension file) "org")
    (if-let ((buf (get-file-buffer file))) ; 判断是否打开了文件对应的 buffer
	(with-current-buffer buf
	  (prog1 t
	    (org-export-to-file
		'yyhtml (format "%s.html" (file-name-base file)))))
      (with-current-buffer (find-file-noselect file)
	(unwind-protect
	    (prog1 t
	      (org-export-to-file
		  'yyhtml (format "%s.html" (file-name-base file))))
	  (kill-buffer))))))

(defalias 'yynt-gen-org-file (yynt-gen-file-gen 'yynt-gen-fn-org)
  "org 文件生成函数")
(defalias 'yynt-gen-org-file-with-cache (yynt-gen-cache-gen 'yynt-gen-fn-org)
  "带有缓存功能的 org 文件生成函数，对 `yynt-gen-fn-org' 的提升
若 org 文件的修改时间早于构建时间，则不生成新的 html 文件")

;;; 下面的类是对项目中的子项目的建模
;; Requests:
;; 1) 希望能根据文件的路径（或目录的路径）确定它所属的子项目
;; 2) 能描述子项目和子项目之间的构建依赖关系，甚至是文件与文件的关系
;; 3) 能完成单个文件的构建，也能够描述子项目之内的文件依赖关系

;; 1
;; 由于子项目有两种目录结构，分别是一层目录和二层目录
;; 我们需要不断地根据当前文件/目录的父目录查找来进行子项目归属确定
;; 而且需要对二层子项目的文章目录内的目录进行子项目判定
;; 为了简单起见，我不允许在二层子项目的文章目录内创建子项目
;; （但也不是不能做，草，但是有些麻烦而且可能不是有效需求）
;; 我们可以提供子项目的根目录，并将其存储在哈希表中方便快速判定

;; 2
;; 实际上由于某个子项目的局部改变引起另一项目的整个重新构建一般不太可
;; 能，不过还是可以加上这个功能，更重要的是局部改变引起另一局部改变
;; 因此，我不准备描述子项目与子项目之间的关系，它们基本上是独立的
;; 主页面算是个例外

;; 3
;; 这一点主要是针对某个子项目中的汇总性文件，它会从其他文件中搜集一些
;; 信息来生成文件内容，当其他文件在构建时最好也对它进行构建
(defclass yynt-build ()
  ((hash :allocation :class
	 :initform (make-hash-table :test 'equal)
	 :documentation "存储 root 与对象键值对的哈希表，方便根据路径反查子项目对象")
   (root :initarg :root
	 :documentation "子项目的根目录，相对于 `yynt-basedir'，不含 `/' 起始符")
   (type :initarg :type
	 :initform -1
	 :documentation "子项目的类型，0 表示单文件，1表示 1 层目录，2 表示 2 层目录")
   (gather-fn :initarg :<<fn
	      :initform #'ignore
	      :documentation "获取子项目中的所有基础文件的函数，无参函数")
   (gen-fn :initarg :>>fn
	   :initform #'ignore
	   :documentation "用于基础文件生成到 HTML 的函数，需要能接受 force 函数")
   (spef-ls :initarg :sls
	    :initform nil
	    :documentation "指定合成文件的列表，它们包含一些在生成时计算的内容")
   (spegen-fn :initarg :sfn
	      :initform #'ignore
	      :documentation "用于合成文件到 HTML 的生成函数，单参函数")
   (cont-ls :initarg :cont
	    :initform nil
	    :documentation "在基础文件生成后需要进行生成的项目外文件，尽量是合成文件
路径以 `yynt-basedir' 为基础目录")
   (config-ls :initarg :conf
	      :initform nil
	      :documentation "作用于项目中文件生成的一些配置文件
我们需要根据这些文件的 ctime 决定项目内的文件是否需要重新生成"))
  :documentation "用于描述 YYNT 在构建某个子项目时的类")

(cl-defmethod initialize-instance :after ((obj yynt-build) &rest args)
  "用于 yynt-build 的额外实例化操作，将当前对象添加到 hash 静态成员中"
  (puthash (oref obj root) obj (oref obj hash)))

(defun yynt-build-name2obj (str)
  "由项目的字符串得到项目对象，返回 nil 则未找到"
  (gethash str (oref-default yynt-build hash)))

(defun yynt-build-clr-hash ()
  "清空 yynt-build 的 hash 成员的哈希表，主要用于调试"
  (clrhash (oref-default yynt-build hash)))

(defun yynt-build-path2obj (path)
  "由文件的路径得到它所对应的子项目对象"
  (when (and (file-exists-p path) ; 文件是否存在
	     (file-in-directory-p path yynt-basedir)) ; 判断是否位于项目内
    (let ((hash (oref-default yynt-build hash))
	  (base (yynt-frela path yynt-basedir))
	  obj)
      (while (and (not (string= "." base))
		  (not (setq obj (gethash base hash))))
	(print base)
	(setq base (yynt-fdir base)))
      obj)))

(defun yynt-build-get-config-ctime (bobj)
  "获取子项目对象中的依赖配置文件的最新更新时间"
  (when-let ((fs (oref bobj config-ls)))
    (let ((ctimes (mapcar
		   (lambda (x)
		     (yynt-get-file-ctime
		      (yynt-fjoin yynt-basedir x)))
		   fs)))
      (car (reverse (sort ctimes 'yynt-time-less-p))))))

(defun yynt-build-gen-basefiles (bobj &optional force filelist)
  "构建某一子项目对象中的基础文件，如没有提供 `filelist' 则默认取全部基础文件
`filelist' 是一个文件列表，要求 `filelist' 必须全部位于 bobj 内且全为基础文件
可设置 `force' 为 `t' 进行强制构建"
  (when-let ((filelist (or filelist (funcall (oref bobj gather-fn)))))
    (let* ((fn (oref bobj gen-fn)))
      (if force ; 强制重新生成
	  (mapc (lambda (x)
		  (funcall fn x t))
		filelist)
	(if-let ((cfg-ctime (yynt-build-get-config-ctime bobj)))
	    (mapc (lambda (f)
		    (let* ((f-btime (yynt-b-sqlite-query
				     (yynt-frela f yynt-basedir)))
			   (force2 (or (not f-btime)
				       (yynt-time-less-p f-btime cfg-ctime))))
		      (funcall fn f force2)))
		  filelist)
	  (mapc fn filelist))))))

(defun yynt-build-get-sumfiles (bobj)
  "获取某个子项目对象中的所有合成文件的绝对路径组成的表"
  (let ((ls (oref bobj spef-ls))
	(root (oref bobj root)))
    (if (= 0 (oref bobj type)) ; 0 型子项目，单文件项目
	(if (null ls) nil
	  (list (yynt-fjoin yynt-basedir root)))
      (mapcar (lambda (x) (yynt-fjoin yynt-basedir root x))
	      ls))))

(defun yynt-build-gen-sumfiles (bobj &optional filelist use-ext)
  "构建某一子项目对象中的合成文件，如没有提供 `filelist' 则默认取全部合成文件
`filelist' 中的文件必须全部位于 bobj 内且全为合成文件"
  (when-let ((filelist (if use-ext filelist
			 (yynt-build-get-sumfiles bobj))))
    (let* ((fn (oref bobj spegen-fn)))
      (mapc fn filelist))))

(defun yynt-build-get-contfiles (bobjls)
  "获取各子项目对象中 contfs 构成的并集，并返回对应的子项目对象
返回值格式为 ((f1 f2 ...) . (o1 o2 ...))"
  (let* ((files (mapcar (lambda (x) (oref x cont-ls))
			bobjls))
	 (common-fs (cl-reduce (lambda (s a)
				 (cl-union s a :test 'string=))
			       files
			       :initial-value nil))
	 (realfs (mapcar (lambda (x) (yynt-fjoin yynt-basedir x))
			 common-fs))
	 (realobjs (mapcar 'yynt-build-path2obj realfs)))
    (when realfs
      (cons realfs realobjs))))

(defun yynt-build-gen-contfiles (bobjls &optional fo-cons)
  "构建子项目表中的 cont-ls 中的所有文件
使用 `yynt-build-get-contfiles' 求并集，避免重复构建
当然我们也可以提供 `fo-cons' 给它，格式为 ((fullpath ...) . (obj ...))"
  (when-let ((fo-cons (or fo-cons (yynt-build-get-contfiles bobjls))))
    (let ((files (car fo-cons))
	  (objs (cdr fo-cons)))
      (cl-mapc (lambda (file obj)
		 (funcall (oref obj spegen-fn) file))
	       files objs))))

(defun yynt-build-get-files (bobj)
  "获取子项目中所有可能需要生成的文件"
  (append (funcall (oref bobj gather-fn))
	  (yynt-build-get-sumfiles bobj)
	  (car (yynt-build-get-contfiles (list bobj)))))

(defun yynt-build-gen-project (bobj &optional force)
  "构建某个子项目中的全部文件
若 force 为非空值则强制重新构建"
  (yynt-gen-log (format "--- project [%s] generate start ---"
			(oref bobj root)))
  (yynt-build-gen-basefiles bobj force)
  (yynt-build-gen-sumfiles bobj)
  (yynt-build-gen-contfiles (list bobj))
  (yynt-gen-log (format "--- project [%s] generate end ---"
			(oref bobj root))))

(defun yynt-build-gen-file (bobj path &optional force)
  "根据文件路径进行文件生成，可选提供 `bobj' 来消去查找时间
若 `force' 为真，则不考虑缓存
能够处理各项依赖关系，比如项目内依赖和项目外依赖，以及配置文件时间依赖"
  (if (member path (yynt-build-get-sumfiles bobj))
      (funcall (oref bobj spegen-fn) path)
    (and
     (member path (funcall (oref bobj gather-fn)))
     (if-let ((ctime (yynt-build-get-config-ctime bobj)))
	 (let* ((f-btime (yynt-b-sqlite-query
			  (yynt-frela path yynt-basedir)))
		(force2 (or (not f-btime)
			    (yynt-time-less-p f-btime ctime))))
	   (funcall (oref bobj gen-fn) path (or force force2)))
       (funcall (oref bobj gen-fn) path force))
     (prog1 t (yynt-build-gen-sumfiles bobj))
     (prog1 t (yynt-build-gen-contfiles (list bobj))))))

(defun yynt-build-gen-projs (bobjls &optional force)
  "构建列表中所有的子项目
能够避免合成文件的重复构建"
  ;; 构建所有的基础文件
  (mapc (lambda (x)
	  (yynt-build-gen-basefiles x force))
	bobjls)
  ;; 获取由所有子项目的 cont-ls 组成的并集表及其对应项目
  (let* ((all-cont-cons (yynt-build-get-contfiles bobjls))
	 (all-cont-files (car all-cont-cons)))
    (mapc (lambda (o)
	    (let* ((spels (yynt-build-get-sumfiles o))
		   (uniq  (cl-set-difference
			   spels all-cont-files :test 'string=)))
	      (yynt-build-gen-sumfiles o uniq t)))
	  bobjls)
    (yynt-build-gen-contfiles bobjls all-cont-cons)))

(defun yynt-build-gen-all (&optional force)
  "构建所有子项目，等同于构建整个博客"
  (let ((bobjls (hash-table-values (oref-default yynt-build hash))))
    (yynt-build-gen-projs bobjls force)))

(defun yynt-build-file (filename &optional force)
  "用户命令，对当前 buffer 进行构建，含依赖关系
如果带有 C-u 前缀，则强制构建"
  (interactive (list (buffer-file-name) current-prefix-arg))
  (let ((obj (yynt-build-path2obj filename)))
    (if (not obj)
	(message "file not includes in any project")
      (let ((start-time (float-time)))
	(yynt-gen-log "---build single file---")
	(if (yynt-build-gen-file obj filename force)
	    (message (format "gen file in [%s] fin in %ss"
			     (oref obj root)
			     (- (float-time) start-time)))
	  (message "not generated, maybe cached"))
	(yynt-gen-log "---build single file fin---")))))

(defun yynt-build-proj (a &optional force)
  "用户命令，对选中的项目进行构建
若输入 `t'，则对整个项目进行构建，若带有 C-u 前缀则强制构建"
  (interactive (list (completing-read
		      "Select a project:> "
		      (cons "t" (hash-table-keys
				 (oref-default yynt-build hash))))
		     current-prefix-arg))
  (let ((start-time (float-time)))
    (cond
     ((string= a "t")
      (yynt-build-gen-all force)
      (message "generate proj [%s] in %ss"
	       a (- (float-time) start-time)))
     ((member a (hash-table-keys
		 (oref-default yynt-build hash)))
      (let ((obj (gethash a (oref-default yynt-build hash))))
	(yynt-build-gen-project obj force)
	(message "generate proj [%s] in %ss"
		 a (- (float-time) start-time))))
     (t (message "seems not a project...")))))


;; 生成与发布

(defvar yynt-publish-prefix "❄<|➈|>❄"
  "用于在数据库中保存发布文件时间戳的前缀，与一般文件进行区分
主要是懒得再专门创建一个表来保存发布时间戳了")

(defvar yynt-publish-dir (yynt-get-fullpath "blog-build")
  "博客构建结果的根目录，绝对路径。可能得位于 `yynt-basedir' 才行。")

(defvar yynt-publish-global-resources
  '("css" "img" "js")
  "全局资源，使用相对于 `yynt-basedir' 的路径描述，无需起始 '/'")

(defvar yynt-publish-common-disallow-regexp
  (regexp-opt '(".ppt" ".pptx" ".org" ".htm" ".html"))
  "在导出时无需导出的文件的文件后缀的正则")

(defclass yynt-publish ()
  ((hash :allocation :class
	 :initform (make-hash-table)
	 :documentation "构建 `yynt-build' 到 `yynt-publish' 的映射哈希表")
   (build :initarg :build
	  :initform nil
	  :documentation "yynt-publish 对象中的 `yynt-build' 对象")
   (resource :initarg :r-ls
	     :initform nil
	     :documentation "当前 publish 项目下的共有资源列表")
   (gather-fn :initarg :<<fn
		   :initform 'ignore
		   :documentation "获取所有可能经过生成的产物的路径组成的表")
   (bres-fn :initarg :b-fn
	       :initform 'ignore
	       :documentation "用于获取某些基础文件对应的目录或文件资源
主要是用于 2 型 bobj 的一层目录的获取，方便发布其中的资源")
   (exreg :initarg :exreg
	  :initform "^$"
	  :documentation "在导出时需要忽略的文件的后缀组成的正则"))
  :documentation "用于描述记录发布相关信息的对象")

(cl-defmethod initialize-instance :after ((obj yynt-publish) &rest args)
  "将当前对象添加到 hash 静态成员中"
  (puthash (oref obj build) obj (oref obj hash)))

(defun yynt-publish-name2obj (str)
  "由项目的字符串得到 `yynt-publish' 对象，返回 nil 则未找到"
  (if-let* ((bobj (gethash str (oref-default yynt-build hash)))
	    (pobj (gethash bobj (oref-default yynt-publish hash))))
      pobj))

(defun yynt-publish-path2obj (str)
  "由文件路径获取对应的 `yynt-publish' 对象"
  (if-let ((bobj (yynt-build-path2obj str)))
      (gethash bobj (oref-default yynt-publish hash))))

;; 部分参考了 `org-publish-attachment'
(defun yynt-publish-attachment (filename)
  "将某个文件移动到 `yynt-publish-dir' 下
文件名需要是绝对路径，且位于 `yynt-basedir' 内"
  (when (and (file-exists-p filename)
	     (file-in-directory-p filename yynt-basedir))
    (let* ((rela-name (file-relative-name filename yynt-basedir))
	   (rela-path (file-name-directory rela-name))
	   (new-path (file-name-concat yynt-publish-dir rela-path))
	   (new-name (file-name-concat yynt-publish-dir rela-name)))
      (unless (file-directory-p new-path)
	(make-directory new-path t))
      (copy-file filename new-name t)
      t)))

(defun yynt-publish-a-dir (dirname regexp)
  "递归发布目录下的所有文件，但排除正则匹配的扩展名
扩展名需要带点，目录为相对路径"
  (when (file-exists-p (yynt-get-fullpath dirname))
    (mapc 'yynt-publish-attachment
	  (cl-remove-if
	   (lambda (x) (string-match-p regexp x))
	   (directory-files-recursively
	    (yynt-get-fullpath dirname) ".*")))))

;; 加上缓存的移动函数
(defun yynt-publish-attach-cached (filename &optional force)
  "带缓存版本的 `yynt-publish-attachment'"
  (when (and (file-exists-p filename)
	     (file-in-directory-p filename yynt-basedir))
    (let ((key (concat yynt-publish-prefix (yynt-frela filename yynt-basedir))))
      (if-let ((dbtime (yynt-b-sqlite-query key))
	       (ctime (yynt-get-file-ctime filename)))
	  (when (or force (yynt-time-less-p dbtime ctime))
	    (when (yynt-publish-attachment filename)
	      (prog1 t (yynt-b-sqlite-update key))))
	(prog1 (yynt-publish-attachment filename)
	       (yynt-b-sqlite-insert key))))))

(defun yynt-publish-a-dir-cached (dirname regexp &optional force)
  "带缓存版本的 `yynt-publish-a-dir'"
  (when (file-exists-p (yynt-get-fullpath dirname))
    (mapc (lambda (x)
	    (yynt-publish-attach-cached x force))
	  (cl-remove-if
	   (lambda (x) (string-match-p regexp x))
	   (directory-files-recursively
	    (yynt-get-fullpath dirname) ".*")))))

(defun yynt-publish-reslist (rls regexp)
  "发布一个资源列表，列表内可为文件或目录
需要提供列表和目录中的排除正则 `regexp'"
  (mapc (lambda (x)
	  (if (file-directory-p x)
	      (yynt-publish-a-dir x regexp)
	    (yynt-publish-attachment x)))
	rls))

(defun yynt-publish-reslist-cached (rls regexp &optional force)
  "`yynt-publish-reslist' 的带缓存版本"
  (mapc (lambda (x)
	  (if (file-directory-p x)
	      (yynt-publish-a-dir-cached x regexp force)
	    (yynt-publish-attach-cached x force)))
	rls))

(defun yynt-publish-pub-global-resources (&optional force)
  "发布全局资源"
  (let ((dirfile (mapcar (lambda (x) (yynt-fjoin yynt-basedir x))
			 yynt-publish-global-resources)))
    (mapc (lambda (x)
	    (if (file-directory-p x)
		(yynt-publish-a-dir-cached x yynt--match-nothing force)
	      (yynt-publish-attach-cached x force)))
	  dirfile)))

(defun yynt-publish-pub-project-gen (pobj &optional force)
  "生成并发布某个项目涉及到的所有和生成文件相关的文件产物
参数 `pobj' 是 `yynt-publish' 对象"
  (let ((bobj (oref pobj build))
	(files (funcall (oref pobj gather-fn))))
    (yynt-build-gen-project bobj force)
    (mapc (lambda (x)
	      (yynt-publish-attach-cached x force))
	    files)))

(defun yynt-publish-pub-project-res (pobj &optional force)
  "发布某个项目内的所有资源，接受参数是 `yynt-publish' 对象
仅包含该项目内的资源"
  (let* ((bobj (oref pobj build)))
    (let ((ress (append (mapcar 'yynt-get-fullpath (oref pobj resource))
			(funcall (oref pobj bres-fn)))))
      (yynt-publish-reslist-cached
       ress (oref pobj exreg) force))))

(defun yynt-publish-pub-project (pobj &optional force)
  "生成并发布某个项目"
  (yynt-publish-pub-project-gen pobj force)
  (yynt-publish-pub-project-res pobj force))

(defun yynt-publish-pub-all-project (&optional force)
  "生成并发布所有项目"
  (yynt-build-gen-all force) ; 生成
  (mapc (lambda (pobj) ; 发布生成的内容
	  (let* ((fs (funcall (oref pobj gather-fn))))
	    (mapc (lambda (x)
		    (yynt-publish-attach-cached x force))
		  fs)))
	(hash-table-values (oref-default yynt-publish hash)))
  (mapc (lambda (pobj) ; 发布资源文件
	  (yynt-publish-pub-project-res pobj force))
	(hash-table-values (oref-default yynt-publish hash)))
  ; 发布全局资源
  (yynt-publish-pub-global-resources force))

(defun yynt-publish-proj (a &optional force)
  "用户命令，对选中的项目进行发布
若输入 `t'，则对整个博客进行部分，若带有 C-u 前缀则强制发布"
  (interactive (list (completing-read
		      "Select a project:>"
		      (append '("t" "glb")
			      (mapcar (lambda (x)
					(oref x root))
				      (hash-table-keys
				       (oref-default yynt-publish hash)))))
		     current-prefix-arg))
  (let ((start-time (float-time)))
    (cond
     ((string= a "t")
      (yynt-publish-pub-all-project force)
      (message "publish project [%s] in %ss"
	       a (- (float-time) start-time)))
     ((string= a "glb")
      (yynt-publish-pub-global-resources force)
      (message "publish project [%s] in %ss"
	       a (- (float-time) start-time)))
     ((member a (mapcar (lambda (x) (oref x root))
			(hash-table-keys (oref-default yynt-publish hash))))
      (let ((obj (yynt-publish-name2obj a)))
	(yynt-publish-pub-project obj force)
	(message "publish project [%s] in %ss"
		 a (- (float-time) start-time))))
     (t (message "seems not a project...")))))

(defun yynt-publish-pub-file (pobj file &optional force)
  "通用的根据项目内文件得到对应需要导出文件并导出的函数，仅和该文件相关
参数 `file' 必须是 `pobj' 内的文件，且为绝对路径
`pobj' 是 `yynt-publish' 对象"
  (when-let* ((bobj (oref pobj build))
	      (type (oref bobj type))
	      (files (cond
		      ((member file (yynt-build-get-sumfiles bobj)) (list file))
		      ((member file (funcall (oref bobj gather-fn)))
		       (append (list file)
			       (yynt-build-get-sumfiles bobj)
			       (car (yynt-build-get-contfiles (list bobj)))))
		      (t nil)))
	      (outfiles (mapcar (lambda (x)
				  (if (string= "htm" (file-name-extension x)) x
				    (yynt-fswap-ext x "html")))
				files))
	      (res (append (mapcar 'yynt-get-fullpath (oref pobj resource))
			   (and (= type 2)
				(not (string= (yynt-fdir file)
					      (yynt-fjoin yynt-basedir (oref bobj root))))
				(list (yynt-fdir file)))
			   outfiles)))
    (prog1 t (print res) (yynt-publish-reslist-cached res (oref pobj exreg) force))))

(defun yynt-publish-file (file &optional force)
  "用户命令，对当前 buffer 进行发布，并顺带发布相应的资源文件
若带有 C-u 前缀，则强制发布"
  (interactive (list (buffer-file-name) current-prefix-arg))
  (let ((pobj (yynt-publish-path2obj file)))
    (if (not pobj)
	(message "file not belongs to any publish project")
      (let ((start-time (float-time)))
	(yynt-build-gen-file (oref pobj build) file force)
	(if (yynt-publish-pub-file pobj file force)
	    (message (format "publish file in [%s] fin in %ss"
			     (oref (oref pobj build) root)
			     (- (float-time) start-time)))
	  (message "publish not success, maybe not a base/sum file"))))))


;;;; 一些固定的元素，可用作编写新元素的参考
(defun yynt-normal-head (base prefix)
  "生成一般 html 文件的 Head"
  (lambda (_)
    (let ((yynt--rela-current-path base))
      (yynt-html-concat-newline
       (yynt-html-gen-css (yynt-html-r yynt-d-css "style.css" prefix))
       (yynt-html-gen-ico (yynt-html-r yynt-d-img "rin.ico" prefix))
       (yynt-html-google-font-roboto)))))

(defun yynt-normal-code-head (base prefix)
  "生成支持代码复制的 html Head"
  (lambda (_)
    (let ((yynt--rela-current-path base))
      (yynt-html-concat-newline
       (yynt-html-gen-css (yynt-html-r yynt-d-css "style.css" prefix))
       (yynt-html-gen-ico (yynt-html-r yynt-d-img "rin.ico" prefix))
       (yynt-html-gen-js (yynt-html-r yynt-d-js "copycode.js" prefix))
       (yynt-html-google-font-roboto)))))

(defun yynt-normal-postamble (prefix)
  "生成一般的 html postamble"
  (lambda (_)
    (concat "\
<hr>
<div id=\"cc-container\">
<div>
<p>Updated: %C</p>
</div>
<a href=\"https://erkin.party/emacs/\"><img src=\""prefix"/img/made4.gif\" alt=\"⑨\"></a>
</div><br>")))

;;;; 为不同类的文章提供设定不同的模板，宏和项目描述

;;; 用于生成处理资源函数的宏
;; 在生成 post 的 index 文件时，我们首先需要获取所有文章及其标题和 tag。
;; 为此涉及到这些数据的获取与释放（我们需要通过这些数据生成页面的内容，
;; 并在使用完毕后删除而不影响下一次生成）。这个宏对这一过程进行了抽象，
;; 可以用来生成获取与释放的代码。
(defmacro yynt-resource-gen-macro (tmpvar create-fn release-fn create-code)
  "生成资源的暂存变量、初始化函数和释放函数"
  `(progn
     (defvar ,tmpvar nil)
     (defun ,create-fn ()
       (setq ,tmpvar ,create-code)
       "")
     (defun ,release-fn ()
       (setq ,tmpvar nil))))


;;; homepage starts here
(defun yynt-homepage-postamble (_info)
  "用于主页的 postamble"
  "<hr><p style=\"text-align:center;\"><i>homepage ends here~</i></p><br>")

;;; 404
(defun yynt-404-postamble (_info)
  "<hr><div style=\"text-align:center;\"><i>404~</i></div>")

;; 对主页文件 index.org 的生成描述
(make-instance
 'yynt-build
 :root "index.org"
 :type 0
 :sls '("index.org")
 :sfn 'yynt-gen-org-file)
;; 对主页文件的发布描述
(make-instance
 'yynt-publish
 :build (yynt-build-name2obj "index.org")
 :<<fn (lambda () (list (yynt-fjoin yynt-basedir "index.html"))))

;; 对主页文件 404.org 的生成描述
(make-instance
 'yynt-build
 :root "404.org"
 :type 0
 :<<fn (lambda () (list (yynt-fjoin yynt-basedir "404.org")))
 :>>fn 'yynt-gen-org-file-with-cache
 :conf '("cfg.org"))
;; 发布描述
(make-instance
 'yynt-publish
 :build (yynt-build-name2obj "404.org")
 :<<fn (lambda () (list (yynt-fjoin yynt-basedir "404.html"))))

;;

;;; post starts here
;; 二层结果 posts/*/index.org
(defvar yynt-sb-post "posts"
  "子构建 posts 的相对目录名")

(defun yynt-post-postamble (_info)
  "用于 post 文章的 postamble"
  "\
<hr>
<div id=\"cc-container\">
<div>
<p>Created: %d</p>
<p>Updated: %C</p>
<p class=\"creator\">Creator: %c</p>
</div>
<a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">
<img alt=\"CC-BY-SA 4.0\" src=\"../../img/by-sa.svg\"></a>
</div>")

(defun yynt-post-index-postamble (_info)
  "用于 post index 的 postamble"
  "\
<hr>
<div id=\"cc-container\">
<div>
<p>Updated: %C</p>
</div>
<a href=\"https://erkin.party/emacs/\"><img src=\"../img/made4.gif\" alt=\"⑨\"></a>
<img src=\"../img/made10.png\" alt=\"⑨\">
<img src=\"../img/made8.png\" alt=\"⑨\">
<img src=\"../img/sink_white.png\" alt=\"⑨\">
</div>")

(defun yynt-post-head (_info)
  "生成用于 post 中文章的 http <head>"
  (let ((yynt--rela-current-path yynt-sb-post))
    (yynt-html-concat-newline
     (yynt-html-gen-css (yynt-html-r yynt-d-css "style.css" ".."))
     (yynt-html-gen-ico (yynt-html-r yynt-d-img "rin.ico" ".."))
     (yynt-html-gen-js (yynt-html-r yynt-d-js "copycode.js" ".."))
     (yynt-html-gen-js (yynt-html-r yynt-d-js "img_hideshow.js" ".."))
     (yynt-html-google-font-roboto))))

(defalias 'yynt-post-index-head (yynt-normal-head yynt-sb-post nil)
  "生成用于 post index 的 html Head")

(defun yynt-get-all-post-files ()
  "获取 posts 目录下的所有文章源文件绝对路径 (目录名 . 绝对路径)"
  (yynt-get-dated-2xxx-under-dir "posts"))

(defun yynt-get-all-post-titles-tags (dir-fnames)
  "获取 dir-fnames 下所有文章的标题和 tag
格式为 ((目录名 . 绝对路径) . ((\"title\" . 标题) (\"filetag\" . tag)))
注意 alist 顺序并不一定是 title 在前，请使用 assoc 访问"
  (yynt-p-get-info dir-fnames '("title" "filetags")))

(defun yynt-get-post-dir-titles-tags ()
  "获取 posts 下所有文章的标题和 tag"
  (yynt-get-all-post-titles-tags (yynt-get-all-post-files)))

;; 下面的代码用于 org-macro 的展开，变量负责在宏展开期间暂存数据
;; 这是为了能在其他页面中插入根据 Post 信息生成的项
;; 这里采用的方法是每对一个年份都过一遍所有的项
;; 可以考虑先根据年份分组，随后直接插入组对应项
;; （不过我懒得改了，又不是不能用） 2023-07-08
(defvar yynt--post-dir-title-tag nil
  "用于暂存生成 post 的 index 文件的标题和 tag")

(defun yynt-generate-titlelists (dir-titles &optional prefix)
  "生成 dir-titles 的 org 列表"
  (let ((fmt "- [%s] [[file:%s][%s]]"))
    (mapconcat (lambda (x)
		 (let* ((d (caar x))
			(file (yynt-fname (cdar x)))
			(title (cdr (assoc "title" (cdr x)))))
		   (format fmt
			   (substring d 0 10)
			   (yynt-fjoin (or prefix "") d file)
			   title)))
	       dir-titles
	       "\n")))

(defun yynt--post-year-filter (yearstr dir-titles)
  "根据年份筛选文章列表"
  (seq-filter (lambda (x)
		(string= (substring (caar x) 0 4)
			 yearstr))
	      dir-titles))

(defun yynt-post-init ()
  "入口点函数，初始化 Post 目录下的 yynt--post-dir-title 变量"
  (setq yynt--post-dir-title-tag (reverse (yynt-get-post-dir-titles-tags)))
  "")
(defun yynt-post-clr ()
  "清空 yynt--post-dir-title-tag"
  (setq yynt--post-dir-title-tag nil)
  "")
(defun yynt-post-total ()
  "获取 posts 目录下文章的数量"
  (number-to-string (length yynt--post-dir-title-tag)))

(defun yynt-post-year-titlelists (year &optional prefix)
  "生成某一年的所有文章列表"
  (yynt-generate-titlelists
   (yynt--post-year-filter
    year yynt--post-dir-title-tag)
   prefix))

(defun yynt-post-year-num (year)
  "生成某一年的文章数量的字符串"
  (format "=%s="
	  (length (yynt--post-year-filter
		   year yynt--post-dir-title-tag))))

(defun yynt-post-homepage-titlelists (numstr)
  "选取最近的 num 篇文章生成位于 homepage 的列表
num 需要是字符串，毕竟是作为 org 宏使用的"
  (let ((num (string-to-number numstr)))
    (yynt-generate-titlelists
     (seq-take yynt--post-dir-title-tag num)
     "posts")))

;;; posts tags start here
;;; 为 post 文件生成 tagfile
;;; 可以考虑给 repost 也做一个，不过文章现在还太少，没必要
(defvar yynt--post-tags-file (yynt-get-fullpath "post-tags.el")
  "tag 文件路径")

(defun yynt--post-read-tags ()
  "从文件获取所有 tag"
  (read (with-temp-buffer
	  (insert-file-contents
	   yynt--post-tags-file)
	  (buffer-string))))

(defun yynt--post-write-tags (taglist)
  "向文件写入 tags"
  (with-temp-message ""
    (with-temp-buffer
      (insert (pp-to-string taglist))
      (write-file yynt--post-tags-file)))
  (message "yynt: write tags fin"))

(defun yynt-post-get-tags ()
  "获取所有文章中的 tag 集合
可用于调试目的，寻找被遗漏的 tag
如果结果中出现了 nil，则说明存在文件没有 tag"
  (delete-dups
   (mapcar (lambda (x)
	     (cdr (assoc "filetags" (cdr x))))
	   (yynt-get-post-dir-titles-tags))))

;; tag 管理用户函数
;; 增加，删除，和在文章中插入 tag
(defun yynt-post-insert-tag ()
  "当前位置插入 tag"
  (interactive)
  (let* ((tags (yynt--post-read-tags))
	 (selected (completing-read "Select a tag: " tags)))
    (insert selected)))

(defun yynt-post-add-tags (newtag)
  "添加新的 tag"
  (interactive (list (downcase (read-from-minibuffer "Enter a new tag: "))))
  (let ((tags (yynt--post-read-tags)))
    (if (and (string-match-p "[[:alnum:]]+" newtag)
	     (cl-every (lambda (x) (not (string= x newtag))) tags))
	(yynt--post-write-tags (cons newtag tags))
      (message "newtag exists or contains non-alnum char"))))

(defun yynt-post-delete-tags (tag)
  "删除某个 tag"
  (interactive (list (completing-read "Select a tag: " (yynt--post-read-tags))))
  (yynt--post-write-tags (remove tag (yynt--post-read-tags))))

;; 用于 org-macro 的模板宏的函数们
;; 同样这里可以优化，比如以 tag 为键使用哈希表，应该能快不少
;; 但是规模太小反倒现在没有必要（笑）
(defun yynt--post-tag-filter (tagstr dir-titles)
  "根据 tag 筛选文章列表"
  (seq-filter (lambda (x)
		(string= (cdr (assoc "filetags" (cdr x)))
			 tagstr))
	      dir-titles))

(defun yynt-post-tag-titlelists (tag &optional prefix)
  "生成某一 tag 的所有文章列表"
  (yynt-generate-titlelists
   (yynt--post-tag-filter
    tag yynt--post-dir-title-tag)
   prefix))

(defun yynt-post-tag-num (tag)
  "生成某一 tag 的文章数量"
  (format "=%s="
	  (length (yynt--post-tag-filter
		   tag yynt--post-dir-title-tag))))

(defun yynt-post-tag-total ()
  "获取 post 的 tag 总数"
  (number-to-string (length (yynt--post-read-tags))))

;; post 的构建项目描述
(make-instance
 'yynt-build
 :root "posts"
 :type 2
 :<<fn (lambda () (mapcar 'cdr (yynt-get-all-post-files)))
 :>>fn 'yynt-gen-org-file-with-cache
 :sls '("index.org" "tags.org")
 :sfn 'yynt-gen-org-file
 :conf '("cfg.org" "posts/setup.org")
 :cont '("index.org"))
;; post 的发布描述对象
(make-instance
 'yynt-publish
 :build (yynt-build-name2obj "posts")
 :<<fn (lambda () (mapcar (lambda (x)
			   (if (not (string= "org" (file-name-extension x))) x
			     (yynt-fswap-ext x "html")))
			  (yynt-build-get-files (yynt-build-name2obj "posts"))))
 :b-fn (lambda () (mapcar (lambda (x)
			    (yynt-fjoin yynt-basedir "posts" (car x)))
			  (yynt-get-all-post-files)))
 :exreg yynt-publish-common-disallow-regexp)


;;; draft
;; 二层结构；可视作 post 的补充
;; 可以用来保存一些暂时没有完成的文章，或者是长期的本地笔记
;; 在 tag 中，0 表示普通草稿，1 表示长期草稿，2 表示废材
;; 可以直接利用 post 的 head 和 postamble 等模板

(defun yynt-draft-get-title-tags ()
  "获取 draft 目录下的所有标题和标签
此处的标签与 post 区分，使用 #+TMP: "
  (yynt-p-get-info
   (yynt-get-dated-2xxx-under-dir "drafts")
   '("title" "tmp")))

(yynt-resource-gen-macro
 yynt--draft-tmp
 yynt-draft-init
 yynt-draft-clr
 (reverse (yynt-draft-get-title-tags)))

(defun yynt--draft-tag-filter (tagstr dir-titles)
  "根据 #+TMP 筛选文章"
  (seq-filter (lambda (x)
		(string= (cdr (assoc "tmp" (cdr x)))
			 tagstr))
	      dir-titles))

(defun yynt-draft-tag-titlelists (tag &optional prefix)
  "生成某一 tag 的草稿列表"
  (yynt-generate-titlelists
   (yynt--draft-tag-filter
    tag yynt--draft-tmp)
   prefix))

(defun yynt-draft-tag-titlelists (tag &optional prefix)
  "生成某一 tag 的所有文章列表"
  (yynt-generate-titlelists
   (yynt--draft-tag-filter
    tag yynt--draft-tmp)
   prefix))

;; draft 的项目描述对象
(make-instance
 'yynt-build
 :root "drafts"
 :type 2
 :<<fn (lambda () (mapcar 'cdr
			  (yynt-p2-getdirs-ex
			   (yynt-fjoin yynt-basedir "drafts")
			   "^2"
			   "^index\\.org")))
 :>>fn 'yynt-gen-org-file-with-cache
 :sls '("index.org")
 :sfn 'yynt-gen-org-file
 :conf '("cfg.org" "drafts/setup.org"))

;;; repost
;; 二层结构，republish/*/index.org
(defvar yynt-sb-repost "republish"
  "repost 的目录")

(defun yynt-repost-home/up-func (_info)
  "用于 repost 的 home/up 元素生成函数"
  "<div id=\"org-div-home-and-up\">
<a href=\"../index.html\"> HOME </a>
</div>\n")

(defalias 'yynt-repost-index-head (yynt-normal-head yynt-sb-repost nil)
  "生成用于 repost index 的 html head")
(defalias 'yynt-repost-head (yynt-normal-code-head yynt-sb-repost "..")
  "生成用于 repost 中文章的 html head")
(defalias 'yynt-repost-index-postamble (yynt-normal-postamble "..")
  "repost index 页面的 postamble")

(defun yynt-repost-postamble (_info)
  "repost 文章的 postamble"
  "<hr>
<div>
<p>Author: %a</p>
<p>Created: %d</p>
<p>Updated: %C</p>
<p class=\"creator\">Creator: %c</p>
</div>")

(defun yynt-get-all-repost-files ()
  "获取 republish 目录下的所有文章源文件绝对路径 (目录名 . 绝对路径)"
  (yynt-get-dated-2xxx-under-dir "republish"))

(defun yynt-get-repost-dir-titles-tags ()
  "获取 republish 下所有文章的标题和 tag"
  (yynt-get-all-post-titles-tags (yynt-get-all-repost-files)))

(defvar yynt--repost-dir-title-tag nil
  "类似 yynt--post-dir-title-tags")

(defun yynt-repost-init ()
  "入口点函数，初始化 republish 目录下的 yynt--repost-dir-title-tags 变量"
  (setq yynt--repost-dir-title-tag (reverse (yynt-get-repost-dir-titles-tags)))
  "")
(defun yynt-repost-clr ()
  "清空 yynt--repost-dir-title-tag"
  (setq yynt--repost-dir-title-tag nil)
  "")
(defun yynt-repost-total ()
  "republish 中文章数量"
  (number-to-string (length yynt--repost-dir-title-tag)))

(defun yynt-repost-titlelists (&optional prefix)
  "生成所有 republish 文章列表"
  (yynt-generate-titlelists
    yynt--repost-dir-title-tag prefix))
(defun yynt-repost-homepage-titlelists (numstr)
  "选取最近的 num 篇文章生成位于 homepage 的列表
num 需要是字符串，毕竟是作为 org 宏使用的"
  (let ((num (string-to-number numstr)))
    (yynt-generate-titlelists
     (seq-take yynt--repost-dir-title-tag num)
     "republish/")))

;; repost 的构建描述对象
(make-instance
 'yynt-build
 :root "republish"
 :type 2
 :<<fn (lambda () (mapcar 'cdr (yynt-get-all-repost-files)))
 :>>fn 'yynt-gen-org-file-with-cache
 :sls '("index.org")
 :sfn 'yynt-gen-org-file
 :conf '("cfg.org" "republish/setup.org")
 :cont '("index.org"))
;; 发布描述对象
(make-instance
 'yynt-publish
 :build (yynt-build-name2obj "republish")
 :<<fn (lambda () (mapcar (lambda (x)
			    (if (not (string= "org" (file-name-extension x))) x
			      (yynt-fswap-ext x "html")))
			  (yynt-build-get-files (yynt-build-name2obj "republish"))))
 :b-fn (lambda () (mapcar (lambda (x)
			    (yynt-fjoin yynt-basedir "republish" (car x)))
			  (yynt-get-all-repost-files)))
 :exreg yynt-publish-common-disallow-regexp)


;;; projecteuler
;; 一层结构，projecteuler/*.org
(defvar yynt-sb-euler "projecteuler"
  "euler 文章的相对路径")

(defun yynt-euler-postamble (_info)
  "euler 文章的 postamble"
  "\
<hr>
<div id=\"cc-container\">
<div>
<p>Created: %d</p>
<p>Updated: %C</p>
<p class=\"creator\">Creator: %c</p>
</div>
<a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">
<img alt=\"CC-BY-SA 4.0\" src=\"../img/by-sa.svg\"></a>
</div>")

(defalias 'yynt-euler-index-postamble (yynt-normal-postamble "..")
  "用于 euler index 页面的 postamble")
(defalias 'yynt-euler-home/up-func 'yynt-repost-home/up-func
  "用于 euler index 页面的 home/up 生成函数")
(defalias 'yynt-euler-head (yynt-normal-code-head yynt-sb-euler nil)
  "用于 euler 文章和 index 的 head")

;; 生成用于 euler index 文件中的宏的函数和变量
(yynt-resource-gen-macro
 yynt--euler-tmp
 yynt-euler-init
 yynt-euler-clr
 (yynt-p-get-info
  (sort (yynt-p1-getfiles
	 (yynt-fjoin yynt-basedir "projecteuler")
	 "^[0-9]+\\.org")
	(lambda (a b)
	  (let ((a0 (string-to-number (car a)))
		(b0 (string-to-number (car b))))
	    (< a0 b0))))
  '("DATE" "FILETAGS" "DESCRIPTION")))

(defun yynt-euler-table (&optional prefix)
  "生成题目表格，用于 projecteuler 下的index.org
请在初始化（指执行 `yynt-euler-init'）后调用该函数"
  (concat "|Problem|Description|TAG|TIME|\n"
	  "|-+-+-+-|\n"
	  (yynt-p-mapconcat
	   (lambda (name path alist)
	     (format "|[[file:%s][%s]]|%s|%s|[%s]|"
		     (yynt-fjoin (or prefix "")
				 (yynt-fname path))
		     name
		     (or (cdr (assoc "DESCRIPTION" alist)) " ")
		     (or (cdr (assoc "FILETAGS" alist)) " ")
		     (substring (cdr (assoc "DATE" alist)) 1 11)))
	   yynt--euler-tmp
	   "\n")))

(defun yynt-euler-get-files ()
  "获取 projecteuler 的所有基础文件"
  (mapcar 'cdr
	  (yynt-p1-getfiles
	   (yynt-fjoin yynt-basedir "projecteuler")
	   "^[0-9]+\\.org")))

;; projecteuler 的项目描述
(make-instance
 'yynt-build
 :root "projecteuler"
 :type 1
 :<<fn 'yynt-euler-get-files
 :>>fn 'yynt-gen-org-file-with-cache
 :sls '("index.org")
 :sfn 'yynt-gen-org-file
 :conf '("cfg.org" "projecteuler/setup.org"))
;; 生成描述
(make-instance
 'yynt-publish
 :build (yynt-build-name2obj "projecteuler")
 :<<fn (lambda () (mapcar (lambda (x) (yynt-fswap-ext x "html"))
			  (yynt-build-get-files (yynt-build-name2obj "projecteuler"))))
 :r-ls '("projecteuler/res")
 :exreg yynt-publish-common-disallow-regexp)


;; 生成 RSS
;; 也许可以考虑添加多个 channel，不过现在没什么必要
;; 也许可以考虑添加图片，不过没几个人看，算了
;; generate rss file
(defvar yynt-rss-filepath (yynt-get-fullpath "rss.xml")
  "RSS 文件位置")
(defvar yynt-rss-link "https://egh0bww1.com"
  "RSS 网站首页 url")
(defvar yynt-rss-post-link "https://egh0bww1.com/posts/"
  "用于 RSS 的网页 url")
(defvar yynt-rss-post-title "include-yy's blog"
  "RSS 标题")
(defvar yynt-rss-post-description "My recent 10 articles"
  "RSS 内容描述")
(defun yynt-rss-get-current-time ()
  "获取当前时间"
  (format-time-string "%Y-%m-%dT%H:%M:%S"))
(defvar yynt-rss-post-n 10
  "在 RSS 中的文章个数")

(defun yynt-rss-generate-chan-header ()
  "生成 channel 的头部"
    (format "\
<title>%s</title>
<link>%s</link>
<description>%s</description>
<pubDate>%s</pubDate>\n"
	    yynt-rss-post-title
	    yynt-rss-link
	    yynt-rss-post-description
	    (yynt-rss-get-current-time)))

(defun yynt-rss-generate-item (title link desc &optional tag date)
  "生成 link 内容"
  (format "<item>
<title>%s</title>
<link>%s</link>
<description>%s</description>%s%s
</item>"
	  title link desc
	  (if tag (format "\n<category>%s</category>" tag) "")
	  (if date (format "\n<pubDate>%s</pubDate>" date) "")))

;; 使用 yynt-get-post-dir-titles-tags，我们可以获得几乎所有的 post 信息，除了 DESC
(defun yynt-get-post-rss ()
  "格式为 ((dir . fpath) . ((\"title\" . title) (\"filtag\" . tag) (\"description\" . rss)))"
  (let ((dir-fnames (yynt-get-all-post-files)))
    (yynt-p-get-info dir-fnames
		     '("title" "filetags" "description"))))

(defun yynt-rss-generate ()
  "生成完整的 RSS"
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
   "<!-- refs: https://www.runoob.com/rss/rss-syntax.html -->\n"
   "<rss version=\"2.0\">\n"
   "<channel>\n"
   (yynt-rss-generate-chan-header)
   ;; 单个数据格式为 ((dir . fpath) . alist), key:{title, filetags, yyntrss}
   (mapconcat (lambda (x)
		(let ((link (concat yynt-rss-post-link (caar x)))
		      (title (cdr (assoc "title" (cdr x))))
		      (desc (cdr (assoc "description" (cdr x))))
		      (tag (cdr (assoc "filetags" (cdr x))))
		      (date (substring (caar x) 0 10)))
		  (if (or (null link) (null title) (null desc))
		      (error "yynt-rss: %s misses title or description!" (caar x))
		    (yynt-rss-generate-item title link desc tag date))))
	      (seq-take (reverse (yynt-get-post-rss)) yynt-rss-post-n) "\n")
   "\n</channel>\n</rss>"))

(defun yynt-rss-update ()
  "更新 RSS 文件"
  (interactive)
  (with-temp-file yynt-rss-filepath
    (set-buffer-file-coding-system 'utf-8)
    (insert (yynt-rss-generate)))
  (save-current-buffer
    (set-buffer (find-file-noselect yynt-rss-filepath))
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer))
  (message "update rss fin"))

;; 对 rss.xml 的生成描述
(make-instance
 'yynt-build
 :root "rss.xml"
 :type 0
 :sls '("rss.xml")
 :sfn (yynt-gen-file-gen (lambda (_) (yynt-rss-update))))
;; 对 rss.xml 的发布描述对象
(make-instance
 'yynt-publish
 :build (yynt-build-name2obj "rss.xml")
 :<<fn (lambda () (list (yynt-fjoin yynt-basedir "rss.xml"))))


;; 一些写作模板
;; some template for posts, reposts and euler
;; [YYYY-MM-DD DAY HH:MM]
(defun yynt-temp-current-time ()
  (format-time-string "[%Y-%m-%d %a %H:%M]"))
(defun yynt-temp-post (title tag)
  (insert (format
	   "\
#+SETUPFILE: ../setup.org
#+FILETAGS: %s\n
#+TMP: 0（未完成的草稿） 1（长期笔记）2（垃圾）
#+TITLE: %s
#+DATE: %s\n
#+DESCRIPTION:"
	   tag title
	   (yynt-temp-current-time))))

(defun yynt-temp-repost ()
  (insert "\
#+SETUPFILE: ../setup.org
#+FILETAGS:\n
#+TITLE:
#+DATE:
#+AUTHOR:\n
#+BEGIN_aside\n
#+END_aside\n"))

(defun yynt-temp-euler (num)
  (insert (format
	   "\
#+SETUPFILE: ./setup.org\n
#+TITLE: Problem %s
#+DESCRIPTION: 最好小于二十汉字。。汉字汉字汉字汉字汉字
#+FILETAGS: 如有必要可添加标签，如 #prime#
#+DATE:\n
* Problem\n
*[[https://projecteuler.net/problem=%s]]*\n
*中文题目*\n
https://pe-cn.github.io/%s
* Solution"
	   num num num)))

;; 一些方便的新建文件夹功能

;; 直接在对应目录创建文件夹和 org 文件
(defun yynt-create-draft (dirname title tag)
  "在 draft 目录创建新的草稿"
  (interactive (list (read-from-minibuffer "Enter dirname: ")
		     (read-from-minibuffer "Enter title: ")
		     (completing-read "Select tag: " (yynt--post-read-tags))))
  (let ((dirpath (yynt-fjoin (yynt-get-fullpath "drafts")
			     (concat (format-time-string "%Y-%m-%d-")
				     dirname))))
    (make-directory dirpath)
    (find-file (yynt-fjoin dirpath "index.org"))
    (yynt-temp-post title tag)))

(defun yynt-move-draft (dirpath)
  "将当前所在草稿 org 文件所在文件夹发布到 post"
  (interactive (list default-directory))
  (if (not (equal (yynt-get-fullpath "drafts")
		  (yynt-fdir dirpath)))
      (message "currently not in draft source file, quit")
    (let ((newdir (yynt-fjoin
		   (yynt-get-fullpath "posts")
		   (concat (format-time-string "%Y-%m-%d-")
			   (substring (yynt-frela
				       dirpath
				       (yynt-get-fullpath "drafts"))
				      11)))))
      (copy-directory dirpath newdir t t t)
      (find-file (yynt-fjoin newdir "index.org"))
      (set-buffer-file-coding-system 'utf-8)
      (message "publish draft fin"))))

(defun yynt-create-repost (dirname)
  "创建新的 republish 文件夹"
  (interactive (list (read-from-minibuffer "Enter dirname: ")))
  (let ((dirpath (yynt-fjoin
		  (yynt-get-fullpath "republish")
		  (concat (format-time-string "%Y-%m-%d-")
			  dirname))))
    (make-directory dirpath)
    (find-file (yynt-fjoin dirpath "index.org"))
    (set-buffer-file-coding-system 'utf-8)
    (yynt-temp-repost)))

(defun yynt-create-euler (number)
  "创建新的 projecteuler 文件"
  (interactive (list (read-from-minibuffer "Enter problem Number: ")))
  (let ((filepath (yynt-fjoin (yynt-get-fullpath "projecteuler")
			      (concat number ".org"))))
    (find-file filepath)
    (unless (file-exists-p filepath)
      (set-buffer-file-coding-system 'utf-8)
      (yynt-temp-euler number))))


;; yynt2.el ends here

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
          (yynt-frela parent)
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

(defun yynt-p-mapcar (fn list)
  "为形如 `yynt-p-get-info' 返回值的 list 提供方便的 map 函数
fn 的参数列表为 (name path alist)，其中 name 为键，path 为绝对路径
alist 为提取得到的属性值"
  (mapcar (lambda (x)
	    (let* ((pair (car x))
		   (name (nth 0 pair))
		   (path (nth 1 pair))
		   (alist (cdr x)))
	      (funcall fn name path alist)))
	  list))

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

(defalias 'yynt-post-index-head 'yynt-normal-head
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
(defalias 'yynt-repost-head (yynt-normal-code-head yynt-sb-post "..")
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


;;; 区别于 org-publish 的构建功能
;;; 待 emacs 29 发布再实现带缓存的构建，可以考虑用 sqlite3
;;; 不过感觉不需要缓存也行，更何况 org-publish 不太能处理引用文件发生变化的情况
;;; 以下实现采用的方式是先在本地生成，随后复制到目标目录，且保持原目录结构不变
;;; 相比于原版直接生成到目标看上去有点蠢，取 `barbarian' 之 `barbar' 作为后缀
;;; 构建系统应该能够描述依赖关系....... 这部分留到下一个 ^L

;; 直接构建整个 blog

;;; 添加一个构建信息 logger，不使用 message
(defvar yynt--logger "*yynt*"
  "用于输出 log 消息的 buffer 名")
(defun yynt-gen-log (str)
  (with-current-buffer (get-buffer-create yynt--logger)
    (goto-char (point-max))
    (insert str "\n")))
(defun yynt-show-log ()
  (interactive)
  (display-buffer (get-buffer-create yynt--logger)))

;; 以下函数可能仅用于博客出现大规模改动时
(defun yynt-gen-org-barbar (files)
  "对列表中的文件进行 org 构建"
  (dolist (f files)
    (if (not (and (file-exists-p f)
		  (string= (file-name-extension f) "org")))
	(yynt-gen-log (format "skipping  : %s" f))
      (yynt-gen-log (format   "generating: %s" f))
      (if-let ((buf (get-file-buffer f)))
	  (with-current-buffer buf
	    (org-export-to-file 'yyhtml (format "%s.html" (file-name-base f))))
	(with-current-buffer (find-file-noselect f)
	  (org-export-to-file 'yyhtml (format "%s.html" (file-name-base f)))
	  (kill-buffer))))))

(defun yynt-gen-all-posts-barbar ()
  "重新构建 posts 中的所有 org 文件，也包括 index 文件"
  (interactive)
  ;;(org-export-to-file 'yyhtml "index.html")
  (let* ((all-posts (mapcar 'cdr (yynt-get-all-post-files)))
	 (index-and-tags (list (yynt-fjoin (yynt-get-fullpath "posts") "index.org")
			       (yynt-fjoin (yynt-get-fullpath "posts") "tags.org"))))
    (yynt-gen-org-barbar (append all-posts index-and-tags))))

(defun yynt-gen-all-reposts-barbar ()
  "重新构建 reposts 中的 org 文件，也包括 index 目录文件"
  (interactive)
  (let ((all-posts (mapcar 'cdr (yynt-get-all-repost-files)))
	(index (list (yynt-fjoin (yynt-get-fullpath "republish") "index.org"))))
    (yynt-gen-org-barbar (append all-posts index))))

(defun yynt-gen-all-projecteuler-barbar ()
  "重新构建 euler 中的 org 文件"
  (interactive)
  (let ((all-posts (mapcar 'cdr (yynt-p1-getfiles
				 (yynt-get-fullpath yynt-sb-euler) "[0-9]\\.org")))
	(index (list (yynt-fjoin (yynt-get-fullpath "projecteuler") "index.org"))))
    (yynt-gen-org-barbar (append all-posts index))))

(defun yynt-gen-toplevel-barbar ()
  "重新构建位于根目录的 org 文件"
  (interactive)
  (let ((all-files (list (yynt-get-fullpath "index.org")
			 (yynt-get-fullpath "404.org"))))
    (yynt-gen-org-barbar all-files)
  (yynt-rss-update)))

(defun yynt-gen-barbar ()
  "重新构建整个 blog"
  (interactive)
  (yynt-gen-all-posts-barbar)
  (yynt-gen-all-reposts-barbar)
  (yynt-gen-all-projecteuler-barbar)
  (yynt-gen-toplevel-barbar))

;;;; 构建中的依赖关系

;; 以上是不带脑子的构建过程，现在让我们考虑一下依赖关系
;; 比如更新了 posts 中的文章，我们需要更新 post index, post tags, homepage 和 rss
;; 更新了 reposts 的文章，我们需要更新 repost index 和 homepage
;; 更新了 euler 的文章，我们需要更新 euler index
;; 也许我们可以使用类似钩子的机制来在某次更新完成后对其他内容进行更新
;; 同时也要考虑到更新合并的问题，比如同时更新两个 posts 文章，我们只需要
;; 更新一次 index, tags, homepage 和 rss

(defun yynt--update-homepage ()
  "只更新 homepage"
  (yynt-gen-org-barbar (list (yynt-get-fullpath "index.org"))))
(defun yynt--update-post-index-and-tags ()
  "更新 posts 中的 index 和 tag"
  (yynt-gen-org-barbar (list (yynt-get-fullpath "posts/index.org")
			     (yynt-get-fullpath "posts/tags.org"))))
(defun yynt--update-repost-index ()
  "更新 repost 的 index"
  (yynt-gen-org-barbar (list (yynt-get-fullpath "republish/index.org"))))
(defun yynt--update-euler-index ()
  "更新 euler 的 index"
  (yynt-gen-org-barbar (list (yynt-get-fullpath "projecteuler/index.org"))))

(defvar yynt--update-post-hook (list 'yynt--update-post-index-and-tags
				     'yynt--update-homepage
				     'yynt-rss-update)
  "更新一些 posts 后需要的更新操作")
(defvar yynt--update-repost-hook (list 'yynt--update-repost-index
				       'yynt--update-homepage)
  "更新 reposts 后需要的更新操作")
(defvar yynt--update-euler-hook (list 'yynt--update-euler-index)
  "更新单篇 euler 后需要的更新操作")

;; 如果项目多起来了，可以考虑创建一个包含各项目判定函数的列表
;; 然后利用这些函数判定属于那个项目，不过现在还是算了
(defun yynt--update-find-sb (path)
  "根据 path 寻找所属 sb, 比如 post 等等，返回用于后续动作的钩子
`path' 需要是文件路径"
  (if (or (not (string-match-p yynt-basedir path)) ; 不在 yynt 内
	  (yynt-fsame-p (yynt-fdir path) yynt-basedir)) ; 在根目录上
      nil
    (let* ((fnm (yynt-fname path))
	   (dir (yynt-fdir path))
	   (d-1 (yynt-fname dir)))
      (pcase d-1
	((guard (equal yynt-sb-post d-1)) nil)
	((guard (equal yynt-sb-repost d-1)) nil)
	((guard (equal yynt-sb-euler d-1))
	 (if (equal fnm "index.org") nil
	   'yynt--update-euler-hook))
	(_ (let* ((dir-2 (yynt-fdir dir))
		  (d-2 (yynt-fname dir-2)))
	     (pcase d-2
	       ((guard (equal yynt-sb-post d-2)) 'yynt--update-post-hook)
	       ((guard (equal yynt-sb-repost d-2)) 'yynt--update-repost-hook)
	       (_ (error "should't happens for now")))))))))

(defun yynt-update-current-buffer (path)
  "更新当前的 org 文件，并运行对应的钩子"
  (interactive (list (buffer-file-name (current-buffer))))
  (let ((hook (yynt--update-find-sb path)))
    (yynt-gen-org-barbar (list path))
    (run-hooks hook)))

;; 为什么没有只更新一个 buffer 的命令，因为我们可以 C-c C-e y y（笑）

;; 这里采用先原地生成后复制到目标的方法
;; 这样一来，资源的发布就是复制而已
;; 也许我在编写过程中部分参考了 org-publish，比如 `org-publish-attachment'

(defvar yynt-publish-dir (yynt-get-fullpath "blog-build")
  "博客构建结果的根目录")

(defun yynt-publish-attachment (filename)
  "将某个文件移动到 `yynt-publish-dir' 下
文件名需要是绝对路径，且位于 `yynt-basedir' 内"
  (when (string-match-p yynt-basedir filename)
    (let* ((rela-name (file-relative-name filename yynt-basedir))
	   (rela-path (file-name-directory rela-name))
	   (new-path (file-name-concat yynt-publish-dir rela-path))
	   (new-name (file-name-concat yynt-publish-dir rela-name)))
      (unless (file-directory-p new-path)
	(make-directory new-path t))
      (copy-file filename new-name t))))

(defun yynt-publish-a-dir (dirname regexp)
  "递归发布目录下的所有文件，但排除正则匹配的扩展名
扩展名需要带点，目录为相对路径"
  (mapc 'yynt-publish-attachment
	(cl-remove-if
	 (lambda (x) (string-match-p regexp x))
	 (directory-files-recursively
	  (yynt-get-fullpath dirname) ".*"))))

(defun yynt-publish-global-res-barbar ()
  "发布全局资源"
  (interactive)
  (let ((dirs (list yynt-d-css yynt-d-img yynt-d-js)))
    (mapc (lambda (x) (copy-directory (yynt-get-fullpath x)
				      (yynt-fjoin yynt-publish-dir x) t t t))
	  dirs))
  (let ((files '("robots.txt" "rss.xml")))
    (mapc (lambda (x) (yynt-publish-attachment (yynt-get-fullpath x)))
	  files))
  (message "publish res: fin"))

(defun yynt--publish-barbar (dirname regexp genfunc &optional update)
  "用于 posts repost euler 的内部函数"
  (when update
    (funcall genfunc))
  (yynt-publish-a-dir dirname regexp))


(defun yynt-publish-posts-barbar (update)
  "发布所有的 posts，询问是否更新"
  (interactive (list (y-or-n-p "Update?")))
  (yynt--publish-barbar yynt-sb-post "\\.org\\|pptx?"
			'yynt-gen-all-posts-barbar
			update))

(defun yynt-publish-reposts-barbar (update)
  "发布所有的 reposts，询问是否更新"
  (interactive (list (y-or-n-p "Update?")))
  (yynt--publish-barbar yynt-sb-repost "\\.org\\|pptx?"
			'yynt-gen-all-reposts-barbar
			update))

(defun yynt-publish-eulers-barbar (update)
  "发布所有的 euler，询问是否更新"
  (interactive (list (y-or-n-p "Update?")))
  (yynt--publish-barbar yynt-sb-euler "\\.org"
			'yynt-gen-all-projecteuler-barbar
			update))

(defun yynt-publish-homepage-404-barbar (update)
  "询问更新 index.org 和 404.org 并发布"
  (interactive (list (y-or-n-p "Update?")))
  (when update
    (yynt-gen-org-barbar
     (list (yynt-get-fullpath "index.org")
	   (yynt-get-fullpath "404.org"))))
  (mapc 'yynt-publish-attachment
	(list (yynt-get-fullpath "index.html")
	      (yynt-get-fullpath "404.html"))))

(defun yynt-publish-barbar (update)
  "发布所有内容，并询问更新"
  (interactive (list (y-or-n-p "Update?")))
  (when update
    (yynt-rss-update))
  (yynt-publish-posts-barbar update)
  (yynt-publish-reposts-barbar update)
  (yynt-publish-eulers-barbar update)
  (yynt-publish-global-res-barbar)
  (yynt-publish-homepage-404-barbar update))
;; 愚蠢的发布方式就到此为止了
;; 同样，这些代码只适合在博客大规模改动时使用


;;; 处理依赖关系的发布当前内容功能
;;; 毕竟是发布，大多数情况下应该默认重新生成
;;; 若之后使用多种后端，可以考虑扩展，不过暂时没有这个需求~

(defun yynt-publish-single (filename)
  "发布当前单个文件（也可用变量指定）
若为 org 文件则询问是否更新对应 html，随后发布"
  (interactive (list (buffer-file-name (current-buffer))))
  (if (not (string= (file-name-extension filename) "org"))
      (yynt-publish-attachment filename)
    (yynt-gen-org-barbar (list filename))
    (yynt-publish-attachment (yynt-fswap-ext filename "html"))))

;; 这里直接借用 `yynt--update-find-sb' 的项目判断
;; 同样，逻辑全写在一起了，有需要的话再改吧（笑）
(defun yynt-publish-current-buffer (filename)
  "发布当前文件（也可用变量指定）
由于有依赖关系，这个文件需要是 org 或 htm 文件"
  (interactive (list (buffer-file-name (current-buffer))))
  (let ((hname (yynt--update-find-sb filename)))
    (if (null hname) (yynt-publish-single filename)
      (yynt-update-current-buffer filename)
      (pcase hname
	('yynt--update-euler-hook
	 (progn
	   (mapc 'yynt-publish-attachment
		 (list (yynt-fswap-ext filename "html")
		       (yynt-get-fullpath "projecteuler/index.html")))
	   (yynt-publish-a-dir "projecteuler/res" "⑨")))
	('yynt--update-repost-hook
	 (yynt-publish-a-dir (yynt-frela (yynt-fdir filename) yynt-basedir)
			     "\\.org\\|pptx?")
	 (mapc 'yynt-publish-attachment
	       (mapcar 'yynt-get-fullpath
		       '("index.html" "republish/index.html"))))
	('yynt--update-post-hook
	 (yynt-publish-a-dir (yynt-frela (yynt-fdir filename) yynt-basedir)
			     "\\.org\\|pptx?")
	 (mapc 'yynt-publish-attachment
	       (mapcar 'yynt-get-fullpath
		       '("index.html" "rss.xml"
			 "posts/index.html" "posts/tags.html"))))
	(_ (error "yynt-publish: never happends"))))))

;; yynt2.el ends here

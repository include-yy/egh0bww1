;; yynt2.el -- yynt.el 的改进版 -*- lexical-binding: t; -*-

;;;; comment

;; 相比于 yynt.el 使用 advice 修改 ox-html 而言使用了新的后端，不影响 ox-html 默认行为
;; 添加了一些功能：
;; - 路径运算
;; - 生成 rss
;; - 区别于 org-publish 的项目工具
;; - ...

;; 请使用 `eval-buffer' 加载 yynt，且保证 yynt.el 在项目根目录上

;;; requests
;; 加载自带的 htmlize
;; 来自 https://github.com/hniksic/emacs-htmlize
(require 'htmlize (expand-file-name "./htmlize.el"))
;; 加载魔改版 html 后端
(load-file (expand-file-name "./ox-yyhtml.el"))

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
  (insert "#+NAME: headimg\n")
  (yynt-imgattr))
(defun yynt-tailimg ()
  "插入尾部图片所需内容"
  (interactive)
  (insert "#+NAME: tailimg\n")
  (yynt-imgattr))

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

;;; post
;; 博客的发表目录，为二层结构，即 posts/*/index.org
(defvar yynt-sb-post "posts"
  "子构建 posts 的相对目录名")

(defun yynt-post-postamble (_info)
  "用于 post 文章的 postamble"
  "<hr>
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
  "<hr>
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

(defun yynt-post-index-head (_info)
  "生成用于 post index 的 http <head>"
  (let ((yynt--rela-current-path yynt-sb-post))
    (yynt-html-concat-newline
     (yynt-html-gen-css (yynt-html-r yynt-d-css "style.css"))
     (yynt-html-gen-ico (yynt-html-r yynt-d-img "rin.ico"))
     (yynt-html-google-font-roboto))))

;;; homepage
(defun yynt-homepage-postamble (_info)
  "用于主页的 postamble"
  "<hr><p style=\"text-align:center;\"><i>homepage ends here~</i></p><br>")

;;; repost
;; repost 发表目录，二层结构，republish/*/index.org
(defvar yynt-sb-repost "republish"
  "repost 的目录")

(defun yynt-repost-home/up-func (_info)
  "用于 repost 的 home/up 元素生成函数"
  "<div id=\"org-div-home-and-up\">
<a href=\"../index.html\"> HOME </a>
</div>\n")

;; 直接使用 post 的 index head 生成
(defalias 'yynt-repost-index-head 'yynt-post-index-head)

(defun yynt-repost-head (_info)
  "生成用于 repost 中文章的 http <head>"
  (let ((yynt--rela-current-path yynt-sb-repost))
    (yynt-html-concat-newline
     (yynt-html-gen-css (yynt-html-r yynt-d-css "style.css" ".."))
     (yynt-html-gen-ico (yynt-html-r yynt-d-img "rin.ico" ".."))
     (yynt-html-gen-js (yynt-html-r yynt-d-js "copycode.js" ".."))
     (yynt-html-google-font-roboto))))

(defun yynt-repost-index-postamble (_info)
  "repost index 页面的 postamble"
  "<hr>
<div id=\"cc-container\">
<div>
<p>Updated: %C</p>
</div>
<a href=\"https://erkin.party/emacs/\"><img src=\"../img/made4.gif\" alt=\"⑨\"></a>
</div><br>
")

(defun yynt-repost-postamble (_info)
  "repost 文章的 postamble"
  "<hr>
<div>
<p>Author: %a</p>
<p>Created: %d</p>
<p>Updated: %C</p>
<p class=\"creator\">Creator: %c</p>
</div>")

;;; euler
;; euler 发表目录，一层结构，projecteuler/*.org
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

;; 直接使用 repost index 的 postamble
(defalias 'yynt-euler-index-postamble 'yynt-repost-index-postamble)
;; 直接使用 repost index 的 home/up
(defalias 'yynt-euler-home/up-func 'yynt-repost-home/up-func)

(defun yynt-euler-head (_info)
  "用于 euler 文章和 index 的 <head>"
  (let ((yynt--rela-current-path yynt-sb-euler))
    (yynt-html-concat-newline
     (yynt-html-gen-css (yynt-html-r yynt-d-css "style.css"))
     (yynt-html-gen-ico (yynt-html-r yynt-d-img "rin.ico"))
     (yynt-html-gen-js  (yynt-html-r yynt-d-js  "copycode.js"))
     (yynt-html-google-font-roboto))))

;;; 404
(defun yynt-404-postamble (_info)
  "<hr><div style=\"text-align:center;\"><i>404~</i></div>")

;;; posts start here
(defun yynt-get-file-info (filename infos)
  "此处只取文件的 4096 字节数据，毕竟标题和 TAG 写在文件的最前面
感觉 1024 也许就足够了"
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

(defun yynt-get-dated-2xxx-under-dir (dirname)
  "获取某目录下的所有以 2xxx 开头的目录，以及其中的 index.org|html 文件绝对路径"
  (let* ((dirs (directory-files (concat yynt-basedir
					(file-name-as-directory dirname))
				nil "2"))
	 (fnames (mapcar (lambda (x)
			   (let* ((d (concat yynt-basedir
					     (file-name-as-directory dirname) x "/"))
				  (org (concat d "index.org"))
				  (htm (concat d "index.htm")))
			     (cond
			      ((file-exists-p org) org)
			      ((file-exists-p htm) htm)
			      (t (error "yynt: can't find post files")))))
			 dirs)))
    (cl-mapcar (lambda (x y) (cons x y)) dirs fnames)))

(defun yynt-get-all-post-files ()
  "获取 posts 目录下的所有文章源文件绝对路径 (目录名 . 绝对路径)"
  (yynt-get-dated-2xxx-under-dir "posts"))

(defun yynt-get-all-post-titles-tags (dir-fnames)
  "获取 dir-fnames 下所有文章的标题和 tag
格式为 ((目录名 . 绝对路径) . ((\"title\" . 标题) (\"filtag\" . tag)))
注意 alist 顺序并不一定是 title 在前，请使用 assoc 访问"
  (cl-mapcar (lambda (x y) (cons x y))
	     dir-fnames
	     (mapcar (lambda (x)
		       (yynt-get-file-info (cdr x) '("title" "filetags")))
		     dir-fnames)))

(defun yynt-get-post-dir-titles-tags ()
  "获取 posts 下所有文章的标题和 tag"
  (yynt-get-all-post-titles-tags (yynt-get-all-post-files)))

(defvar yynt--post-dir-title-tag nil
  "用于暂存生成 post 的 index 文件的标题和 tag")

(defun yynt-generate-titlelists (dir-titles &optional prefix)
  "生成 dir-titles 的 org 列表"
  (let ((fmt "- [%s] [[file:%s][%s]]"))
    (mapconcat (lambda (x)
		 (let* ((d (caar x))
			(file (file-name-nondirectory (cdar x)))
			(title (cdr (assoc "title" (cdr x)))))
		   (format fmt
			   (substring d 0 10)
			   (concat prefix d "/" file)
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
     "posts/")))

;;; posts tags start here
(defvar yynt--post-tags-file (concat yynt-basedir "post-tags.el")
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
  "获取所有文章中的 tag 集合"
  (delete-dups
   (mapcar (lambda (x)
	     (cdr (assoc "filetags" (cdr x))))
	   (yynt-get-post-dir-titles-tags))))

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

(defun yynt--post-tag-filter (tagstr dir-titles)
  "根据年份筛选文章列表"
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

;;; repost starts here
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
#+YYNTRSS:"
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
  (interactive)
  (insert (format
	   "\
#+SETUPFILE: ./setup.org\n
#+TITLE: Problem %s
#+DATE:\n
* Problem\n
*[[https://projecteuler.net/problem=%s]]*\n
*中文题目*\n
https://pe-cn.github.io/%s
* Solution"
	   num num num)))

;; generate rss file
(defvar yynt-rss-filepath (concat yynt-basedir "rss.xml")
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
  "格式为 ((dir . fpath) . ((\"title\" . title) (\"filtag\" . tag) (\"yyntrss\" . rss)))"
  (let ((dir-fnames (yynt-get-all-post-files)))
    (cl-mapcar (lambda (x y) (cons x y))
	       dir-fnames
	       (mapcar (lambda (x)
			 (yynt-get-file-info (cdr x) '("title" "filetags" "yyntrss")))
		       dir-fnames))))

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
		      (desc (cdr (assoc "yyntrss" (cdr x))))
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

;; 直接在对应目录创建文件夹和 org 文件
(defun yynt-create-draft (dirname title tag)
  "在 draft 目录创建新的草稿"
  (interactive (list (read-from-minibuffer "Enter dirname: ")
		     (read-from-minibuffer "Enter title: ")
		     (completing-read "Select tag: " (yynt--post-read-tags))))
  (let ((dirpath (concat yynt-basedir "drafts/"
			 (format-time-string "%Y-%m-%d-")
			 dirname)))
    (make-directory dirpath)
    (find-file (concat dirpath "/index.org"))
    (yynt-temp-post title tag)))

(defun yynt-publish-draft (dirpath)
  "将当前所在草稿 org 文件所在文件夹发布到 post"
  (interactive (list default-directory))
  (if (not (string-match-p (concat yynt-basedir "drafts")
			   dirpath))
      (message "currently not in draft source file, quit")
    (let ((newdir (concat yynt-basedir "posts/"
			  (format-time-string "%Y-%m-%d-")
			  (substring (file-relative-name
				      dirpath
				      (concat yynt-basedir "drafts"))
				     11))))
      (copy-directory dirpath newdir t t t)
      (find-file (concat newdir "/index.org"))
      (set-buffer-file-coding-system 'utf-8)
      (message "publish draft fin"))))

(defun yynt-create-repost (dirname)
  "创建新的 republish 文件夹"
  (interactive (list (read-from-minibuffer "Enter dirname: ")))
  (let ((dirpath (concat yynt-basedir "republish/"
			 (format-time-string "%Y-%m-%d-")
			 dirname)))
    (make-directory dirpath)
    (find-file (concat dirpath "/index.org"))
    (set-buffer-file-coding-system 'utf-8)
    (yynt-temp-repost)))

(defun yynt-create-euler (number)
  "创建新的 projecteuler 文件"
  (interactive (list (read-from-minibuffer "Enter problem Number: ")))
  (let ((filepath (concat yynt-basedir "projecteuler/"
			  number ".org")))
    (find-file filepath)
    (set-buffer-file-coding-system 'utf-8)
    (unless (file-exists-p filepath)
      (yynt-temp-euler number))))

;;; 基于 org-publish 完成新的构建工具
;;; 待 emacs 29 发布再实现带缓存的构建

(defvar yynt-publish-dir (expand-file-name "./blog-build")
  "博客构建结果的根目录")

(defun yynt-gen-org-barbar (files)
  "对列表中的文件进行 org 构建"
  (dolist (f files)
    (when (and (file-exists-p f)
	       (string= (file-name-extension f) "org"))
      (message "yynt: gen %s" f)
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
	 (index-and-tags (list (file-name-concat yynt-basedir "posts" "index.org")
			       (file-name-concat yynt-basedir "posts" "tags.org"))))
    (yynt-gen-org-barbar (append all-posts index-and-tags))))

(defun yynt-gen-all-reposts-barbar ()
  "重新构建 reposts 中的 org 文件，也包括 index 目录文件"
  (interactive)
  (let ((all-posts (mapcar 'cdr (yynt-get-all-repost-files)))
	(index (list (file-name-concat yynt-basedir "republish" "index.org"))))
    (yynt-gen-org-barbar (append all-posts index))))

(defun yynt-gen-all-projecteuler-barbar ()
  "重新构建 euler 中的 org 文件"
  (interactive)
  (let ((all-posts (directory-files
		    (file-name-concat yynt-basedir "projecteuler")
		    t "[0-9]+\\.org"))
	(index (list (file-name-concat yynt-basedir "projecteuler" "index.org"))))
    (yynt-gen-org-barbar (append all-posts index))))

(defun yynt-gen-toplevel-barbar ()
  "重新构建位于根目录的 org 文件"
  (interactive)
  (let ((all-files (list (file-name-concat yynt-basedir "index.org")
			 (file-name-concat yynt-basedir "404.org"))))
    (yynt-gen-org-barbar all-files)
  (yynt-rss-update)))

(defun yynt-gen-barbar ()
  "重新构建整个 blog"
  (interactive)
  (yynt-gen-all-posts-barbar)
  (yynt-gen-all-reposts-barbar)
  (yynt-gen-all-projecteuler-barbar)
  (yynt-gen-toplevel-barbar))

;; 更新 post 需要更新 post index, post tags index, homepage index, rss
(defun yynt-update-current-post (filepath &optional noquiz)
  "更新某一 post，同时更新各种目录文件和 rss"
  (interactive (list (buffer-file-name (current-buffer))))
  (if (or (not (string-match-p (file-name-concat yynt-basedir "posts")
			       filepath))
	  (string= (file-name-directory filepath)
		   (file-name-concat yynt-basedir "posts/")))
      (message "current not in post's dir dir, quit")
    (let ((idx-tag-home (list (file-name-concat yynt-basedir "posts" "index.org")
			      (file-name-concat yynt-basedir "posts" "tags.org")
			      (file-name-concat yynt-basedir "index.org"))))
      (if (string= "org" (file-name-extension filepath))
	  (progn (yynt-gen-org-barbar (cons filepath idx-tag-home))
		 (yynt-rss-update))
	(if (or noquiz (y-or-n-p "no org file, still want to update?"))
	    (progn (yynt-gen-org-barbar idx-tag-home)
		   (yynt-rss-update))
	  (message "update nothing, quit"))))))

;; 更新 reposts 需要更新 repost index, homepage index
(defun yynt-update-current-repost (filepath)
  "更新某一 repost，以及各种 index 文件"
  (interactive (list (buffer-file-name (current-buffer))))
  (if (or (not (string-match-p (file-name-concat yynt-basedir "republish")
			       filepath))
	  (string= (file-name-directory filepath)
		   (file-name-concat yynt-basedir "republish/")))
      (message "current not in repost's dir dir, quit")
    (let ((idx-home (list (file-name-concat yynt-basedir "republish" "index.org")
			  (file-name-concat yynt-basedir "index.org"))))
      (if (string= "org" (file-name-extension filepath))
	  (yynt-gen-org-barbar (cons filepath idx-home))
	(yynt-gen-org-barbar idx-home)))))

;; 更新 projecteuler 需要更新 euler index
(defun yynt-update-current-euler (filepath)
  (interactive (list (buffer-file-name (current-buffer))))
  (if (not (string= (file-name-directory filepath)
		    (file-name-concat yynt-basedir "projecteuler/")))
      (message "current not in euler's dir, quit")
    (if (not (string= "org" (file-name-extension filepath)))
	(message "file is not euler's source")
      (yynt-gen-org-barbar (list filepath
				 (file-name-concat yynt-basedir
						   "projecteuler" "index.org"))))))

;; 这里采用先原地生成后复制到目标的方法
;; 这样一来，资源的发布就是复制而已

;;; 这里参考了 org-publish-attachment
;; (file-relative-name)
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

(defun yynt-publish-global-resource ()
  "将全局图片和一些东西进行复制"
  (interactive)
  (let ((css (cddr (directory-files (file-name-concat yynt-basedir "css") t)))
	(img (cddr (directory-files (file-name-concat yynt-basedir "img") t)))
	(js  (cddr (directory-files (file-name-concat yynt-basedir "js")  t)))
	(rss (list (file-name-concat yynt-basedir "rss.xml")))
	(eulerimg (cddr (directory-files
			 (file-name-concat yynt-basedir "projecteuler" "res") t)))
	;; 404 可能几个月更新一次
	(f404 (list (file-name-concat yynt-basedir "404.html"))))
    (mapc 'yynt-publish-attachment (append css img js rss eulerimg f404))))

(defun yynt-publish-single-post (dir)
  "将某个 posts 下的所有文件发布
不包括一些文件，比如 org, pptx, "
  (let* ((files (cddr (directory-files dir t)))
	 (exfiles (cl-remove-if
		   (lambda (x)
		     (string-match-p
		      "pptx?\\|org"
		      (file-name-extension x)))
		   files)))
    (mapc 'yynt-publish-attachment exfiles)))

(defun yynt-publish-current-post-barbar (filename)
  "将当前 post 发布"
  (interactive (list (buffer-file-name (current-buffer))))
  ;; 首先重生成
  (yynt-update-current-post filename t)
  (yynt-publish-single-post (file-name-directory filename))
  ;;接着发布目录文件和主页，以及 rss
  (yynt-publish-attachment (file-name-concat yynt-basedir "posts" "index.html"))
  (yynt-publish-attachment (file-name-concat yynt-basedir "posts" "tags.html"))
  (yynt-publish-attachment (file-name-concat yynt-basedir "index.html"))
  (yynt-publish-attachment (file-name-concat yynt-basedir "rss.xml")))

(defun yynt-publish-all-posts-barbar (update)
  "发布所有的 posts 内容，交互模式下询问是否更新"
  (interactive (list (y-or-n-p "Update?")))
  (when update
    (yynt-gen-all-posts-barbar))
  (mapc 'yynt-publish-single-post
	(mapcar (lambda (x) (file-name-directory (cdr x)))
		(yynt-get-all-post-files)))
  (yynt-publish-attachment (file-name-concat yynt-basedir "posts" "index.html"))
  (yynt-publish-attachment (file-name-concat yynt-basedir "posts" "tags.html")))

(defun yynt-publish-single-repost (dir)
  "将某个 repost 下的所有文件发布
不包括 org，如果源文件是 html，则直接复制整个目录"
  (if (file-exists-p (file-name-concat dir "index.htm"))
      (let* ((b-dir (file-relative-name dir yynt-basedir))
	    (new-dir (file-name-concat yynt-publish-dir b-dir)))
	(copy-directory dir new-dir t t t))
    (yynt-publish-single-post dir)))

(defun yynt-publish-current-repost-barbar (filename)
  "发布当前 repost"
  (interactive (list (buffer-file-name (current-buffer))))
  (yynt-update-current-repost filename)
  (yynt-publish-single-repost (file-name-directory filename))
  (yynt-publish-attachment (file-name-concat yynt-basedir "republish" "index.html"))
  (yynt-publish-attachment (file-name-concat yynt-basedir "index.html")))

(defun yynt-publish-all-reposts-barbar (update)
  "发布所有的 repost 内容，询问是否更新"
  (interactive (list (y-or-n-p "Update?")))
  (when update
    (yynt-gen-all-reposts-barbar))
  (mapc 'yynt-publish-single-repost
	(mapcar (lambda (x) (file-name-directory (cdr x)))
		(yynt-get-all-repost-files)))
  (yynt-publish-attachment (file-name-concat yynt-basedir "republish" "index.html")))

(defun yynt-publish-current-euler-barbar (filename)
  "发布当前 euler"
  (interactive (list (buffer-file-name (current-buffer))))
  (yynt-update-current-euler filename)
  (yynt-publish-attachment (file-name-concat
			    (file-name-directory filename)
			    (format "%s.html" (file-name-base filename))))
  (yynt-publish-attachment (file-name-concat yynt-basedir "projecteuler" "index.html")))

(defun yynt-publish-all-euler-barbar (update)
  "发布所有的 euler 内容，询问是否更新"
  (interactive (list (y-or-n-p "Update?")))
  (when update
    (yynt-gen-all-projecteuler-barbar))
  (mapc 'yynt-publish-attachment
	(directory-files (file-name-concat yynt-basedir "projecteuler")
			 t ".*\\.html")))

(defun yynt-publish-current-file (filename &optional update)
  "发布当前文件（也可用变量指定）
若为 org 文件则询问是否更新对应 html，随后发布"
  (interactive (list (buffer-file-name (current-buffer))))
  (if (not (string= (file-name-extension filename) "org"))
      (yynt-publish-attachment filename)
    (when (or update (y-or-n-p "Update this file?"))
      (yynt-gen-org-barbar (list filename)))
    (yynt-publish-attachment (file-name-concat
			      (file-name-directory filename)
			      (format "%s.html" (file-name-base filename))))))

(defun yynt-publish-whole-barbar (update)
  "发布当前博客的全部内容，询问是否重生成"
  (interactive (list (y-or-n-p "Update all?")))
  (when update
    (yynt-gen-barbar))
  (yynt-publish-all-euler-barbar nil)
  (yynt-publish-all-reposts-barbar nil)
  (yynt-publish-all-posts-barbar nil)
  (yynt-publish-global-resource)
  (yynt-publish-current-file
   (file-name-concat yynt-basedir "index.org")))

;; https://sachachua.com/blog/2023/01/adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export/
;; (setq org-babel-exp-code-template "#+begin_src %lang :summary %summary\n%body\n#+end_src")
;; (defun t-org-html-src-block (src-block _contents info)
;;   (let* ((result (org-html-src-block src-block _contents info))
;;          (block-info
;;           (org-with-point-at (org-element-property :begin src-block)
;;             (org-babel-get-src-block-info)))
;;          (summary (assoc-default :summary (elt block-info 2))))
;;     (if (member summary '("%summary" ""))
;;         result
;;       (format "<details><summary>%s</summary>%s</details>"
;;               summary
;;               result))))
;; (with-eval-after-load 'ox-html
;;   (map-put!
;;    (org-export-backend-transcoders (org-export-get-backend 'html))
;;    'src-block 't-org-html-src-block))

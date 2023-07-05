;; -*- lexical-binding: t; -*-

(require 'htmlize (expand-file-name "./htmlize.el"))
(load-file (expand-file-name "./ox-yyhtml.el"))

(defvar yynt-basedir (expand-file-name "./"))

(defun yynt-imgattr ()
  "插入用于头图和尾图的 CSS"
  (interactive)
  (insert "#+ATTR_HTML: :class top-down-img"))

;;; post
(defun yynt-post-postamble (_info)
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

(defun yynt-post-head (_info)
  "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../../css/style.css\">
<link rel=\"icon\" type=\"image/x-icon\" href=\"../../img/rin.ico\">
<script src=\"../../js/copycode.js\"></script>
<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
<link href=\"https://fonts.googleapis.com/css2?family=Roboto&display=swap\" rel=\"stylesheet\">")

(defun yynt-post-index-postamble (_info)
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

;;; homepage
(defun yynt-homepage-postamble (_info)
  "<hr><p style=\"text-align:center;\"><i>homepage ends here~</i></p><br>")

(defun yynt-repost-home/up-func (_info)
  "<div id=\"org-div-home-and-up\">
<a href=\"../index.html\"> HOME </a>
</div>\n")

;;; repost
(defun yynt-repost-index-postamble (_info)
  "\
<hr>
<div id=\"cc-container\">
<div>
<p>Updated: %C</p>
</div>
<a href=\"https://erkin.party/emacs/\"><img src=\"../img/made4.gif\" alt=\"⑨\"></a>
</div><br>
")

(defun yynt-repost-postamble (_info)
  "\
<hr>
<div>
<p>Author: %a</p>
<p>Created: %d</p>
<p>Updated: %C</p>
<p class=\"creator\">Creator: %c</p>
</div>")

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

;;; yy.el -- manager for egh0bww1 -*- lexical-binding:t; -*-

;; Created: 22 Apr 2024
;; Author: include-yy <yy@egh0bww1.com>

;;; Commentary:

;;; Code:

(require 'yynt)
(require 'ox-w3ctr)

(defun yynt/yy-fn (plist in out)
  (if (string-match-p "\\.org$" in)
      (let ((default-directory (file-name-directory in))
	    (org-export-coding-system 'utf-8-unix)
	    (org-export-use-babel nil))
	(org-export-to-file 'w3ctr out
	  nil nil nil nil plist))
    t))

(defun yynt/yy-convert-fn (file)
  (if (string= "org" (file-name-extension file))
      (file-name-with-extension file "html")
    file))

(defvar yynt/yy-project)
(setq yynt/yy-project
      (yynt-create-project 'egh0bww1 "blog-build" "build.sqlite3"
			   '("title" "filetags" "description" "date" "tmp")))
(yynt-choose-project 'egh0bww1)

(defvar yynt/yy-common-plist
  '( :with-sub-superscript {} ; #+options: ^:{}
     :html-head-include-style nil ; #+options: html-style:nil
     :html-timestamp-option int
     :html-timestamp-wrapper time
     :html-datetime-option T-none-zulu
     :html-timezone 28800
     :html-export-timezone 0
     :author "include-yy"
     ))

(defvar yynt/yy-index)
(setq yynt/yy-index
      (yynt-create-build
       :project yynt/yy-project
       :path "index.org"
       :type 0
       :collect-ex t
       :fn #'yynt/yy-fn
       :no-cache-files t
       :published t
       :convert-fn #'yynt/yy-convert-fn
       :included-resources '("assets")
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :section-numbers nil
		 :html-preamble nil
		 :html-zeroth-section-tocname nil
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"./assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"./assets/img/lily.svg\">"
		 :html-fixup-js "\
<script src=\"./assets/js/fixup.js\"></script>"
                 :html-timestamp-option fmt
                 :html-timestamp-formats ("%F" . "%F %R")
		 ))))

(defvar yynt/yy-404)
(setq yynt/yy-404
      (yynt-create-build
       :project yynt/yy-project
       :path "404.org"
       :type 0
       :fn #'yynt/yy-fn
       :no-cache-files nil
       :published t
       :convert-fn #'yynt/yy-convert-fn
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :with-toc nil
		 :html-back-to-top nil
		 :section-numbers nil
		 :html-preamble nil
		 :html-zeroth-section-tocname nil
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"https://egh0bww1.com/assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"https://egh0bww1.com/assets/img/lily.svg\">"
		 :html-fixup-js "\
<script src=\"https://egh0bww1.com/assets/js/fixup.js\"></script>"
		 ))))

(defun yynt/yy-rss-fn (_plist _in out)
  (with-temp-file out
    (insert (yynt/yy-rss-generate))))

(defvar yynt/yy-rss)
(setq yynt/yy-rss
      (yynt-create-build
       :project yynt/yy-project
       :path "rss.xml"
       :type 0
       :collect-ex t
       :fn #'yynt/yy-rss-fn
       :no-cache-files t
       :published t
       :convert-fn #'identity))

(defvar yynt/yy-rss-link "https://egh0bww1.com")
(defvar yynt/yy-rss-post-link "https://egh0bww1.com/posts/")
(defvar yynt/yy-rss-post-title "include-yy's blog")
(defvar yynt/yy-rss-post-description "My recent 10 articles")
(defvar yynt/yy-rss-post-n 10)

(defun yynt/yy-rss-get-current-time ()
  (format-time-string "%Y-%m-%dT%H:%M:%S" nil t))

(defun yynt/yy-rss-generate-chan-header ()
  (format "\
<title>%s</title>
<link>%s</link>
<description>%s</description>
<pubDate>%s</pubDate>\n"
	  yynt/yy-rss-post-title
	  yynt/yy-rss-link
	  yynt/yy-rss-post-description
	  (yynt/yy-rss-get-current-time)))

(defun yynt/yy-rss-generate-item (title link desc &optional tag date)
  (format "<item>
<title>%s</title>
<link>%s</link>
<description>%s</description>%s%s
</item>"
	  title link desc
	  (if tag (format "\n<category>%s</category>" tag) "")
	  (if date (format "\n<pubDate>%s</pubDate>" date) "")))

(defun yynt/yy-get-post-rss ()
  (let ((items (yynt-select* "\
SELECT title, path, description, filetags, substr(path,7,10) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%'
ORDER BY path DESC
LIMIT ?" (list yynt/yy-rss-post-n))))
    items))

(defun yynt/yy-rss-generate ()
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
   "<!-- refs: https://www.runoob.com/rss/rss-syntax.html -->\n"
   "<rss version=\"2.0\">\n"
   "<channel>\n"
   (yynt/yy-rss-generate-chan-header)
   (mapconcat
    (lambda (x)
      (cl-multiple-value-bind (title link desc tag date) x
	(if (or (null link) (null title) (null desc))
	    (error "yynt-rss: %s misses title or description!" (caar x))
	  (setq link (concat yynt/yy-rss-link "/"
			     (if (string= "org" (file-name-extension link))
				 (file-name-with-extension link "html")
			       link)))
	  (yynt/yy-rss-generate-item title link desc tag date))))
    (yynt/yy-get-post-rss) "\n")
   "\n</channel>\n</rss>"))


(defvar yynt/yy-euler)
(setq yynt/yy-euler
      (yynt-create-build
       :project yynt/yy-project
       :path "projecteuler"
       :type 1
       :collect (yynt-p1 "^[0-9]+\\.org")
       :collect-ex (yynt-p1s '("index.org"))
       :fn #'yynt/yy-fn
       :attrs '("description" "filetags" "date")
       :no-cache-files '("index.org")
       :published t
       :convert-fn #'yynt/yy-convert-fn
       :included-resources '("res")
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :html-zeroth-section-tocname nil
		 :author "include-yy"
		 :html-link-navbar [("../index.html" . "HOME")
				     ("./index.html" . "SUM")]
		 :html-link-left "../index.html"
		 :html-link-lname "HOME"
		 :html-link-right "./index.html"
		 :html-link-rname "SUM"
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../assets/img/lily.svg\">"
		 :html-fixup-js "\
<script src=\"../assets/js/fixup.js\"></script>"))
       :info-ex '( :html-preamble nil
		   :section-numbers nil
		   :html-link-navbar [("../index.html". "HOME")]
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-right ""
		   :html-link-rname ""
		   )))

(defun yynt/yy-euler-table (prefix)
  (let ((items (yynt-select* "\
SELECT substr(path, 14), filetags, description, date FROM YYNT WHERE
build_name='projecteuler' AND ex='0'")))
    (setq items (sort items (lambda (a b)
			      (let ((a0 (string-to-number (car a)))
				    (b0 (string-to-number (car b))))
				(< a0 b0)))))
    (concat "|Problem|Description|TAG|TIME|\n"
	    "|-+-+-+-|\n"
	    (mapconcat
	     (lambda (x)
	       (let ((path (file-name-concat prefix (nth 0 x)))
		     (name (string-to-number (nth 0 x)))
		     (tag (nth 1 x))
		     (desc (nth 2 x))
		     (date (nth 3 x)))
		 (format "|[[file:%s][%s]]|%s|%s|[%s]|"
			 path name
			 (or desc " ")
			 (or tag " ")
			 (substring date 1 11))))
	     items
	     "\n"))))


(defvar yynt/yy-repost)
(setq yynt/yy-repost
      (yynt-create-build
       :project yynt/yy-project
       :path "republish"
       :type 2
       :collect (yynt-p2 "^2" "\\.\\(htm\\|org\\)$")
       :collect-ex (yynt-p1s '("index.org"))
       :fn #'yynt/yy-fn
       :no-cache-files '("index.org")
       :ext-files '("index.org")
       :attrs '("title")
       :published t
       :convert-fn #'yynt/yy-convert-fn
       :collect-2 (yynt-c2 "^2")
       :excluded-fn-2 (yynt-e2 "\\.org$")
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :html-preamble nil
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../../assets/img/lily.svg\">"
		 :html-fixup-js "\
<script src=\"../../assets/js/fixup.js\"></script>"
		 :html-link-navbar [("../../index.html". "HOME")
				     ("../index.html" . "REPUB")]
		 :html-link-lname "HOME"
		 :html-link-left "../../index.html"
		 :html-link-rname "REPUB"
		 :html-link-right "../index.html"))
       :info-ex '( :section-numbers nil
		   :with-toc nil
		   :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../assets/img/lily.svg\">"
		   :html-fixup-js "\
<script src=\"../assets/js/fixup.js\"></script>"
		   :html-link-navbar [("../index.html" . "HOME")]
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-rname ""
		   :html-link-right ""
                   :html-timestamp-formats ("%F" . "%F %R")
                   :html-timestamp-option fmt
		   )))

(defun yynt/yy-repost-list (prefix limit)
  (let ((items (yynt-select* "\
SELECT path, title FROM YYNT WHERE
build_name='republish' AND ex='0' AND file_name LIKE 'index%'
ORDER BY path DESC
LIMIT ?" (list (or limit 100000)))))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "republish/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat
				 prefix (substring name len))
			   title)))
	       items "\n")))

(defvar yynt/yy-post)
(setq yynt/yy-post
      (yynt-create-build
       :project yynt/yy-project
       :path "posts"
       :type 2
       :collect (yynt-p2 "^2" "\\.\\(htm\\|org\\)$")
       :collect-ex (yynt-p1s '("index.org" "tags.org"))
       :fn #'yynt/yy-fn
       :no-cache-files '("index.org" "tags.org")
       :ext-files '("index.org" "rss.xml")
       :attrs '("title" "filetags" "description")
       :published t
       :convert-fn #'yynt/yy-convert-fn
       :collect-2 (yynt-c2 "^2")
       :excluded-fn-2 (yynt-e2 "\\(dev\\)\\|\\(\\.org$\\)")
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :author "include-yy"
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../../assets/img/lily.svg\">"
		 :html-fixup-js "\
<script src=\"../../assets/js/fixup.js\"></script>"
		 :html-link-navbar [("../../index.html" . "HOME")
				     ("../index.html" . "BLOG")]
		 :html-link-lname "HOME"
		 :html-link-left "../../index.html"
		 :html-link-rname "BLOG"
		 :html-link-right "../index.html"))
       :info-ex '( :html-preamble nil
		   :section-numbers nil
		   :html-zeroth-section-tocname nil
		   :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../assets/img/lily.svg\">"
		   :html-fixup-js "\
<script src=\"../assets/js/fixup.js\"></script>"
		   :html-link-navbar [("../index.html" . "HOME")
				       ("./tags.html" . "TAGS")]
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-rname "TAGS"
		   :html-link-right "./tags.html"
                   :html-timestamp-option fmt
                   :html-timestamp-formats ("%F" . "%F %R")
		   )))

(defun yynt/yy-post-list (prefix limit)
  (let ((items (yynt-select* "\
SELECT path, title FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%'
ORDER BY path DESC
LIMIT ?" (list (or limit 100000)))))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "posts/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat prefix (substring name len))
			   title)))
	       items "\n")))

(defun yynt/yy-post-tag-list (prefix tag)
  (let ((items (yynt-select* "\
SELECT path, title FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND filetags=?
ORDER BY path DESC" (list tag))))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "posts/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat prefix (substring name len))
			   title)))
	       items "\n")))

(defun yynt/yy-post-tag-num (tag)
  (let ((res (yynt-select* "\
SELECT COUNT(*) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND filetags=?
ORDER BY path DESC" (list tag))))
    (format "=%s=" (caar res))))

(defun yynt/yy-post-year-list (prefix year)
  (let ((items (yynt-select* "\
SELECT path, title FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND
substr(path, 7, 4)=?
ORDER BY path DESC" (list year))))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "posts/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat prefix (substring name len))
			   title)))
	       items "\n")))

(defun yynt/yy-post-year-num (year)
  (let ((res (yynt-select* "\
SELECT COUNT(*) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND
substr(path, 7, 4)=?
ORDER BY path DESC" (list year))))
    (format "=%s=" (caar res))))

(defun yynt/yy-post-total-num ()
  (let ((res (yynt-select* "\
SELECT COUNT(*) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%'")))
    (format "%s" (caar res))))

(defvar yynt/yy-drafts)
(setq yynt/yy-drafts
      (yynt-create-build
       :project yynt/yy-project
       :path "drafts"
       :type 2
       :collect (yynt-p2 "^2" "\\.\\(htm\\|org\\)$")
       :collect-ex (yynt-p1s '("index.org"))
       :fn #'yynt/yy-fn
       :no-cache-files '("index.org")
       :ext-files nil
       :attrs '("title" "tmp")
       :convert-fn #'yynt/yy-convert-fn
       :collect-2 (yynt-c2 "^2")
       :excluded-fn-2 (yynt-e2 "\\(dev\\)\\|\\(\\.org$\\)")
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :author "include-yy"
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../../assets/img/lily.svg\">"
		 :html-fixup-js "\
<script src=\"../../assets/js/fixup.js\"></script>"
		 :html-link-navbar [("../../index.html" . "HOME")
				     ("../index.html" . "DRAFT")]
		 :html-link-lname "HOME"
		 :html-link-left "../../index.html"
		 :html-link-rname "DRFAT"
		 :html-link-right "../index.html"))
       :info-ex '( :html-preamble nil
		   :section-numbers nil
		   :html-zeroth-section-tocname nil
		   :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../assets/img/lily.svg\">"
		   :html-fixup-js "\
<script src=\"../assets/js/fixup.js\"></script>"
		   :html-link-navbar [("../index.html" . "HOME")]
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-rname ""
		   :html-link-right ""
                   :html-timestamp-option fmt
                   :html-timestamp-formats ("%F" . "%F %R")
		   )))

(defun yynt/yy-drafts-tmp-list (prefix tmp)
  (let ((items (yynt-select* "\
SELECT path, title FROM YYNT WHERE
build_name='drafts' AND ex='0' AND file_name LIKE 'index%' AND tmp=?
ORDER BY path DESC" (list tmp))))
    (setq items (cl-remove-if-not
		 (lambda (x) (let ((file (yynt-get-file-project-fullname
				      (car x) yynt/yy-project)))
			   (file-exists-p file)))
		 items))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "drafts/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat prefix (substring name len))
			   title)))
	       items "\n")))

(defun yynt/yy-drafts-total-num ()
  (let ((res (yynt-select* "\
SELECT path FROM YYNT WHERE
build_name='drafts' AND ex='0' AND file_name LIKE 'index%'")))
    (format "%s"
	    (length (cl-remove-if-not
		     (lambda (x) (let ((file (yynt-get-file-project-fullname
					  (car x) yynt/yy-project)))
			       (file-exists-p file)))
		     res)))))

;; some template for posts, reposts and euler
;; [YYYY-MM-DD DAY HH:MM]
(defun yynt/yy-temp-current-time ()
  (format-time-string "[%Y-%m-%d %a %H:%M]"))
(defun yynt/yy-temp-post (title tag)
  (insert (format
	   "\
#+TITLE: %s
#+DATE: %s
#+FILETAGS: %s
#+DESCRIPTION: ...
#+TMP: 0（未完成的草稿） 1（长期笔记）2（垃圾）"
	   title (yynt/yy-temp-current-time) tag)))

(defun yynt/yy-temp-repost ()
  (insert "\
#+TITLE:
#+DATE:
#+FILETAGS:
#+AUTHOR:"))

(defun yynt/yy-temp-euler (num)
  (insert (format
	   "\
#+TITLE: Problem %s
#+DATE:
#+FILETAGS: 如有必要可添加标签，如 #prime#
#+DESCRIPTION: 最好小于二十汉字。。汉字汉字汉字汉字汉字

* Problem\n
*[[https://projecteuler.net/problem=%s]]*\n
*中文题目*\n
https://pe-cn.github.io/%s\n
* Solution"
	   num num num)))

(defun yynt/yy-create-draft (dirname title tag)
  (interactive (list (read-from-minibuffer "Enter dirname: ")
		     (read-from-minibuffer "Enter title: ")
		     (completing-read "Select tag: "
				      (yynt/yy--post-read-tags))))
  (let ((dirpath (file-name-concat
		  (yynt-get-file-project-fullname "drafts" yynt/yy-project)
		  (concat (format-time-string "%Y-%m-%d-")
			  dirname))))
    (make-directory dirpath)
    (find-file (file-name-concat dirpath "index.org"))
    (yynt/yy-temp-post title tag)))

(defun yynt/yy-move-draft (dirpath)
  (interactive (list default-directory))
  (if (not (equal (yynt-get-file-project-fullname "drafts/" yynt/yy-project)
		  (file-name-directory
		   (directory-file-name default-directory))))
      (message "currently not in draft source file, quit")
    (let ((newdir (file-name-concat
		   (yynt-get-file-project-fullname "posts" yynt/yy-project)
		   (concat (format-time-string "%Y-%m-%d-")
			   (substring (yynt-get-file-build-basename
				       default-directory
				       yynt/yy-drafts)
				      11)))))
      (copy-directory dirpath newdir t t t)
      (find-file (file-name-concat newdir "index.org"))
      (set-buffer-file-coding-system 'utf-8)
      (message "publish draft fin"))))

(defun yynt/yy-create-repost (dirname)
  (interactive (list (read-from-minibuffer "Enter dirname: ")))
  (let ((dirpath (file-name-concat
		  (yynt-get-file-project-fullname "republish" yynt/yy-project)
		  (concat (format-time-string "%Y-%m-%d-")
			  dirname))))
    (make-directory dirpath)
    (find-file (file-name-concat dirpath "index.org"))
    (set-buffer-file-coding-system 'utf-8)
    (yynt/yy-temp-repost)))

(defun yynt/yy-create-euler (number)
  (interactive (list (read-from-minibuffer "Enter problem Number: ")))
  (let ((filepath (file-name-concat
		   (yynt-get-file-project-fullname
		    "projecteuler" yynt/yy-project)
		   (concat number ".org"))))
    (find-file filepath)
    (unless (file-exists-p filepath)
      (set-buffer-file-coding-system 'utf-8)
      (yynt/yy-temp-euler number))))

(defvar yynt/yy--post-tags-file
  (yynt-get-file-project-fullname "tags.eld" yynt/yy-project))

(defun yynt/yy--post-read-tags ()
  (read (with-temp-buffer
	  (insert-file-contents
	   yynt/yy--post-tags-file)
	  (buffer-string))))

(defun yynt/yy--post-write-tags (taglist)
  (with-temp-message ""
    (with-temp-buffer
      (insert (pp-to-string taglist))
      (write-file yynt/yy--post-tags-file)))
  (message "yynt: write tags fin"))

(defun yynt/yy-post-insert-tag ()
  (interactive)
  (let* ((tags (yynt/yy--post-read-tags))
	 (selected (completing-read "Select a tag: " tags)))
    (insert selected)))

(defun yynt/yy-post-add-tags (newtag)
  (interactive (list (downcase (read-from-minibuffer "Enter a new tag: "))))
  (let ((tags (yynt/yy--post-read-tags)))
    (if (and (string-match-p "[[:alnum:]]+" newtag)
	     (cl-every (lambda (x) (not (string= x newtag))) tags))
	(yynt/yy--post-write-tags (cons newtag tags))
      (message "newtag exists or contains non-alnum char"))))

(defun yynt/yy-post-delete-tags (tag)
  (interactive (list (completing-read "Select a tag: "
				      (yynt/yy--post-read-tags))))
  (yynt/yy--post-write-tags (remove tag (yynt/yy--post-read-tags))))



(defun yynt/yy-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  (yynt-export-current-buffer (current-buffer) yynt/yy-project))

(org-export-define-derived-backend 'yy 'w3ctr
  :menu-entry
  '(?y "Export to W3C technical reports style html"
       ((?y "As HTML file" yynt/yy-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (org-open-file (yynt/yy-export-to-html nil s v b)))))))

;;; yy.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; End:

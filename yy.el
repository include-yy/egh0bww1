;;; yy.el -- manager for egh0bww1 -*- lexical-binding:t; -*-

;; Created: 22 Apr 2024
;; Author: include-yy <yy@egh0bww1.com>

;;; Commentary:

;;; Code:

(if (file-exists-p "yynt/yynt.el")
    (require 'yynt (file-truename "yynt/yynt.el"))
  (require 'yynt))

(if (file-exists-p "ox-w3ctr/ox-w3ctr.el")
    (require 'ox-w3ctr (file-truename "ox-w3ctr/ox-w3ctr.el"))
  (require 'ox-w3ctr))

(defun yynt/yy-fn (plist in out)
  (if (string-match-p "\\.org$" in)
      (let ((org-export-coding-system org-w3ctr-coding-system)
	    (org-export-use-babel org-w3ctr-use-babel))
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
     :html-head-include-default-style nil ; #+options: html-style:nil
     ))

(defvar yynt/yy-index)
(setq yynt/yy-index
      (yynt-create-build
       :path "index.org"
       :type 0
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
<script type=\"text/javascript\" src=\"./assets/js/fixup.js\"></script>"
		 ))))

(defvar yynt/yy-404)
(setq yynt/yy-404
      (yynt-create-build
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
<script type=\"text/javascript\" src=\"https://egh0bww1.com/assets/js/fixup.js\"></script>"
		 ))))

(defun yynt/yy-rss-fn (_plist _in out)
  (with-temp-file out
    (insert (yynt/yy-rss-generate))))

(defvar yynt/yy-rss)
(setq yynt/yy-rss
      (yynt-create-build
       :path "rss.xml"
       :type 0
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
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
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
   ;; 单个数据格式为 ((dir . fpath) . alist), key:{title, filetags, yyntrss}
   (mapconcat (lambda (x)
		(cl-multiple-value-bind (link title desc tag date) x
		  (if (or (null link) (null title) (null desc))
		      (error "yynt-rss: %s misses title or description!" (caar x))
		    (yynt/yy-rss-generate-item title link desc tag date))))
	      (yynt/yy-get-post-rss) "\n")
   "\n</channel>\n</rss>"))


(defvar yynt/yy-euler)
(setq yynt/yy-euler
      (yynt-create-build
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
       :ext-files '("index.org")
       :info (yynt-combine-plists
	      yynt/yy-common-plist
	      '( :html-zeroth-section-tocname nil
		 :author "include-yy"
		 :html-link-left "../index.html"
		 :html-link-lname "HOME"
		 :html-link-right "./index.html"
		 :html-link-rname "SUMMARY"
		 :html-head "\
<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\">
<link rel=\"icon\" type=\"image/svg+xml\" href=\"../assets/img/lily.svg\">"
		 :html-fixup-js "\
<script type=\"text/javascript\" src=\"../assets/js/fixup.js\"></script>"))
       :info-ex '( :html-preamble nil
		   :section-numbers nil
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-right ""
		   :html-link-rname ""
		   )))

(defun yynt/yy-euler-table (prefix)
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
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
<script type=\"text/javascript\" src=\"../../assets/js/fixup.js\"></script>"
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
<script type=\"text/javascript\" src=\"../assets/js/fixup.js\"></script>"
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-rname ""
		   :html-link-right ""
		   )))

(defun yynt/yy-repost-list (prefix limit)
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
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
			     time (file-name-concat prefix (substring name len)) title)))
		 items "\n")))


(defvar yynt/yy-post)
(setq yynt/yy-post
      (yynt-create-build
       :path "posts"
       :type 2
       :collect (yynt-p2 "^2" "\\.\\(htm\\|org\\)$")
       :collect-ex (yynt-p1s '("index.org" "tags.org"))
       :fn #'yynt/yy-fn
       :no-cache-files '("index.org" "tags.org")
       :ext-files nil
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
<script type=\"text/javascript\" src=\"../../assets/js/fixup.js\"></script>"
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
<script type=\"text/javascript\" src=\"../assets/js/fixup.js\"></script>"
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-rname "TAGS"
		   :html-link-right "./tags.html"
		   )))

(defun yynt/yy-post-list (prefix limit)
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
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
			   time (file-name-concat prefix (substring name len)) title)))
	       items "\n")))

(defun yynt/yy-post-tag-list (prefix tag)
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
SELECT path, title FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND filetags=?
ORDER BY path DESC" (list tag))))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "posts/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat prefix (substring name len)) title)))
	       items "\n")))

(defun yynt/yy-post-tag-num (tag)
  (let ((res
	 (sqlite-select
	  yynt--sqlite-obj
	  "\
SELECT COUNT(*) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND filetags=?
ORDER BY path DESC" (list tag))))
    (format "=%s=" (caar res))))

(defun yynt/yy-post-year-list (prefix year)
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
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
			   time (file-name-concat prefix (substring name len)) title)))
	       items "\n")))

(defun yynt/yy-post-year-num (year)
  (let ((res (sqlite-select
		yynt--sqlite-obj
		"\
SELECT COUNT(*) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%' AND
substr(path, 7, 4)=?
ORDER BY path DESC" (list year))))
    (format "=%s=" (caar res))))

(defun yynt/yy-post-total-num ()
  (let ((res (sqlite-select
		yynt--sqlite-obj
		"\
SELECT COUNT(*) FROM YYNT WHERE
build_name='posts' AND ex='0' AND file_name LIKE 'index%'")))
    (format "%s" (caar res))))

(defvar yynt/yy-drafts)
(setq yynt/yy-drafts
      (yynt-create-build
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
<script type=\"text/javascript\" src=\"../../assets/js/fixup.js\"></script>"
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
<script type=\"text/javascript\" src=\"../assets/js/fixup.js\"></script>"
		   :html-link-lname "HOME"
		   :html-link-left "../index.html"
		   :html-link-rname ""
		   :html-link-right ""
		   )))

(defun yynt/yy-drafts-tmp-list (prefix tmp)
  (let ((items (sqlite-select
		yynt--sqlite-obj
		"\
SELECT path, title FROM YYNT WHERE
build_name='drafts' AND ex='0' AND file_name LIKE 'index%' AND tmp=?
ORDER BY path DESC" (list tmp))))
    (setq items (cl-remove-if-not
		 (lambda (x) (let ((file (yynt-get-file-project-fullname (car x) yynt/yy-project)))
			   (file-exists-p file)))
		 items))
    (mapconcat (lambda (i)
		 (let* ((name (car i))
			(title (cadr i))
			(len (length "drafts/"))
			(time (substring name len (+ len 10))))
		   (format "- [%s] [[%s][%s]]"
			   time (file-name-concat prefix (substring name len)) title)))
	       items "\n")))

(defun yynt/yy-drafts-total-num ()
  (let ((res (sqlite-select
		yynt--sqlite-obj
		"\
SELECT path FROM YYNT WHERE
build_name='drafts' AND ex='0' AND file_name LIKE 'index%'")))
    (format "%s"
	    (length (cl-remove-if-not
		     (lambda (x) (let ((file (yynt-get-file-project-fullname (car x) yynt/yy-project)))
			       (file-exists-p file)))
		     res)))))

;;; yy.el ends here

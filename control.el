;; -*- lexical-binding: t -*-
(require 'vc)

(defun yy-read-html (filename)
  (let ((rootdir (vc-git-root default-directory)))
    (if rootdir
	(let ((fullname (concat rootdir "templates/" filename)))
	  (with-temp-buffer
	    (insert-file-contents fullname)
	    (buffer-string)))
      nil)))
;;restore
(defun yy-restore-template ()
  (setq org-html-preamble nil)
  (setq org-html-postamble 'auto))

;;add image command
(defun yy-org-add-img (fn)
  (interactive (list (ido-read-file-name ">")))
  (insert "@@html:<div class=\"yyheadimg\"><img src="
	  "\"./"(file-relative-name fn default-directory)"\""
	  " alt=\"load failed\"></div>@@"))


(progn
  ;;pre
  (setq org-html-preamble t)
  (setq org-html-preamble-format
	(list (list "en" (yy-read-html "pre.html"))))
  ;;post
  (setq org-html-postamble t)
  (setq org-html-postamble-format
	(list (list "en" (yy-read-html "post.html"))))
  ;;set time format
  (setq system-time-locale "C")
  )

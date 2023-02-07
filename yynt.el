;; -*- lexical-binding:t; -*-

(defvar t-var-store nil
  "list to store variables' previous value")

(defvar t-advice-store nil
  "list to store funtion's previous value")

(defun t-emptyp ()
  (interactive)
  (if (and (null t-var-store)
	   (null t-advice-store))
      (message "empty")
    (message "not empty")))

(defmacro t-setq (name val)
  `(progn
     (when (boundp ',name)
       (push (list ',name ,name) t-var-store))
     (setq ,name ,val)))

(defun t-restore-var ()
  (dolist (a t-var-store)
    (set (car a) (cadr a)))
  (setq t-var-store nil))

(defmacro t-adv (sym where func)
  `(progn
     (when (fboundp ',sym)
       (push (list ',sym ',func) t-advice-store))
     (advice-add ',sym ,where ',func)))

(defun t-restore-adv ()
  (dolist (a t-advice-store)
    (advice-remove (car a) (cadr a)))
  (setq t-advice-store nil))

(defun t-restore ()
  (interactive)
  (t-restore-var)
  (t-restore-adv))

(t-setq org-publish-project-alist
	`(("resources"
	   :base-directory ,(expand-file-name "posts")
	   :base-extension "jpg\\|jpeg\\|gif\\|png\\|JPG\\|JPEG\\|GIF\\|PNG\\|css\\|el\\|py\\|c\\|ico\\|js"
	   :publishing-directory ,(expand-file-name "blog-build/posts")
	   :publishing-function org-publish-attachment
	   :recursive t)
	  ("articles"
	   :base-directory ,(expand-file-name "posts")
	   :base-extension "org\\|htm"
	   :publishing-directory ,(expand-file-name "blog-build/posts")
	   :publishing-function t-publish-org-htm
	   :recursive t)
	  ("js"
	   :base-directory ,(expand-file-name "js")
	   :base-extension "js"
	   :publishing-directory ,(expand-file-name "blog-build/js")
	   :publishing-function org-publish-attachment)
	  ("css"
	   :base-directory ,(expand-file-name "css")
	   :base-extension "css"
	   :publishing-directory ,(expand-file-name "blog-build/css")
	   :publishing-function org-publish-attachment)
	  ("img"
	   :base-directory ,(expand-file-name "img")
	   :base-extension "ico\\|svg\\|jpg\\|gif\\|png"
	   :publishing-directory ,(expand-file-name "blog-build/img")
	   :publishing-function org-publish-attachment)
	  ("homepage"
	   :base-directory ,default-directory
	   :base-extension "org"
	   :publishing-directory ,(expand-file-name "blog-build")
	   :publishing-function org-html-publish-to-html
	   :exclude "README\\|setup")
	  ("yynt"
	   :components ("resources" "articles" "js" "css" "img" "homepage"))))

(defun t-imgattr ()
  (interactive)
  (insert "#+ATTR_HTML: :class top-down-img"))

(t-setq org-html-home/up-format "\
  <div id=\"home-and-up\"> <a href=\"%s\">BLOG</a> \
  <a href=\"%s\">HOME</a></div>
  ")

(t-setq org-html-preamble-format '(("en" "")))

(t-setq org-html-postamble-format '(("en" "\
  <hr>
  <div id=\"cc-container\">
  <div>
  <p class=\"author\">Author: %a</p>
  <p>Created: %d</p>
  <p>Updated: %C</p>
  <p class=\"creator\">Creator: %c</p>
  </div>
  <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">
  <img alt=\"CC-BY-SA 4.0\" src=\"../../img/by-sa.svg\"></a>
  </div>")))

(t-setq org-html-prefer-user-labels t)

(t-setq org-html-text-markup-alist
	'((bold . "<em>%s</em>")
	  (code . "<code>%s</code>")
	  (italic . "<i>%s</i>")
	  (strike-through . "<del>%s</del>")
	  (underline . "<span class=\"underline\">%s</span>")
	  (verbatim . "<code>%s</code>")))

(t-setq org-html-divs
	'((preamble "div" "preamble")
	  (content "main" "content")
	  (postamble "footer" "postamble")))

;; https://sachachua.com/blog/2023/01/adding-a-custom-header-argument-to-org-mode-source-blocks-and-using-that-argument-during-export/
(setq org-babel-exp-code-template "#+begin_src %lang :summary %summary\n%body\n#+end_src")
(defun t-org-html-src-block (src-block _contents info)
  (let* ((result (org-html-src-block src-block _contents info))
         (block-info
          (org-with-point-at (org-element-property :begin src-block)
            (org-babel-get-src-block-info)))
         (summary (assoc-default :summary (elt block-info 2))))
    (if (member summary '("%summary" ""))
        result
      (format "<details><summary>%s</summary>%s</details>"
              summary
              result))))
(with-eval-after-load 'ox-html
  (map-put!
   (org-export-backend-transcoders (org-export-get-backend 'html))
   'src-block 't-org-html-src-block))

(defun t-|org-html--build-meta-entry (str)
  (let ((len (length str)))
    (concat (substring str nil (- len 4))
	    ">\n")))
(t-adv org-html--build-meta-entry :filter-return t-|org-html--build-meta-entry)

(defun t-|org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.
  DATUM is an element or a `target' type object.  INFO is the
  current export state, as a plist.
  When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
  nil.  This doesn't apply to headlines, inline tasks, radio
  targets and targets."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (pcase type
	     ((or `headline `inlinetask) :CUSTOM_ID)
	     ((or `radio-target `target) :value)
	     (_ :name))
	   datum)))
    (when (eq type 'headline)
      (unless user-label
	(let ((numbers (org-export-get-headline-number datum info)))
	  (setq user-label (concat "org-h-" (mapconcat #'number-to-string numbers "-"))))))
    (when (org-html-standalone-image-p datum info)
      (unless user-label (setq user-label "")))
    (when (eq type 'special-block)
      (unless user-label
	(setq user-label nil)
	(setq named-only t)))
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
	       (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))
(t-adv org-html--reference :override t-|org-html--reference)

(defun t-|org-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
  CONTENTS holds the contents of the headline.  INFO is a plist
  holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (id (org-html--reference headline info))
	   (formatted-text
	    (if (plist-get info :html-self-link-headlines)
		(format "<a href=\"#%s\">%s</a>" id full-text)
	      full-text)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (apply #'format "<%s class=\"org-%s\">\n"
			 (make-list 2 html-type)))
	     (org-html-format-list-item
	      contents (if numberedp 'ordered 'unordered)
	      nil info nil
	      (concat (org-html--anchor id nil nil info) formatted-text)) "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let ((extra-class
	       (org-element-property :HTML_CONTAINER_CLASS headline))
	      (headline-class
	       (org-element-property :HTML_HEADLINE_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                          level
                          id
			  (if (not headline-class) ""
			    (format " class=\"%s\"" headline-class))
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (concat (mapconcat #'number-to-string numbers ".") ".")))
                           formatted-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
  ;; `org-info.js'.
  (if (eq (org-element-type first-content) 'section) contents
    (concat (org-html-section first-content "" info) contents))
  (org-html--container headline info)))))))

(defun t-|org-html-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
			   (1- (plist-get info :html-toplevel-hlevel))))
	     (section-number
	      (and (org-export-numbered-headline-p parent info)
		   (mapconcat
		    #'number-to-string
		    (org-export-get-headline-number parent info) "-"))))
        ;; Build return value.
	(format "<div class=\"outline-text-%d\">\n%s</div>\n"
		class-num
		(or contents ""))))))

(t-adv org-html-headline :override t-|org-html-headline)
(t-adv org-html-section :override t-|org-html-section)

(defun t-|org-html--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.
  INFO is a plist used as a communication channel.  When optional
  arguments CAPTION and LABEL are given, use them for caption and
  \"id\" attribute."
  (let ((html5-fancy (org-html--html5-fancy-p info)))
    (format (if html5-fancy "\n<figure%s>\n%s%s</figure>" ;;去掉了最后一个 \n
	      "\n<div%s class=\"figure\">\n%s%s\n</div>")
	    ;; ID.
	    (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
	    ;; Contents.
	    (if html5-fancy contents (format "<p>%s</p>" contents))
	    ;; Caption.
	    (if (not (org-string-nw-p caption)) ""
	      (format (if html5-fancy "<figcaption>%s</figcaption>\n" ;; 去掉和加上 \n
			"\n<p>%s</p>")
		      caption)))))
(t-adv org-html--wrap-image :override t-|org-html--wrap-image)

(defun t-|org-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
	 (dot (when (> (length html-ext) 0) "."))
	 (link-org-files-as-html-maybe
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
	    ;; needed.  See `org-html-link-org-files-as-html'.
	    (cond
	     ((and (plist-get info :html-link-org-files-as-html)
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) dot html-ext))
	     (t raw-path))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto" "news"))
	    (url-encode-url (concat type ":" raw-path)))
	   ((string= "file" type)
	    ;; During publishing, turn absolute file names belonging
	    ;; to base directory into relative file names.  Otherwise,
	    ;; append "file" protocol to absolute file name.
	    (setq raw-path
		  (org-export-file-uri
		   (org-publish-file-relative-name raw-path info)))
	    ;; Possibly append `:html-link-home' to relative file
	    ;; name.
	    (let ((home (and (plist-get info :html-link-home)
			     (org-trim (plist-get info :html-link-home)))))
	      (when (and home
			 (plist-get info :html-link-use-abs-url)
			 (file-name-absolute-p raw-path))
		(setq raw-path (concat (file-name-as-directory home) raw-path))))
	    ;; Maybe turn ".org" into ".html".
	    (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id, a headline title, a name or
	    ;; a target.
	    (let ((option (org-element-property :search-option link)))
	      (if (not option) raw-path
		(let ((path (org-element-property :path link)))
		  (concat raw-path
			  "#"
			  (org-publish-resolve-external-link option path t))))))
	   (t raw-path)))
	 (attributes-plist
	  (org-combine-plists
	   ;; Extract attributes from parent's paragraph.  HACK: Only
	   ;; do this for the first link in parent (inner image link
	   ;; for inline images).  This is needed as long as
	   ;; attributes cannot be set on a per link basis.
	   (let* ((parent (org-export-get-parent-element link))
		  (link (let ((container (org-export-get-parent link)))
			  (if (and (eq 'link (org-element-type container))
				   (org-html-inline-image-p link info))
			      container
			    link))))
	     (and (eq link (org-element-map parent 'link #'identity info t))
		  (org-export-read-attribute :attr_html parent)))
	   ;; Also add attributes from link itself.  Currently, those
	   ;; need to be added programmatically before `org-html-link'
	   ;; is invoked, for example, by backends building upon HTML
	   ;; export.
	   (org-export-read-attribute :attr_html link)))
	 (attributes
	  (let ((attr (org-html--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	   (org-export-inline-image-p
	    link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "<a href=\"#%s\"%s>%s</a>"
		  ;;(org-export-get-reference destination info)
		  (org-html--reference destination info)
		  attributes
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  ;; ID link points to an external file.
	  (`plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  (`nil
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (`headline
	   (let ((href (org-html--reference destination info))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat #'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc
			(org-export-data
			 (org-element-property :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (_
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (eq 'latex-environment (org-element-type destination))
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
	       ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (org-html--reference destination info))
             (let* ((ref (org-html--reference destination info))
                    (org-html-standalone-image-predicate
                     #'org-html--has-caption-p)
                    (counter-predicate
                     (if (eq 'latex-environment (org-element-type destination))
                         #'org-html--math-environment-p
                       #'org-html--has-caption-p))
                    (number
		     (cond
		      (desc nil)
		      ((org-html-standalone-image-p destination info)
		       (org-export-get-ordinal
			(org-element-map destination 'link #'identity info t)
			info 'link 'org-html-standalone-image-p))
		      (t (org-export-get-ordinal
			  destination info nil counter-predicate))))
                    (desc
		     (cond (desc)
			   ((not number) "No description for this link")
			   ((numberp number) (number-to-string number))
			   (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
	(format "<a href=\"#%s\" %s%s>%s</a>"
		fragment
		(format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			fragment fragment)
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
	      (org-html-encode-plain-text path)
	      attributes
	      desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
	(format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))
(t-adv org-html-link :override t-|org-html-link)

(defun t-publish-org-htm (plist filename pub-dir)
  (if (string= "org" (file-name-extension filename))
      (org-html-publish-to-html plist filename pub-dir)
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir t))
    (let ((output (expand-file-name (concat (file-name-base filename) ".html") pub-dir)))
      (unless (file-equal-p (expand-file-name (file-name-directory filename))
			    (file-name-as-directory (expand-file-name pub-dir)))
	(copy-file filename output t))
      ;; Return file name.
      output)))

(defun t-sitemap-files-to-lisp (files project format-entry)
  (let ((root (expand-file-name
	       (file-name-as-directory
		(org-publish-property :base-directory project)))))
    (cons 'unordered
	  (mapcar
	   (lambda (f)
	     (list (funcall format-entry
			    (file-relative-name f root)
			    project)))
	   files))))

(defun t-sitemap-find-html-title (file project)
  (let ((file (org-publish--expand-file-name file project)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (match-string 1))))

(defun t-sitemap-format (entry project)
  (let ((time (substring entry 0 10)))
    (if (string= (file-name-extension entry) "htm")
	(let ((name (string-replace ".htm" ".html" entry))
	      (title (t-sitemap-find-html-title entry project)))
	  (format "[%s] [[file:%s][%s]]"
		  time name title))
      (format "[%s] [[file:%s][%s]]"
	      time entry (org-publish-find-title entry project)))))

(defun t-sitemap ()
  (interactive)
  (let* ((project (assoc "articles" org-publish-project-alist))
	 (root (expand-file-name
		(file-name-as-directory
		 (org-publish-property :base-directory project))))
	 (sitemap-filename (expand-file-name "index.org" root)))
    (let ((files (remove sitemap-filename
			 (org-publish-get-base-files project))))
      (setq files (reverse files))
      (insert "* 目录\n"
	      (org-list-to-org
	       (t-sitemap-files-to-lisp files project 't-sitemap-format))))))

(defun t-home-sitemap-format (entry project)
  (let ((time (substring entry 0 10)))
    (if (string= (file-name-extension entry) "htm")
	(let ((name (string-replace "htm" "html" entry))
	      (title (t-sitemap-find-html-title entry project)))
	  (format "[%s] [[file:%s][%s]]"
		  time (concat "posts/" name) title))
      (format "[%s] [[file:%s][%s]]"
	      time (concat "posts/" entry) (org-publish-find-title entry project)))))

(defun t-home-sitemap ()
  (interactive)
  (let* ((project (assoc "articles" org-publish-project-alist))
	 (root (expand-file-name
		(file-name-as-directory
		 (org-publish-property :base-directory project))))
	 (sitemap-filename (expand-file-name "index.org" root)))
    (let ((files (remove sitemap-filename
			 (org-publish-get-base-files project))))
      (setq files (seq-take (reverse files) 10))
      (insert "* Recent [[./posts/index.org][Posts]]\n"
	      (org-list-to-org
	       (t-sitemap-files-to-lisp files project 't-home-sitemap-format))))))



;; Local Variables:
;; read-symbol-shorthands: (("t-" . "yynt-"))
;; End:

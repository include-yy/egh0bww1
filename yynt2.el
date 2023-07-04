;; -*- lexical-binding: t; -*-

(require 'ox-yyhtml (expand-file-name "./ox-yyhtml.el"))
(require 'htmlize (expand-file-name "./htmlize.el"))

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


(provide 'yynt2)

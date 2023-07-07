var yynt_headimg
var yynt_tailimg
var yynt_imgstate

window.onload = function(e) {
    yynt_headimg = document.getElementById('headimg')
    yynt_tailimg = document.getElementById('tailimg')
    yynt_headimg ? yynt_headimg.style.display = 'none' : null
    yynt_tailimg ? yynt_tailimg.style.display = 'none' : null
    yynt_imgstate = 0

    document.onkeydown = (e) => {
	if (e.keyCode == 89) {
	    if (yynt_imgstate == 0) {
		yynt_headimg ? yynt_headimg.style.display = 'flex' : null
		yynt_tailimg ? yynt_tailimg.style.display = '' : null
		yynt_imgstate = 1
	    } else {
		yynt_headimg ? yynt_headimg.style.display = 'none' : null
		yynt_tailimg ? yynt_tailimg.style.display = 'none' : null
		yynt_imgstate = 0
	    }
	}
    }
}

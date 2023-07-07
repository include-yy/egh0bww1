var yynt_headimg = document.getElementById('headimg')
var yynt_tailimg = document.getElementById('tailimg')
var yynt_imgstate = 0;

yynt_tailimg.style.display = 'none'
yynt_headimg.style.display = 'none'

document.onkeydown = (e) => {
    if (e.keyCode == 89) {
	if (yynt_imgstate == 0) {
	    yynt_tailimg.style.display = ''
	    yynt_headimg.style.display = ''
	    yynt_imgstate = 1
	} else {
	    yynt_tailimg.style.display = 'none'
	    yynt_headimg.style.display = 'none'
	    yynt_imgstate = 0
	}
    }
}

let yynt_imgs
let yynt_h
let yynt_t

let yynt_imgstate = 0
let yynt_loaded = false

let yynt_headimg
let yynt_tailimg

const yynt_no2yes =  function(e, action) {
    let parser = new DOMParser()
    let res = action(parser.parseFromString(e.innerHTML, 'text/html').body.firstElementChild)
    e.insertAdjacentElement('afterend', res)
}

const yynt_none = function(e) {
    e.style.display = 'none'
    return e
}
document.addEventListener('DOMContentLoaded', function() {
    yynt_imgs = document.getElementsByTagName('noscript')
    yynt_h = yynt_imgs[0]
    yynt_t = yynt_imgs[1]
})

window.onload = function (e) {
    document.onkeydown = (e) => {
	if (e.keyCode == 89) {
	    if (yynt_imgstate == 0) {
		if (yynt_loaded == false) {
		    yynt_h ? yynt_no2yes(yynt_h, yynt_none) : null
		    yynt_t ? yynt_no2yes(yynt_t, yynt_none) : null
		    yynt_headimg = document.getElementById('headimg')
		    yynt_tailimg = document.getElementById('tailimg')
		    yynt_loaded = true
		}
		yynt_headimg ? yynt_headimg.style = '' : null
		yynt_tailimg ? yynt_tailimg.style = '' : null
		yynt_imgstate = 1
	    } else {
		yynt_headimg ? yynt_headimg.style.display = 'none' : null
		yynt_tailimg ? yynt_tailimg.style.display = 'none' : null
		yynt_imgstate = 0
	    }
	}
    }
}

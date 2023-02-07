let cirno = document.getElementById("cirno")
let flag = true;

cirno.onclick = () => {
    if (flag) {
	cirno.src = "./img/cirno.gif"
	flag = false
    } else {
	cirno.src = "./img/cirno.jpg"
	flag = true
    }
}

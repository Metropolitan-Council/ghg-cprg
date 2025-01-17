// move footer iframe into actual footer

const mydiv = document.getElementById("myspecialdiv");
const navFoot = document.getElementsByClassName("footer");


function changePosition() {
   navFoot[0].insertAdjacentElement('beforebegin', mydiv);
};

window.onload = function() {
    changePosition();
};


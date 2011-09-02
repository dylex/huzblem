document.addEventListener("DOMContentLoaded", function(event) {
	var scripts = document.getElementsByTagName('script');
	for (var i = scripts.length-1; i >= 0; i--)
		scripts[i].parentNode.removeChild(scripts[i]);
}, true);
undefined;

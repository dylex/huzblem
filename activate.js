(function() {
	var a = document.activeElement;
	while (a && a != document) {
		if (typeof(a.click) === 'function')
			a.click();
		else if (typeof(a.href) === 'string')
			location = a.href;
		a = a.parentNode;
	}
})();

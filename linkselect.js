function linkselect(type, regexp) {
	var el = document.querySelector('link[rel="' + type + '"]');
	if (el)
		location = el.href;
	else {
		if (arguments.length < 2)
			regexp = RegExp("\\b" + type, "i");
		var els = document.getElementsByTagName("a");
		for (var i = 0; i < els.length; ++i)
		{
			if (els[i].text.search(regexp) > -1) {
				els[i].focus();
				break;
			}
		}
	}
}

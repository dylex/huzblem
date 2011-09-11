var huzbl = {
	links : document.getElementsByTagName("a"),
	activate : function(e) {
		while (e && e != document) {
			if (typeof(e.click) === 'function')
				e.click();
			else if (typeof(e.href) === 'string')
				location = e.href;
			e = e.parentNode;
		}
	},
	linkSelected : -1,
	linkFocus : function(n) {
		this.linkSelected = n;
		this.links[n].focus();
	},
	linkSelect : function(t, r) {
		var el = document.querySelector('link[rel=' + t + ']');
		if (el)
		        location = el.href;
		else {
		        for (var i = this.linkSelected+1; i < this.links.length; i++)
				if (r.test(this.links[i].text)) {
					this.linkFocus(i);
					break;
				}
			this.linkSelected = -1;
		}
        }
};

huzbl.linkNumber = {
	class : "huzbl_link_number",
	update : function (n) {
		var link = huzbl.links[n];
		var ln = link.firstChild;
		if (!(ln && "className" in ln && ln.className === this.class))
		{
			ln = document.createElement("span");
			ln.className = "huzbl_link_number";
			link.insertBefore(ln, link.firstChild);
		}
		ln.innerText = n+1;
	},
	show : function () {
		for (var n = 0; n < huzbl.links.length; n ++)
			this.update(n);
	},
	hide : function () {
		var l = document.getElementsByClassName(this.class);
		for (var i = l.length-1; i >= 0; i --)
			l[i].parentNode.removeChild(l[i]);
	},
	focus : function(n) {
		huzbl.linkFocus(n-1);
	},
	activate : function (n) {
		huzbl.activate(huzbl.links[n-1]);
	},
	get : function (n) {
		return huzbl.links[n-1].href;
	}
};

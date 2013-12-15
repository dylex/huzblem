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
        },
	blockTest : function(type, src) {
		var block = huzbl.block[type.toUpperCase()];
		if (!block) {
			if (huzbl.block.verbose)
				console.warn("allowing unknown block type: " + type);
			return true;
		}
		var res = block.default;
		if ("cur" in block && huzbl.domainre.test(src))
			res = block.cur;
		else if (block.src && block.src.test(src))
			res = !res;
		if (huzbl.block.verbose && (!res || res !== block.default))
			console.log((res ? "allowing" : "blocking") + " " + type + " " + src);
		return res;
	},
	addEvents : function(doc) {
		doc.addEventListener("beforeload", function(event) {
			if (!huzbl.blockTest(event.target.tagName, event.target.src)) {
				event.preventDefault();
				event.target.parentNode.removeChild(event.target);
			} else if (event.target.tagName.toUpperCase() === "IFRAME")
				huzbl.addEvents(event.target.contentDocument); /* this does not work, possibly because the document hasn't started loading yet */
		}, true);
		doc.addEventListener("DOMContentLoaded", function(event) {
			if (!huzbl.blockTest("script", doc.location)) {
				var scripts = doc.getElementsByTagName('script');
				for (var i = scripts.length-1; i >= 0; i--)
					scripts[i].parentNode.removeChild(scripts[i]);
			}
		}, true);
	},
	linkNumber : {
		class : "huzbl_link_number",
		update : function (n) {
			var link = huzbl.links[n];
			var ln = link.firstChild;
			if (!(ln && "className" in ln && ln.className === this.class))
			{
				ln = document.createElement("span");
				ln.className = this.class;
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
	}
};
huzbl.addEvents(document);

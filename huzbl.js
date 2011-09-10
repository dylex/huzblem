var huzbl = {
	hostre : /^(?:https?|ftp):\/\/(?:[^@\/?#]*@)?([^:\/?#]*)(?::\d+)?(?:[\/?#]|$)/i,
	activate : function(e) {
		while (e && e != document) {
			if (typeof(e.click) === 'function')
				e.click();
			else if (typeof(e.href) === 'string')
				location = e.href;
			e = e.parentNode;
		}
	},
	linkSelect : function(t, r) {
		var el = document.querySelector('link[rel=' + t + ']');
		if (el)
		        location = el.href;
		else {
		        var els = document.getElementsByTagName('a');
		        for (var i = 0; i < els.length; ++i)
		   	     if (r.test(els[i].text)) {
		   		     els[i].focus();
		   		     break;
		   	     }
		}
        }
};

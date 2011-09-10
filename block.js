document.addEventListener("beforeload", function(event) {
	var block = huzbl.block[event.target.tagName];
	if (!block) {
		if (huzbl.block.verbose)
			console.warn("allowing unknown block target tag: " + event.target.tagName);
		return;
	}
	var res = block.default;
	if ("cur" in block && huzbl.domainre.test(event.target.src))
		res = block.cur;
	else if ("src" in block && block.src.test(event.target.src))
		res = !res;
	if (!res)
	{
		event.preventDefault();
		event.target.parentNode.removeChild(event.target);
	}
	if (huzbl.block.verbose && (!res || res !== block.default))
		console.log((res ? "allowing" : "blocking") + " " + event.target.tagName + " " + event.target.src);
}, true);

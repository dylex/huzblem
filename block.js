document.addEventListener("beforeload", function(event) {
	var block = uzbl_block[event.target.tagName];
	if (!block) {
		console.warn("unknown block target tag: " + event.target.tagName);
		return;
	}
	var res = block.default;
	if (block.src && block.src.test(event.target.src))
		res = !res;
	if (!res)
	{
		event.preventDefault();
		event.target.parentNode.removeChild(event.target);
	}
	if (uzbl_block.verbose && (!res || res !== block.default))
		console.log((res ? "allowing" : "blocking") + " " + event.target.tagName + " " + event.target.src);
}, true);
undefined;

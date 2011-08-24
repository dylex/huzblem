#!/bin/sh

debug=true

run () {
	$debug && echo "run $*"
	echo "$*" >> "$UZBL_FIFO"
}

cmd=$1 ; shift
[ $# -gt 0 ] && shift
eval "$cmd"

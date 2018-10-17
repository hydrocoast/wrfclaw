#!/bin/bash
py_new="./data.py"
py_org="$CLAW/geoclaw/src/python/geoclaw/data.py"
py_bak=${py_org//data\.py/data\.py_org}

if [ -z "$1" ]; then
    	echo "copy $py_new to $py_org"
	if test ! -e $py_bak; then
		echo original data.py was renamed to data.py_org
		cp -p $py_org $py_bak
	fi
        cp -p $py_new $py_org

elif [ "$1" = "original" ]; then
	echo "let data.py original"
	cp -p $py_bak $py_org
else
	echo "Nothing to ne done"
fi

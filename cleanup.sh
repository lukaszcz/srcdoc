#!/bin/bash

if [ $1 ] && [ $1 = "noprompt" ]; then
    REPLY="yes"
else
    echo "This script will remove all files with extensions other than"
    echo ".pas or .inc or .sh or with names other than ''Makefile'' or"
    echo "''Makefile.fpc'' or ''INSTALL'' or ''LICENSE'', etc."
    echo "It will leave the docs and versions directory intact."
    echo "Do you want to proceed? (yes/no) "
    read
fi

if [ "$REPLY" = "yes" ]; then
    rm `ls -1 | grep '~'`;
    rm -r `ls -1 | egrep -v '(\.pas$)|(\.inc$)|(\.sh$)|(Makefile)|(Makefile\.fpc)|(INSTALL)|(README)|(LICENSE)|(COPYING)|(NEWS)|(^tests$)|(^docs$)|(^docsrc$)|(versions)|(^\.backup\.bpl$)'`;
    cd docsrc
    rm `ls -1 | grep '~'`;
fi



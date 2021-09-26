#!/bin/sh

# srcdoc package creation script

BIN_PACKAGE_FILES="srcdoc COPYING LICENSE NEWS README"

if [ $1 ]; then
    PACKAGE=srcdoc-$1
    SRC_PACKAGE=${PACKAGE}-src
    BIN_PACKAGE=${PACKAGE}
    DOCS_PACKAGE=${PACKAGE}-docs
    SRC_FILE=${PACKAGE}.src.tar.gz
    BIN_FILE=${PACKAGE}.bin.i386-lin.tar.bz2
    DOCS_FILE=${PACKAGE}.docs.tar.bz2

    make clean
    make cleandocs

    ./backup.sh
    mv srcdoc_save $SRC_PACKAGE
    tar -czf $SRC_FILE ./$SRC_PACKAGE
    cp $SRC_FILE ./versions/
    cp $SRC_FILE ~/html/homepage/srcdoc/download/

    make docs
    cp docs ~/html/homepage/srcdoc/docs/
    mv docs $DOCS_PACKAGE
    tar -cjf $DOCS_FILE ./$DOCS_PACKAGE
    cp $DOCS_FILE ./versions/
    cp $DOCS_FILE ~/html/homepage/srcdoc/download/

    make smart
    mkdir $BIN_PACKAGE
    cp $BIN_PACKAGE_FILES $BIN_PACKAGE
    tar -cjf $BIN_FILE ./$BIN_PACKAGE
    cp $BIN_FILE ./versions/
    cp $BIN_FILE ~/html/homepage/srcdoc/download/

    ./srcdoc -db -p devel -q -o ~/html/homepage/srcdoc/sample/ srcdoc.pas
else
    echo "usage: package.sh <version number>"
fi

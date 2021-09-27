#!/bin/bash
                  ##############################
                  # SrcDoc installation script #
                  ##############################

# VERSION is the number of the library version with nothing prepended
# or appended

VERSION="0.1"

# VERSION_SUFFIX must have a dash ("-") prepended and nothing should be
# appended to the version number 

VERSION_SUFFIX="-$VERSION"

function checked_eval()
{
    echo $*
    if ! eval $* ; then
	echo "install.sh: Error - exiting"
	exit 3
    fi
}

# checks whether $1 command is available
function check_present()
{
    eval "$1 --version >/dev/null 2>&1"
    if [ $? != 127 ]; then
	return 0;
    else
	return 1;
    fi;
}

# processes a directory name in $REPLY; appends a slash if not already present
function process_dirname()
{
    REPLY=`eval 'echo -n '$REPLY` # to have path/tidle expansion work
    if [ ! ${REPLY:${#REPLY}-1:1} == '/' ]; then
	REPLY=$REPLY/
    fi
}

if check_present docbook2html; then
    DOCBOOK2HTML_PRESENT=1
else
    DOCBOOK2HTML_PRESENT=0
fi

if check_present docbook2pdf; then
    DOCBOOK2PDF_PRESENT=1
else
    DOCBOOK2PDF_PRESENT=0
fi

echo
echo "SrcDoc $VERSION installation script."
echo
echo "See the INSTALL file first."
echo

read -e -p "Enter the directory with PascalAdt library binaries (default /usr/local/lib/): "
if [ $REPLY ]; then
    process_dirname
    LIB_DIR=$REPLY
else
    LIB_DIR="/usr/local/lib/"
fi

read -e -p "Enter the directory with PascalAdt .ppu files (default /usr/local/include/): "
if [ $REPLY ]; then
    process_dirname
    INCLUDE_DIR=$REPLY
else
    INCLUDE_DIR="/usr/local/include/"
fi

read -e -p "Enter the target binary directory (default /usr/local/bin/): "
if [ $REPLY ]; then
    process_dirname
    BIN_DIR=$REPLY
else
    BIN_DIR="/usr/local/bin/"
fi

MAKE_HTML=0
if [ $DOCBOOK2HTML_PRESENT == 1 ]; then
    read -n 1 -p "Make HTML documentation? (y/n) "
    echo
    if [ "$REPLY" != "n" ]; then
	MAKE_HTML=1
    fi
fi

MAKE_PDF=0
if [ $DOCBOOK2PDF_PRESENT == 1 ]; then
    read -n 1 -p "Make PDF documentation? (n/y) "
    echo
    if [ "$REPLY" == "y" ]; then
	MAKE_PDF=1
    fi
fi

if [ $MAKE_HTML == 1 -o $MAKE_PDF == 1 ]; then
    read -e -p "Enter the target documentation directory (default /usr/local/doc/srcdoc/): "
    if [ $REPLY ]; then
	process_dirname
	DOCS_DIR=$REPLY
    else
	DOCS_DIR="/usr/local/doc/srcdoc"
    fi
fi

checked_eval make $1 INCLUDE_DIR=$INCLUDE_DIR LIB_DIR=$LIB_DIR

install srcdoc $BIN_DIR/srcdoc

if [ ! -d $DOCS_DIR ]; then
    checked_eval mkdir -p $DOCS_DIR
fi

if [ \( ! -d ${DOCS_DIR}html/ \) -a \( $MAKE_HTML == 1 \) ]; then
    checked_eval mkdir -p ${DOCS_DIR}html/
fi

if [ \( ! -d ${DOCS_DIR}pdf/ \) -a \( $MAKE_PDF == 1 \) ]; then
    checked_eval mkdir -p ${DOCS_DIR}pdf/
fi

if [ $MAKE_HTML == 1 ]; then
    checked_eval docbook2html -o ${DOCS_DIR}html/ docsrc/srcdoc.sgml
fi

if [ $MAKE_PDF == 1 ]; then
    checked_eval docbook2pdf -o ${DOCS_DIR}pdf/ docsrc/srcdoc.sgml
fi

echo
echo "Installation finished."
echo
echo "Documentation is available at https://srcdoc.github.io"


ifneq ($(findstring debug, $(MAKECMDGOALS)),)
DEBUG = true
INCLUDE_DIR = /usr/local/include/debug/
LIB_DIR = /usr/local/lib/debug/
else
INCLUDE_DIR = /usr/local/include/
LIB_DIR = /usr/local/lib/
endif

override OPTS += -Si -S2 -Sh -XD -Fu$(INCLUDE_DIR) -Fl$(LIB_DIR)

ifdef DEBUG
override OPTS += -vewn -Sa -g -gl -dDEBUG
else
override OPTS += -Ur -O3 -Xs
endif

ifdef WINDOWS
override OPTS += -Twin32
endif

obj_suffix := .o
prog_suffix := 

UNITS := $(patsubst %.pas, %$(obj_suffix), $(filter-out srcdoc.pas, $(wildcard *.pas)))

.PHONY : all units debug docs clean cleandocs

all : srcdoc$(prog_suffix)

srcdoc$(prog_suffix) : srcdoc.pas $(UNITS)

units : $(UNITS)

debug : all

docs :
	mkdir -p docs/html
	mkdir -p docs/pdf
	-docbook2html -o docs/html docsrc/srcdoc.sgml
	-docbook2pdf -o docs/pdf docsrc/srcdoc.sgml

clean :
	-./cleanup.sh noprompt > /dev/null 2>&1

cleandocs :
	-rm -r docs

%$(prog_suffix) : %.pas
	fpc $(OPTS) -o$@ $<

%$(obj_suffix) : %.pas
	fpc $(OPTS) -o$@ $<


ifneq ($(findstring test, $(MAKECMDGOALS)),)
TEST = true
DEBUG = true
endif

ifneq ($(findstring tests, $(MAKECMDGOALS)),)
TEST = true
DEBUG = true
endif

ifneq ($(findstring debug, $(MAKECMDGOALS)),)
DEBUG = true
INCLUDE_DIR = /usr/local/include/debug/
LIB_DIR = /usr/local/lib/debug/
else
INCLUDE_DIR = /usr/local/include/
LIB_DIR = /usr/local/lib/
endif

ifneq ($(findstring windows, $(MAKECMDGOALS)),)
WINDOWS = true
endif

ifneq ($(findstring dynamic, $(MAKECMDGOALS)),)
DYNAMIC = true
endif

ifneq ($(findstring smart, $(MAKECMDGOALS)),)
SMART = true
endif

override OPTS += -XD -Si -S2 -Sh -Fu$(INCLUDE_DIR) -Fl$(LIB_DIR)

ifdef DEBUG
override OPTS += -vewn -Sa -gl -gc -gh -dDEBUG
else
override OPTS += -Ur -O3 -Xs
endif

ifdef DYNAMIC
override OPTS += -XD
endif

ifdef SMART
override OPTS += -XX
endif

ifdef WINDOWS
override OPTS += -Twin32
endif

obj_suffix := .o
prog_suffix := 

UNITS := $(patsubst %.pas, %$(obj_suffix), $(filter-out srcdoc.pas, $(wildcard *.pas)))

.PHONY : all static dynamic smart units debug windows docs clean cleandocs

all : srcdoc$(prog_suffix)

static : all

dynamic : all

smart : all

srcdoc$(prog_suffix) : srcdoc.pas $(UNITS)

units : $(UNITS)

debug : all

windows :
	./compile_windows.sh

docs :
	docbook2html -o docs/ docsrc/srcdoc.sgml
	docbook2pdf -o docs/ docsrc/srcdoc.sgml

clean :
	-./cleanup.sh noprompt > /dev/null 2>&1

cleandocs :
	-rm -r docs

%$(prog_suffix) : %.pas
	fpc $(OPTS) -o$@ $<

%$(obj_suffix) : %.pas
	fpc $(OPTS) -o$@ $<


targets = ipc2008-sat ipc2011-sat ipc2014-sat ipc2008-opt ipc2011-opt ipc2014-opt ipc2006-optsat zerocost-opt aaai16-opt ipc2014-agl ipc2014-mco misc-adl misc-strips

default: help

help:
	@echo possible targets:
	@(for x in $(targets) ; do if [ -d $$x ] ; then echo "$$x ✔" ; else echo "$$x" ; fi ; done )
	@echo all --- all of them

all: $(targets)

%sat:
	git clone https://github.com/guicho271828/$@.git
%opt:
	git clone https://github.com/guicho271828/$@.git
%agl:
	git clone https://github.com/guicho271828/$@.git
%mco:
	git clone https://github.com/guicho271828/$@.git
%adl:
	git clone https://github.com/guicho271828/$@.git
%strips:
	git clone https://github.com/guicho271828/$@.git


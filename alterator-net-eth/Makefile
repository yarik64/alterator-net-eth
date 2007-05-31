NAME=net-eth
DESCRIPTION="IP interfaces"

INSTALL=/usr/bin/install

all:  build-tools
clean: clean-po 
install: install-backend install-ui install-po  install-tools install-html

include /usr/share/alterator/build/po.mak
include /usr/share/alterator/build/ui2.mak
include /usr/share/alterator/build/backend.mak
include /usr/share/alterator/build/fbi.mak
include /usr/share/alterator/build/html-messages.mak

HTML_PO_TEMPLATE=ui/net-eth/html-messages.scm

build-tools:
	make -C tools/maskname

install-tools:
	make -C tools/maskname bindir=$(bindir) install
	install -Dpm755 tools/ifaceinfo $(bindir)/ifaceinfo

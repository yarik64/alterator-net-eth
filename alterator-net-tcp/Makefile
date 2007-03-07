NAME=net-tcp
DESCRIPTION="IP interfaces"

INSTALL=/usr/bin/install

all: build-profile build-tools
clean: clean-po clean-profile
install: install-backend install-ui install-po install-scripts install-profile install-tools install-html

include /usr/share/alterator/build/po.mak
include /usr/share/alterator/build/ui2.mak
include /usr/share/alterator/build/backend.mak
include /usr/share/alterator/build/standalone.mak
include /usr/share/alterator/build/fbi.mak

build-tools:
	make -C tools/maskname

install-tools:
	make -C tools/maskname bindir=$(bindir) install
	install -Dpm755 tools/ifaceinfo $(bindir)/ifaceinfo

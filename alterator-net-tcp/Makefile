NAME=net-tcp
DESCRIPTION="TCP/IP connections"

INSTALL=/usr/bin/install

all: build-profile build-tools
clean: clean-po clean-profile
install: install-backend install-ui install-po install-scripts install-profile install-tools

include /usr/share/alterator/build/po.mak
include /usr/share/alterator/build/ui2.mak
include /usr/share/alterator/build/backend.mak
include /usr/share/alterator/build/standalone.mak

build-tools:
	make -C tools

install-tools:
	make -C tools bindir=$(bindir) install

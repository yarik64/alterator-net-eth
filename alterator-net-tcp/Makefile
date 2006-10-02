NAME=net-tcp
DESCRIPTION="TCP/IP connections"

INSTALL=/usr/bin/install

all: build-profile
clean: clean-po clean-profile
install: install-backend install-ui install-po install-scripts install-profile

include /usr/share/alterator/build/po.mak
include /usr/share/alterator/build/ui2.mak
include /usr/share/alterator/build/backend.mak
include /usr/share/alterator/build/standalone.mak

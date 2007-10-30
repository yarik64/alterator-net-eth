NAME=net-eth
DESCRIPTION="IP interfaces"

INSTALL=/usr/bin/install

all:
clean: clean-po 
install: install-backend install-ui install-po install-html

include /usr/share/alterator/build/po.mak
include /usr/share/alterator/build/ui2.mak
include /usr/share/alterator/build/backend.mak
include /usr/share/alterator/build/fbi.mak
include /usr/share/alterator/build/html-messages.mak

HTML_PO_TEMPLATE=ui/net-eth/html-messages.scm

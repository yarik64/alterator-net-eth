NAME=net-eth
INSTALL=/usr/bin/install

all:
clean: clean-po
install: install-module install-data

include /usr/share/alterator/build/module.mak

install-data:
	install -d $(datadir)/install2
	cp -a preinstall.d $(datadir)/install2
	install -d $(libexecdir)/alterator/hooks/net-eth.d

NAME=net-eth
INSTALL=/usr/bin/install

all:
clean: clean-po
install: install-module install-data

include /usr/share/alterator/build/module.mak

install-data:
	install -d $(datadir)/install2
	install -d $(libexecdir)/alterator/hooks/net-eth.d
	install -d $(libexecdir)/alterator/hooks/net-eth-precommit.d
	install -Dm0755 hook/hosts-hook-hostname ${sysconfdir}/hooks/hostname.d/05-hosts

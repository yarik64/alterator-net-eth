#!/bin/sh

. install2-init-functions

case "$METHOD" in
	cdrom|disk) exit 0 ;;
esac

run_chroot alterator-cmdline -l /net-eth action "write" \
				      commit '#t' \
				      hostname "${HOSTNAME:-localhost.localdomain}" \
				      search "${DOMAINNAME:-}" \
				      dns "${DNS_SERVER:-}${DNS_SERVER2:+ $DNS_SERVER2}"

iface="$(exec_chroot ip link show up |
	sed -n -e '/^[[:digit:]]\+:/!d' -e '/^[[:digit:]]\+: lo:/d' -e 's/^[[:digit:]]\+: \([^:]\+\):.*/\1/p' -e 'q')"

if [ -n "$iface" ]; then
    case "${BOOTPROTO:-}" in
	static)
		run_chroot alterator-cmdline -l "/net-eth" action "write" \
							commit '#t' \
							name "$iface" \
							controlled 'etcnet' \
							configuration 'static' \
							ip "${IPADDR:-}" \
							mask "${NETBITS:-}" \
							default "${GATEWAY:-}"
		;;
	dhcp)
		run_chroot alterator-cmdline -l "/net-eth" action "write" \
							commit '#t' \
							name "$iface" \
							controlled 'etcnet' \
							configuration 'dhcp'
		;;
    esac
fi

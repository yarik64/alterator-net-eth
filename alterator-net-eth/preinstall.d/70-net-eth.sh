#!/bin/sh

. install2-init-functions

case "$METHOD" in
	cdrom|disk) exit 0 ;;
esac

run_chroot alterator-cmdline -l /net-eth action "write" \
				      commit '#t' \
				      hostname "${HOSTNAME:-localhost.localdomain}"

iface="$(exec_chroot /sbin/ip -o addr show up|
	sed -n -e 's/[[:digit:]]:[[:space:]]\+\([[:alnum:]]\+\)[[:space:]]\+inet[[:space:]].*/\1/p'|
	grep -v lo)"

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
							default "${GATEWAY:-}" \
							search "${DOMAINNAME:-}" \
							dns "${DNS_SERVER:-}${DNS_SERVER2:+ $DNS_SERVER2}"
		;;
	dhcp)
		run_chroot alterator-cmdline -l "/net-eth" action "write" \
							commit '#t' \
							name "$iface" \
							controlled 'etcnet' \
							configuration 'dhcp' \
							search "${DOMAINNAME:-}" \
							dns "${DNS_SERVER:-}${DNS_SERVER2:+ $DNS_SERVER2}"
		;;
    esac
fi

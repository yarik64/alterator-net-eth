(document:surround "/std/base")
(document:insert "/std/functions")

(document:envelop with-container-presentations ((netmask '/net-tcp/netmask text)) )

width 600
height 500



(define iface (woo-read-first "/net-tcp/eth0"))


(document:id i (edit (woo-get-option iface 'ip)))
(document:id n (netmask (woo-get-option iface 'mask)))



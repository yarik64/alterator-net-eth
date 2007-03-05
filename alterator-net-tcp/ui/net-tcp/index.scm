(document:surround "/std/frame")
(document:insert "/std/functions")

(document:insert "/net-tcp/common-functions")
(document:insert "/net-tcp/base-functions")
(document:insert "/net-tcp/install-functions")

(document:envelop with-translation _ "alterator-net-tcp")

;;;;;;;;;;;;;;;;;;;;;;;;;

(gridbox
 columns "20;20;40;20"
 ;;
 (spacer)
 (label (_ "Interface") align "right")
 (document:id ifaces (combobox layout-policy 20 -1))
 (spacer)

 (spacer)
 (spacer)
 (document:id w-button (button (_ "Wireless settings")))
 (spacer)
 ;;
 (spacer)
 (document:id iface-enabled (checkbox (_ "Interface is enabled") widget-name "state"))
 (spacer)
 (spacer)
 ;;
 (spacer)
 (document:id iface-dhcp (checkbox (_ "Use DHCP") widget-name "dhcp"))
 (spacer)
 (spacer)
 ;;
 (spacer)
 (label (_ "IP address") align "right")
 (document:id iface-ip (edit "" widget-name "ip"))
 (spacer)
 ;;
 (spacer)
 (label (_ "Netmask") align "right")
 (document:id iface-mask (combobox "" rows (map cdr avail-masks) widget-name "mask"))
 (spacer)
 ;;
 (spacer)
 (label (_ "Default gateway") align "right")
 (document:id iface-gw (edit "" widget-name "default"))
 (spacer))

;;;;;;;;;;;;;;;

(or (global 'frame:next)
    (hbox align "center"
          (document:id c-button (button (_ "Commit")))
          (document:id r-button (button (_ "Reset")))
          (document:id q-button (button (_ "Quit")))))

;;;;;;;;;;;;;;;;;;
(common-behaviour)

(if (global 'frame:next) (install-behaviour) (base-behaviour))

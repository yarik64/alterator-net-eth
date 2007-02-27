(document:surround "/std/frame")
(document:insert "/std/functions")

(document:insert "/net-tcp/common-functions")
(document:insert "/net-tcp/base-functions")
(document:insert "/net-tcp/install-functions")

(document:envelop with-translation _ "alterator-net-tcp")

;;;;;;;;;;;;;;;;;;;;;;;;;

(hbox
 align "center"
 (document:id g-button (button (_ "General network settings")))
 (document:id w-button (button (_ "Wireless settings"))))

(gridbox
 columns "20;20;40;20"
 ;;
 (spacer)
 (label (_ "Interface"))
 (document:id ifaces (combobox layout-policy 20 -1))
 (spacer)
 ;;
 (spacer)
 (document:id iface-enabled (checkbox (_ "Enabled") widget-name "state"))
 (spacer)
 (spacer)
 ;;
 (spacer)
 (document:id iface-dhcp (checkbox (_ "Use DHCP") widget-name "dhcp"))
 (spacer)
 (spacer)
 ;;
 (spacer)
 (label (_ "IP address"))
 (document:id iface-ip (edit "" widget-name "ip"))
 (spacer)
 ;;
 (spacer)
 (label (_ "Netmask"))
 (document:id iface-mask (combobox "" rows (map cdr avail-masks) widget-name "mask"))
 (spacer)
 ;;
 (spacer)
 (label (_ "Default gateway"))
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

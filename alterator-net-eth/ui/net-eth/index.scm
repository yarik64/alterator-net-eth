(document:surround "/std/frame")
(document:insert "/std/functions")

(document:insert "/net-eth/common-functions")
(document:insert "/net-eth/base-functions")
(document:insert "/net-eth/install-functions")

(document:envelop with-translation _ "alterator-net-eth")

;;;;;;;;;;;;;;;;;;;;;;;;;

(gridbox
 columns "20;20;40;20"
 ;;
 (spacer)
 (label (_ "Interface") align "right")
 (document:id ifaces (combobox layout-policy 20 -1))
 (spacer)

 ;;
 (spacer)
 (spacer)
 (document:id iface-info (label ""))
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
 (spacer)

 ;;
 (spacer)
 (label (_ "Hardware binding") align "right")
 (document:id iface-hw-binding (combobox "" rows (map cdr avail-hw-bindings) widget-name "hw_binding"))

 ;;
 (spacer)
 (spacer)
 (document:id w-button (button (_ "Wireless settings")))
 (spacer)
 
 )

;;;;;;;;;;;;;;;

(or (global 'frame:next)
    (hbox align "center"
          (document:id c-button (button (_ "Apply")))
          (document:id r-button (button (_ "Reset")))
          (document:id q-button (button (_ "Quit")))))

;;;;;;;;;;;;;;;;;;
(common-behaviour)

(if (global 'frame:next) (install-behaviour) (base-behaviour))

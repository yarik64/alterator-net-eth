(document:surround "/std/base")
(document:insert "/std/functions")

(document:envelop with-container-presentations ((netmask '/net-tcp/mask text)) )

width 600
height 400

spacing 5
margin 10

(hbox
 layout-policy 100 -1
 spacing 2
 (label "Interface:")
 (document:id ifaces (combobox layout-policy 20 -1)))


(document:id iface-enabled (checkbox "Enabled" widget-name "state"))
(document:id iface-dhcp (checkbox "Use DHCP" widget-name "dhcp"))

(hbox
 layout-policy 100 -1
 (label "IP address" layout-policy 30 -1)
 (document:id iface-ip (edit "" layout-policy -2 -1 widget-name "ip")))

(hbox
 layout-policy 100 -1
 (label "NetMask" layout-policy 30 -1)
 (document:id iface-mask (netmask "" layout-policy -2 -1 widget-name "mask")))

(hbox
 layout-policy 100 -1
 (label "Default gateway" layout-policy 30 -1)
 (document:id iface-gw (edit "" layout-policy -2 -1 widget-name "default")))

(vertical-spacer)
(hbox layout-policy 100 -1
      spacing 10
      (document:id c-button (button "Commit" layout-policy 33 -1))
      (document:id r-button (button "Reset"  layout-policy 33 -1))
      (or (global 'frame:next)
          (document:id q-button (button "Quit" layout-policy -2 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-interface name)
  (and (not-empty-string? name)
       (let ((cmd (woo-read-first (string-append "/net-tcp" "/" name))))
         
         (iface-enabled state (woo-get-option cmd 'state #f))
         (iface-dhcp state (woo-get-option cmd 'dhcp #f))
         (iface-ip text (woo-get-option cmd 'ip))
         (iface-mask text (woo-get-option cmd 'mask))
         (iface-gw text (woo-get-option cmd 'default)))))


(define (commit-interface name)
  (and (not-empty-string? name)
       (woo-catch/message
        (thunk
         (woo-write/constraints (string-append "/net-tcp" "/" name)
                                'state (iface-enabled state)
                                'dhcp  (iface-dhcp state)
                                'ip    (iface-ip text)
                                'mask  (iface-mask text)
                                'default (iface-gw text))))))

(ifaces header (vector"Network interfaces")
        rows (woo-list-names "/net-tcp")
        (when selected (update-interface (ifaces text))))

(c-button (when clicked (commit-interface (ifaces text))))
(r-button (when clicked (update-interface (ifaces text))))
(or (global 'frame:next)
    (q-button (when clicked (document:end))))

;;init first update
(ifaces current 0 selected)

(document:root
 (when loaded
   (update-constraints "write" "/net-tcp")))


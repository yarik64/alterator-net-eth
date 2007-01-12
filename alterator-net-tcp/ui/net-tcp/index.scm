(document:surround "/std/frame")
(document:insert "/std/functions")

(document:envelop with-translation _ "alterator-net-tcp")

width 600
height 400

spacing 5
margin 10

(define avail-ifaces (woo-list-names "/net-tcp"))
(define avail-masks (woo-list-names "/net-tcp/eth0/avail_masks"))

(hbox
 layout-policy 100 -1
 spacing 2
 (label (_"Interface:"))
 (document:id ifaces (combobox layout-policy 20 -1)))

(document:id iface-enabled (checkbox (_ "Enabled") widget-name "state"))
(document:id iface-dhcp (checkbox (_ "Use DHCP") widget-name "dhcp"))

(define (field x y)
  (y layout-policy -2 -1)
  (hbox layout-policy 100 -1
        (label x layout-policy 50 -1)
        y))

(field
 (_ "IP address")
 (document:id iface-ip (edit "" widget-name "ip")))

(field
 (_ "NetMask")
 (document:id iface-mask (combobox "" rows avail-masks widget-name "mask")))

(field
 (_ "Default gateway")
 (document:id iface-gw (edit "" widget-name "default")))

(vertical-spacer)
(or (global 'frame:next)
    (hbox layout-policy 100 -1
          spacing 10
          (document:id c-button (button (_ "Commit") layout-policy 33 -1))
          (document:id r-button (button (_ "Reset")  layout-policy 33 -1))
          (document:id q-button (button (_ "Quit") layout-policy -2 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-interface name)
  (and (not-empty-string? name)
       (let ((cmd (woo-read-first (string-append "/net-tcp" "/" name))))
         
         (iface-enabled state (woo-get-option cmd 'state #f))
         (iface-dhcp state (woo-get-option cmd 'dhcp #f))
         (iface-ip text (woo-get-option cmd 'ip))
         (iface-mask current (or (string-list-index (woo-get-option cmd 'mask) avail-masks)
                                 0))
         (iface-gw text (woo-get-option cmd 'default)))))


(define (commit-interface name)
  (or (string-null? name)
       (begin
         (splash-message (_ "Restarting network..."))
         (document:release)
         (splash-message)
         (woo-catch/message
          (thunk
           (woo-write/constraints (string-append "/net-tcp" "/" name)
                                  'state (iface-enabled state)
                                  'dhcp  (iface-dhcp state)
                                  'ip    (iface-ip text)
                                  'mask  (iface-mask text)
                                  'default (iface-gw text)))))))



;;common behaviour
(ifaces header (vector (_ "Network interfaces"))
        rows avail-ifaces
        (when selected (update-interface (ifaces text))))
(ifaces current 0 selected)
(document:root
 (when loaded
   (update-constraints "write" "/net-tcp")))

;;standalone specific behaviour
(or (global 'frame:next)
    (begin
      (c-button (when clicked (commit-interface (ifaces text))))
      (r-button (when clicked (update-interface (ifaces text))))
      (q-button (when clicked (document:end)))))

;;installer specific behaviour
(and (global 'frame:next)
     (begin (iface-dhcp state #t)
            (document:root (when loaded
                             (if (null? avail-ifaces)
                                 (if (eq? (global 'frame:direction) 'next)
                                     (frame:next)
                                     (frame:back)))))))
(frame:on-next (thunk (commit-interface (ifaces text))))

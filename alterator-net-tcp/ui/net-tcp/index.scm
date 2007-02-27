(document:surround "/std/frame")
(document:insert "/std/functions")

(document:envelop with-translation _ "alterator-net-tcp")

(define prev-current (make-cell 0))

width 600
height 400

spacing 5
margin 10

(define (name+label x)
  (cons (woo-get-option x 'name)
        (woo-get-option x 'label)))

(define avail-ifaces (map name+label (woo-list "/net-tcp")))
(define avail-masks (map name+label (woo-list "/net-tcp/eth0/avail_masks")))

(hbox
 align "center"
 (button (_ "General network settings")
         (when clicked (and (restart-interfaces)
			    (frame:replace "/net-general"))))
 (document:id w-button (button (_ "Wireless settings")
			       (when clicked (frame:replace "/net-wifi")))))

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

(or (global 'frame:next)
    (hbox align "center"
          (document:id c-button (button (_ "Commit") layout-policy 33 -1))
          (document:id r-button (button (_ "Reset")  layout-policy 33 -1))
          (document:id s-button (button (_ "Restart network")  layout-policy 33 -1))
          (document:id q-button (button (_ "Quit") layout-policy -2 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prev-interface)
  (car (list-ref avail-ifaces (cell-ref prev-current))))

(define (current-interface)
  (car (list-ref avail-ifaces (ifaces current))))

(define (current-mask)
  (car (list-ref avail-masks (iface-mask current))))

(define (restart-interfaces)
  (commit-interface (prev-interface))
  (splash-message (_ "Restarting network..."))
  (document:release)
  (splash-message)
  (woo-catch/message (thunk (and  (woo-try "restart" "/net-tcp")
				  (woo-try "restart" "/autoinstall/net-tcp")))))

(define (update-interface name)
  (and (not-empty-string? name)
       (let ((cmd (woo-read-first (string-append "/net-tcp" "/" name))))
         (iface-enabled state (woo-get-option cmd 'state #f))
         (iface-dhcp state (woo-get-option cmd 'dhcp #f))
         (iface-ip text (woo-get-option cmd 'ip))
         (iface-mask current (or (string-list-index (woo-get-option cmd 'mask) (map car avail-masks))
                                 24))
         (w-button activity (woo-get-option cmd 'wireless))
         (iface-gw text (woo-get-option cmd 'default)))))

(define (commit-interface name)
  (or (string-null? name)
      (begin
	(woo-catch/message
	 (thunk
	  (woo-write/constraints (string-append "/net-tcp" "/" name)
				 'state (iface-enabled state)
				 'dhcp  (iface-dhcp state)
				 'ip    (iface-ip text)
				 'mask  (current-mask)
				 'default (iface-gw text))
	  (and (global 'frame:next)
	       (woo-write (string-append "/autoinstall/net-tcp" "/" name)
                          'state (iface-enabled state)
                          'dhcp  (iface-dhcp state)
                          'ip    (iface-ip text)
                          'mask  (current-mask)
                          'default (iface-gw text))))))))

;;common behaviour
(ifaces header (vector (_ "Network interfaces"))
        rows (map cdr avail-ifaces)
        (when selected (and (commit-interface (prev-interface))
			    (update-interface (current-interface))
			    (cell-set! prev-current (ifaces current)))))

(and (positive? (ifaces count))
     (begin (ifaces current 0)
	    (update-interface (current-interface))))

(document:root
 (when loaded
       (update-constraints "write" "/net-tcp")))

(if (global 'frame:next)
    (begin
      (frame:on-back (thunk (restart-interfaces) (frame:replace "/net-general") 'cancel))
      (frame:on-next (thunk (restart-interfaces) (frame:replace "/net-general") 'cancel)))
    (begin (c-button (when clicked (commit-interface (current-interface))))
           (r-button (when clicked (update-interface (current-interface))))
           (s-button (when clicked (restart-interfaces)))
           (q-button (when clicked (document:end)))))

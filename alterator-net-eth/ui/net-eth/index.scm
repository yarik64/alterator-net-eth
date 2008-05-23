(document:surround "/std/frame")
(document:insert "/std/functions")

(document:envelop with-translation _ "alterator-net-eth")

;;; Functions

(define *prev-current* (make-cell 0))

(define (read-interface name)
  (and (string? name)
       (let ((cmd (woo-read-first "/net-eth" 'ifname name)))

         (hostname value (woo-get-option cmd 'hostname))
         (dns value (woo-get-option cmd 'dns))
         (search value (woo-get-option cmd 'search))

	 (iface-info text (woo-get-option cmd 'info))
	 (iface-ip value (woo-get-option cmd 'ip))
	 (iface-mask value (woo-get-option cmd 'mask))
	 (iface-gw text (woo-get-option cmd 'default))
	 (iface-hw-binding value (woo-get-option cmd 'hw_binding))

	 (iface-configuration value (woo-get-option cmd 'configuration))
	 (iface-configuration selected)

	 (w-button activity (woo-get-option cmd 'wireless)))))

(define (write-interface path name)
    (and (string? name)
	 (woo-write/constraints
	   path
	   'hostname (hostname value)
	   'dns (dns value)
	   'search (search value)

	   'ifname name
	   'ip    (iface-ip value)
	   'mask  (iface-mask value)
	   'hw_binding (iface-hw-binding value)
	   'configuration (iface-configuration value)
	   'default (iface-gw value))))

(define (commit-interface)
  (let ((name (or (ifaces value) "")))
    (woo-catch/message
      (thunk
       (write-interface "/net-eth/" name)
       (woo-write "/net-eth" 'commit #t)))))

(define (reset-interface)
  (woo-catch/message
    (thunk
      (woo-write "/net-eth" 'reset #t)
      (and (global 'frame:next) (woo-write "/net-eth" 'reset #t))

      (iface-mask enumref "/net-eth/avail_masks")
      (iface-hw-binding enumref "/net-eth/avail_hw_bindings")
      (iface-configuration enumref "/net-eth/avail_configurations")
      (ifaces enumref "/net-eth/avail_ifaces")

      (read-interface "")
      (cell-set! *prev-current* "")
      (or (zero? (ifaces count)) (ifaces current 0)))))

;;; UI

(gridbox
  columns "0;100"
  margin 20

  (gridbox
    colspan 2
    columns "0;40;60"

    ;;
    (label text (_ "Host name:") align "right")
    (document:id hostname (edit))
    (spacer)

    ;;
     (label colspan 3)

     ;;
     (label text (_ "DNS servers:") align "right")
     (document:id dns (edit))
     (spacer)

     ;;
     (label text (_ "Search domains:") align "right")
     (document:id search (edit))
     (spacer)

     (spacer)
     (label text (string-append "<small>("
				(_ "multiple values should be space separated")
				")<small>"))
     (spacer)
     )


  (separator colspan 2)

  (label text (bold (_ "Interfaces")))
  (spacer)

  (document:id ifaces (listbox
			(when selected
			  (or (woo-catch/message
				(thunk
				  (write-interface "/net-eth" (cell-ref *prev-current*))
				  (read-interface (ifaces value))
				  (cell-set! *prev-current* (ifaces value))))
			      (ifaces value (cell-ref *prev-current*))))))
  (gridbox
    columns "0;100"
    ;;
    (label text (_ "Status:") align "right")
    (document:id iface-info (label))

    ;;
    (label text (_ "Configuration:") align "right")
    (document:id iface-configuration (combobox
				       (when selected
					 ((widgets iface-ip
						   iface-mask
						   iface-gw)
					  activity (string=? (iface-configuration value) "static")))))

    ;;
    (label text (_ "IP address:") align "right")
    (document:id iface-ip (edit))

    ;;
    (label text (_ "Netmask:") align "right")
    (document:id iface-mask (combobox))

    ;;
    (label text (_ "Default gateway:") align "right")
    (document:id iface-gw (edit))

    ;;
    (label text (_ "Hardware binding:") align "right")
    (document:id iface-hw-binding (combobox))

    ;;
    (spacer)
    (document:id w-button (button text (_ "Wireless settings...")
				  align "left"
				  activity #f
				  (when clicked
				    (let ((name (ifaces value)))
				      (and (not-empty-string? name)
					   (document:popup "/net-wifi/" 'interface name))))))


    )

    ;;
    (label colspan 2)

    ;;
    (if (global 'frame:next)
      (label)
      (hbox align "left"
	    (button (_ "Apply") (when clicked (commit-interface)))
	    (button (_ "Reset") (when clicked (reset-interface)))))
    (spacer)
    )


;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;

(document:root
  (when loaded (reset-interface)))

(frame:on-back (thunk (or (commit-interface) 'cancel)))
(frame:on-next (thunk (or (commit-interface) 'cancel)))

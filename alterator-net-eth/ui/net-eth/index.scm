(document:surround "/std/frame")
(document:insert "/std/functions")

(document:envelop with-translation _ "alterator-net-eth")

;;; Functions

(define *avail-ifaces* (make-cell '()))
(define *avail-masks* (make-cell '()))
(define *avail-hw-bindings* (make-cell '()))
(define *avail-configurations* (make-cell '()))

(define *prev-current* (make-cell 0))

(define (prev-interface)
  (car (list-ref (cell-ref *avail-ifaces*) (cell-ref *prev-current*))))

(define (current-interface)
  (let ((c (ifaces current)))
    (and (number? c)
	 (>= c 0)
	 (car (list-ref (cell-ref *avail-ifaces*) c)))))

(define (param-index cmd param list)
  (or (string-list-index (woo-get-option cmd param) (map car (cell-ref list)))
      0))

(define (param-value w list)
  (let ((c (w current)))
    (if (>= c 0)
      (car (list-ref (cell-ref list) c))
      "")))

(define (param-init path widget list)
  (let ((data (woo-list/name+label path)))
    (cell-set! list data)
    (widget rows (map cdr data))))

(define (read-interface name)
  (and (string? name)
       (let ((cmd (woo-read-first "/net-eth" 'ifname name)))

         (hostname text (woo-get-option cmd 'hostname))
         (dns text (woo-get-option cmd 'dns))
         (search text (woo-get-option cmd 'search))

	 (iface-info text (woo-get-option cmd 'info))
	 (iface-ip text (woo-get-option cmd 'ip))
	 (iface-mask current (param-index cmd 'mask *avail-masks*))
	 (iface-gw text (woo-get-option cmd 'default)
	 (iface-hw-binding current (param-index cmd 'hw_binding *avail-hw-bindings*))

	 (iface-configuration current (param-index cmd 'configuration *avail-configurations*))
	 (iface-configuration selected)

	 (w-button activity (woo-get-option cmd 'wireless))))))

(define (write-interface path name)
    (and (string? name)
	 (woo-write/constraints
	   path
	   'hostname (hostname text)
	   'dns (dns text)
	   'search (search text)

	   'ifname name
	   'ip    (iface-ip text)
	   'mask  (param-value iface-mask *avail-masks*)
	   'hw_binding (param-value iface-hw-binding *avail-hw-bindings*)
	   'configuration (param-value iface-configuration *avail-configurations*)
	   'default (iface-gw text))))

(define (commit-interface)
  (let ((name (or (current-interface) "")))
    (woo-catch/message
      (thunk
       (write-interface "/net-eth/" name)
       (and (global 'frame:next)
          (write-interface "/autoinstall/net-eth/" name))
       (woo-write "/net-eth" 'commit #t)
       (and (global 'frame:next)
          (woo-write "/net-eth" 'commit #t))))))

(define (reset-interface)
  (woo-catch/message
    (thunk
      (woo-write "/net-eth" 'reset #t)
      (and (global 'frame:next) (woo-write "/net-eth" 'reset #t))

      (param-init "/net-eth/avail_masks" iface-mask *avail-masks*)
      (param-init "/net-eth/avail_hw_bindings" iface-hw-binding *avail-hw-bindings*)
      (param-init "/net-eth/avail_configurations" iface-configuration *avail-configurations*)
      (param-init "/net-eth/avail_ifaces" ifaces *avail-ifaces*)

      (read-interface "")
      (cell-set! *prev-current* 0)
      (or (null? (cell-ref *avail-ifaces*)) (ifaces current 0)))))

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
				  (write-interface "/net-eth" (prev-interface))
				  (and (global 'frame:next) (write-interface "/autoinstall/net-eth" (prev-interface)))
				  (read-interface (current-interface))
				  (cell-set! *prev-current* (ifaces current))))
			      (ifaces current (cell-ref *prev-current*))))))
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
					  activity (string=? (param-value iface-configuration *avail-configurations*)
							     "static")))))

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
				    (let ((name (current-interface)))
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

(document:surround "/std/frame")
(document:insert "/std/functions")

(document:envelop with-translation _ "alterator-net-eth")

;;; Functions

(define *avail-ifaces* (make-cell '()))
(define *avail-masks* (make-cell '()))
(define *avail-hw-bindings* (make-cell '()))

(define *prev-current* (make-cell 0))

(define (prev-interface)
  (car (list-ref (cell-ref *avail-ifaces*) (cell-ref *prev-current*))))

(define (mask-index cmd)
  (or (string-list-index (woo-get-option cmd 'mask "24") (map car (cell-ref *avail-masks*)))
      0))

(define (hw-binding-index cmd)
  (format #t "cmd=~S,list=~S~%" (woo-get-option cmd 'hw_binding) (map car (cell-ref *avail-hw-bindings*)))
  (or (string-list-index (woo-get-option cmd 'hw_binding) (map car (cell-ref *avail-hw-bindings*)))
      0))

(define (current-interface)
  (let ((c (ifaces current)))
    (and (number? c)
	 (>= c 0)
	 (car (list-ref (cell-ref *avail-ifaces*) c)))))

(define (current-mask)
  (let ((c (iface-mask current)))
    (if (>= c 0)
      (car (list-ref (cell-ref *avail-masks*) c))
      "")))

(define (current-hw-binding)
  (let ((c (iface-hw-binding current)))
    (if (>= c 0)
      (car (list-ref (cell-ref *avail-hw-bindings*) c))
      "")))

(define (read-interface name)
  (and (not-empty-string? name)
       (let ((cmd (woo-read-first (string-append "/net-eth" "/" name))))
	 (iface-info text (string-append "<small>(" (woo-get-option cmd 'info) ")</small>"))
	 (iface-dhcp state (woo-get-option cmd 'dhcp #f) toggled)
	 (iface-ip text (woo-get-option cmd 'ip))
	 (iface-mask current (mask-index cmd))
	 (iface-hw-binding current (hw-binding-index cmd))
	 (w-button activity (woo-get-option cmd 'wireless))
	 (iface-gw text (woo-get-option cmd 'default)))))

(define (write-interface path name)
    (and (not-empty-string? name)
	 (woo-write/constraints 
	   (string-append path "/" name)
	   'dhcp  (iface-dhcp state)
	   'ip    (iface-ip text)
	   'mask  (current-mask)
	   'hw_binding (current-hw-binding)
	   'default (iface-gw text))))

(define (commit-interface)
  (woo-catch/message
    (thunk
      (write-interface "/net-eth/" (current-interface))
      (and (global 'frame:next)
	   (write-interface "/autoinstall/net-eth/" (current-interface)))
      (woo-write "/net-eth" 'commit #t)
      (and (global 'frame:next)
	   (woo-write "/net-eth" 'commit #t)))))

(define (reset-interface)
  (woo-catch/message
    (thunk
      (woo-write "/net-eth" 'reset #t)
      (and (global 'frame:next)
	   (woo-write "/net-eth" 'reset #t))
       (let ((avail-masks (woo-list/name+label "/net-eth/eth0/avail_masks")))
	(cell-set! *avail-masks* avail-masks)
	(iface-mask rows (map cdr avail-masks)))
       (let ((avail-hw-bindings (woo-list/name+label "/net-eth/eth0/avail_hw_bindings")))
         (cell-set! *avail-hw-bindings* avail-hw-bindings)
         (iface-hw-binding rows (map cdr avail-hw-bindings)))
      (let ((avail-ifaces (woo-list/name+label "/net-eth")))
	(cell-set! *avail-ifaces* avail-ifaces)
	(ifaces rows (map cdr avail-ifaces))
	(or (null? avail-ifaces)
	    (begin (ifaces current 0)
	           (cell-set! *prev-current* 0)
	           (read-interface (current-interface))))))))

;;; UI

(gridbox
  columns "20;20;40;20"
  ;;
  (spacer)
  (label (_ "Interface") align "right")
  (document:id ifaces (combobox
			(when selected
			  (or (woo-catch/message
				(thunk
				  (write-interface "/net-eth" (prev-interface))
				  (and (global 'frame:next) (write-interface "/autoinstall/net-eth" (prev-interface)))
				  (read-interface (current-interface))
				  (cell-set! *prev-current* (ifaces current))))
			      (ifaces current (cell-ref *prev-current*))))))
  (spacer)

  ;;
  (spacer)
  (spacer)
  (document:id iface-info (label ""))
  (spacer)

  ;;
  (spacer)
  (spacer)
  (document:id iface-dhcp (checkbox (_ "Use DHCP")
				    (when toggled
				      ((widgets iface-ip
						iface-gw
						iface-mask)
				       activity (not (iface-dhcp state))))))
  (spacer)
 
  ;;
  (spacer)
  (label (_ "IP address") align "right")
  (document:id iface-ip (edit ""))
  (spacer)

  ;;
  (spacer)
  (label (_ "Netmask") align "right")
  (document:id iface-mask (combobox))
  (spacer)

  ;;
  (spacer)
  (label (_ "Default gateway") align "right")
  (document:id iface-gw (edit ""))
  (spacer)

  ;;
  (spacer)
  (label (_ "Hardware binding") align "right")
  (document:id iface-hw-binding (combobox))

  ;;
  (spacer)
  (spacer)
  (spacer)
  (document:id w-button (button (_ "Wireless settings")
				activity #f
				(when clicked
				  (let ((name (current-interface)))
				    (and (not-empty-string? name)
					 (document:popup "/net-wifi/" 'interface name))))))
  )


;;;;;;;;;;;;;;;

(or (global 'frame:next)
    (hbox align "center"
	  (document:id c-button (button (_ "Apply") (when clicked (commit-interface))))
	  (document:id r-button (button (_ "Reset") (when clicked (reset-interface))))))

;;;;;;;;;;;;;;;;;;

(document:root
  (when loaded (reset-interface)))

(frame:on-back (thunk (or (commit-interface) 'cancel)))
(frame:on-next (thunk (or (commit-interface) 'cancel)))

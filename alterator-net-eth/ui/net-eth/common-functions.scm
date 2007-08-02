(define avail-ifaces (woo-list/name+label "/net-eth"))
(define avail-masks (woo-list/name+label "/net-eth/eth0/avail_masks"))

(define (update-interface name)
  (and (not-empty-string? name)
       (let ((cmd (woo-read-first (string-append "/net-eth" "/" name))))
	 (iface-info text (string-append "<small>(" (woo-get-option cmd 'info) ")</small>"))
         (iface-dhcp state (woo-get-option cmd 'dhcp #f) toggled)
         (iface-ip text (woo-get-option cmd 'ip))
         (iface-mask current (string-list-index (woo-get-option cmd 'mask "24") (map car avail-masks)))
         (w-button activity (woo-get-option cmd 'wireless))
         (iface-gw text (woo-get-option cmd 'default)))))

(define (commit-interface path name . args)
  (if (or (iface-dhcp state) (not-empty-string? (iface-ip text)))
      (and (not-empty-string? name)
           (woo-catch/message
            (thunk
             (apply
              woo-write/constraints (string-append path "/" name)
              'dhcp  (iface-dhcp state)
              'ip    (iface-ip text)
              'mask  (current-mask)
              'default (iface-gw text) args))))
      #t))

(define (current-interface)
  (and (number? (ifaces current))
       (>= (ifaces current) 0)
       (car (list-ref avail-ifaces (ifaces current)))))

(define (current-mask)
  (car (list-ref avail-masks (iface-mask current))))

(define (net-wifi name)
  (and (not-empty-string? name)
       (document:popup "/net-wifi/" 'interface name)))

(define (common-behaviour)
  (w-button (when clicked (net-wifi (current-interface))))
  (ifaces rows (map cdr avail-ifaces))
  (and (positive? (ifaces count))
       (begin (ifaces current 0)
              (update-interface (current-interface))))
  ;;constraints
  (iface-dhcp (when toggled
                ((widgets iface-ip
                          iface-gw
                          iface-mask)
                 activity (not (iface-dhcp state)))))
  (iface-dhcp toggled))


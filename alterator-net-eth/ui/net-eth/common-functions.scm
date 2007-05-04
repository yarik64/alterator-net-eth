(define (name+label x)
  (cons (woo-get-option x 'name)
        (woo-get-option x 'label)))

(define avail-ifaces (map name+label (woo-list "/net-eth")))
(define avail-masks (map name+label (woo-list "/net-eth/eth0/avail_masks")))

(define (update-interface name)
  (and (not-empty-string? name)
       (let ((cmd (woo-read-first (string-append "/net-eth" "/" name))))
         (iface-enabled state (woo-get-option cmd 'state #f))
         (iface-dhcp state (woo-get-option cmd 'dhcp #f))
         (iface-ip text (woo-get-option cmd 'ip))
         (iface-mask current (string-list-index (woo-get-option cmd 'mask "24") (map car avail-masks)))
         (w-button activity (woo-get-option cmd 'wireless))
         (iface-gw text (woo-get-option cmd 'default)))))

(define (commit-interface name . args)
  (and (not-empty-string? name)
       (woo-catch/message
        (thunk
         (apply
          woo-write/constraints (string-append "/net-eth" "/" name)
          'state (iface-enabled state)
          'dhcp  (iface-dhcp state)
          'ip    (iface-ip text)
          'mask  (current-mask)
          'default (iface-gw text) args)))))

(define (current-interface)
  (and (number? (ifaces current))
       (>= (ifaces current) 0)
       (car (list-ref avail-ifaces (ifaces current)))))

(define (current-mask)
  (car (list-ref avail-masks (iface-mask current))))

(define (common-behaviour)
  (w-button (when clicked (frame:replace "/net-wifi")))
  (ifaces rows (map cdr avail-ifaces))
  (and (positive? (ifaces count))
       (begin (ifaces current 0)
              (update-interface (current-interface))))
  (document:root
   (when loaded
     (update-constraints "write" "/net-eth"))))

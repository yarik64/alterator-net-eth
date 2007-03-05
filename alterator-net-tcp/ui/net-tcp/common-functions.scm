(define (name+label x)
  (cons (woo-get-option x 'name)
        (woo-get-option x 'label)))

(define avail-ifaces (map name+label (woo-list "/net-tcp")))
(define avail-masks (map name+label (woo-list "/net-tcp/eth0/avail_masks")))

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

(define (commit-interface name . args)
  (woo-catch/message
   (thunk
    (apply
     woo-write/constraints (string-append "/net-tcp" "/" name)
                           'state (iface-enabled state)
                           'dhcp  (iface-dhcp state)
                           'ip    (iface-ip text)
                           'mask  (current-mask)
                           'default (iface-gw text) args))))

(define (current-interface)
  (and (>= (ifaces current) 0)
       (car (list-ref avail-ifaces (ifaces current)))))

(define (current-mask)
  (car (list-ref avail-masks (iface-mask current))))

(define (common-behaviour)
  (w-button (when clicked (frame:replace "/net-wifi")))
  (ifaces header (vector (_ "Network interfaces"))
          rows (map cdr avail-ifaces))
  (and (positive? (ifaces count))
       (begin (ifaces current 0)
              (update-interface (current-interface))))
  (document:root
   (when loaded
     (update-constraints "write" "/net-tcp"))))

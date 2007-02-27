(define prev-current (make-cell 0))

(define (prev-interface)
  (car (list-ref avail-ifaces (cell-ref prev-current))))

(define (restart-interfaces)
  (woo-catch/message
   (thunk (woo-try "restart" "/net-tcp")
          (woo-try "restart" "/autoinstall/net-tcp"))))

(define (auto-restart-interfaces)
  (woo-try "restart" "/autoinstall/net-tcp"))

(define (auto-commit-interface name . args)
  (apply
   woo-write (string-append "/autoinstall/net-tcp" "/" name)
             'state (iface-enabled state)
             'dhcp  (iface-dhcp state)
             'ip    (iface-ip text)
             'mask  (current-mask)
             'default (iface-gw text) args))

(define (install-behaviour)
  (ifaces (when selected
            (and (commit-interface (prev-interface) 'restart #f)
                 (auto-commit-interface (prev-interface) 'restart #f)
                 (update-interface (current-interface))
                 (cell-set! prev-current (ifaces current)))))
  (g-button (when clicked (and (restart-interfaces)
                               (auto-restart-interfaces)
                               (frame:replace "/net-general"))))
  (frame:on-back (thunk (restart-interfaces) (frame:replace "/net-general") 'cancel))
  (frame:on-next (thunk (restart-interfaces) (frame:replace "/net-general") 'cancel)))


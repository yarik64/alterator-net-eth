(define prev-current (make-cell 0))

(define (prev-interface)
  (car (list-ref avail-ifaces (cell-ref prev-current))))

(define (auto-commit-interface name . args)
  (and (not-empty-string? name)
       (apply
        woo-write (string-append "/autoinstall/net-eth" "/" name)
        'dhcp  (iface-dhcp state)
        'ip    (iface-ip text)
        'mask  (current-mask)
        'default (iface-gw text) args)))

(define (commit-current-interface)
  (commit-interface (current-interface) 'restart #f)
  (auto-commit-interface (current-interface) 'restart #f))

(define (restart-interfaces)
  (and  (commit-current-interface)
        (woo-catch/message
          (thunk 
            (woo-try "restart" "/net-eth")
            (woo-try "restart" "/autoinstall/net-eth")))))
 
(define (install-behaviour)
  (ifaces (when selected
            (and (commit-interface (prev-interface) 'restart #f)
                 (auto-commit-interface (prev-interface) 'restart #f)
                 (update-interface (current-interface))
                 (cell-set! prev-current (ifaces current)))))
  (and (global 'frame:auto-save)
       (cell-set! (global 'frame:auto-save) commit-current-interface))
  (frame:on-back (thunk (or (restart-interfaces) 'cancel)))
  (frame:on-next (thunk (or (restart-interfaces) 'cancel))))



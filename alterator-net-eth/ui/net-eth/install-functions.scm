(define prev-current (make-cell 0))

(define (prev-interface)
  (car (list-ref avail-ifaces (cell-ref prev-current))))

(define (commit-current-interface)
  (and (commit-interface "/net-eth" (current-interface) 'restart #f)
       (commit-interface "/autoinstall/net-eth" (current-interface) 'restart #f)))

(define (restart-interfaces)
  (and  (commit-current-interface)
        (woo-catch/message
          (thunk 
            (woo-try "restart" "/net-eth")
            (woo-try "restart" "/autoinstall/net-eth")))))
 
(define (install-behaviour)
  (ifaces (when selected
            (and (commit-interface "/net-eth" (prev-interface) 'restart #f)
                 (commit-interface "/autoinstall/net-eth" (prev-interface) 'restart #f)
                 (update-interface (current-interface))
                 (cell-set! prev-current (ifaces current)))))
  (and (global 'frame:auto-save)
       (cell-set! (global 'frame:auto-save) commit-current-interface))
  (frame:on-back (thunk (or (restart-interfaces) 'cancel)))
  (frame:on-next (thunk (or (restart-interfaces) 'cancel))))



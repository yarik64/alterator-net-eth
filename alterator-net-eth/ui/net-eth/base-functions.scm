(define (base-behaviour)
  (ifaces (when selected (update-interface (current-interface))))
  (c-button (when clicked (commit-interface "/net-eth" (current-interface))))
  (r-button (when clicked (update-interface (current-interface)))))

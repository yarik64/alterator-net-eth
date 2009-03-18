(define-module (ui net-eth advanced ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

(define (ui-read name)
  (catch/message
    (lambda()
      (let ((cmd (woo-read-first "/net-eth" 'name name)))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled"))
      (form-update-enum "hw_binding" (woo-list "/net-eth/avail_hw_bindings"))
      (form-update-value "iface" name)
      (form-update-visibility "wireless" (woo-get-option cmd 'wireless))

      (form-update-value-list '("name" "controlled" "hw_binding") cmd)))))

(define (ui-exit)
  (form-replace (format #f "/net-eth?iface=~A" (form-value "name"))))

(define (ui-write)
  (catch/message
    (lambda()
      (woo-write "/net-eth"
		 'name (form-value "name")
		 'hw_binding (form-value "hw_binding")
		 'controlled (form-value "controlled"))
      (ui-exit))))

(define (ui-wireless)
  (form-replace (format #f "/net-wifi?iface=~A" (form-value "name"))))

(define (init)
  (ui-read (form-value "iface"))
  (form-bind "ok" "click" ui-write)
  (form-bind "cancel" "click" ui-exit)
  (form-bind "wireless" "click" ui-wireless))

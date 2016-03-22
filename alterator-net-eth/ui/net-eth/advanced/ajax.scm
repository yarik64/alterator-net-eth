(define-module (ui net-eth advanced ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

(define (ui-read name)
  (catch/message
	(lambda()
	  (let ((cmd (woo-read-first "/net-eth" 'name name)))
		(form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'name name))
		(form-update-value "iface" name)
		(form-update-value-list '("name" "controlled" "onboot") cmd)))))

(define (ui-exit)
  (form-replace (format #f "/net-eth?iface=~A" (form-value "name"))))

(define (ui-write)
  (catch/message
    (lambda()
      (woo-write "/net-eth"
		 'name (form-value "name")
		 'controlled (form-value "controlled")
		 'onboot (form-value "onboot"))
      (ui-exit))))

(define (init)
  (ui-read (form-value "iface"))
  (form-bind "ok" "click" ui-write)
  (form-bind "cancel" "click" ui-exit))

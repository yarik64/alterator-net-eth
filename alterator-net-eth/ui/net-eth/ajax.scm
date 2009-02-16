(define-module (ui net-eth ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

(define (update-configuration configuration)
    (form-update-activity
      '("ip" "mask" "default")
      (string=? configuration "static")))

(define (read-interface name)
  (let ((cmd (woo-read-first "/net-eth" 'name name 'language (form-value "language"))))

    (form-update-value-list
      '("name")
      cmd)
    (form-update-value-list
      '("hostname" "dns" "search")
      cmd)
    (form-update-value-list
      '("adaptor" "info" "ip" "mask" "default" "hw_binding" "configuration")
      cmd)

    (form-update-visibility
      "wireless"
      (woo-get-option cmd 'wireless))

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("language"
			    "hostname" "dns" "search"
			    "ip" "mask" "default" "hw_binding" "configuration"))))

(define (wireless-interface)
 (form-replace (format #f "/net-wifi?iface=~A" (form-value "name"))))

(define (update-interface)
  (or (catch/message
	(lambda()
	  (let ((name (form-value "name")))
	    (write-interface (form-value "prev_name"))
	    (read-interface name)
	    (form-update-value "prev_name" name))))
      (form-update-value "name" (form-value "prev_name"))))

(define (init)
 (read-interface "")
 (form-update-value "prev_name" (form-value "name"))

 (form-bind "name" "change" update-interface)
 (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration"))))
 (form-bind "wireless" "click" wireless-interface))

(define-module (ui net-eth ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

(define (update-configuration configuration)
    (form-update-activity
      '("ip" "mask" "default")
      (string=? configuration "static")))

(define (read-interface name)
  (let ((cmd (woo-read-first "/net-eth" 'ifname name)))

    (form-update-value-list
      '("name")
      cmd)
    (form-update-value-list
      '("hostname" "dns" "search")
      cmd)
    (form-update-value-list
      '("info" "ip" "mask" "default" "hw_binding" "configuration")
      cmd)

    (form-update-visibility
      "wireless"
      (woo-get-option cmd 'wireless))

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'ifname name
	 (form-value-list '("language"
			    "hostname" "dns" "search"
			    "ip" "mask" "default" "hw_binding" "configuration"))))

(define (update-interface)
  (or (catch
        #t
	(lambda()
	  (let ((name (form-value "name")))
	    (write-interface (form-value "prev_name"))
	    (read-interface name)
	    (form-update-value "prev_name" name)))
	(lambda args
	  (format #t "args=~S~%" args)
	  #f))
      (form-update-value "name" (form-value "prev_name"))))

(define (init)
 (read-interface "")
 (form-update-value "prev_name" (form-value "name"))

 (form-bind "name" "change" update-interface)
 (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration")))))
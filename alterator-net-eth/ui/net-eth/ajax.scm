(define-module (ui net-eth ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

;;; low level

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
      '("computer_name" "dns" "search")
      cmd)
    (form-update-value-list
      '("adaptor" "ip" "mask" "default" "configuration")
      cmd)

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("language"
			    "computer_name" "dns" "search"
			    "ip" "mask" "default" "configuration"))))

;;; high level

(define (update-interface)
  (or (catch/message
	(lambda()
	  (let ((name (form-value "name")))
	    (write-interface (form-value "prev_name"))
	    (read-interface name)
	    (form-update-value "prev_name" name))))
      (form-update-value "name" (form-value "prev_name"))))

(define (advanced-interface)
  (form-replace (format #f "/net-eth/advanced?iface=~A" (form-value "name"))))

(define (commit-interface)
  (catch/message
    (lambda()
      (write-interface (or (form-value "name") ""))
      (woo-write "/net-eth" 'commit #t))))

(define (reset-interface)
  (catch/message
    (lambda()
      (woo-write "/net-eth" 'reset #t)

      (read-interface (or (form-value "iface") ""))
      (form-update-value "prev_name" (or (form-value "iface") "")))))

(define (init-interface)
  (catch/message
    (lambda()
      (form-update-enum "mask" (woo-list "/net-eth/avail_masks" 'language (form-value "language")))
      (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations" 'language (form-value "language")))
      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces" 'language (form-value "language")))

      (read-interface (or (form-value "iface") ""))
      (form-update-value "prev_name" (or (form-value "iface") "")))))


(define (init)
 (init-interface)

 (form-bind "name" "change" update-interface)
 (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration"))))
 (form-bind "advanced" "click" advanced-interface)

 (form-bind "commit" "click" commit-interface)
 (form-bind "reset" "click" reset-interface))

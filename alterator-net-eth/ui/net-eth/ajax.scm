(define-module (ui net-eth ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

;;; low level

(define (update-configuration configuration)
    (form-update-activity
      '("addresses" "ip" "mask" "add-ip" "default")
      (string=? configuration "static")))

(define (read-interface name)
  (let ((cmd (woo-read-first "/net-eth" 'name name 'language (form-value "language"))))

   (form-update-visibility
      "wireless"
      (and (woo-get-option cmd 'wireless)
	   (string=? (woo-get-option cmd 'controlled) "etcnet")))
    (form-update-value-list
      '("name" "real_name")
      cmd)
    (form-update-value-list
      '("computer_name" "dns" "search")
      cmd)
    (form-update-value-list
      '("adaptor" "addresses" "mask" "default" "configuration")
      cmd)

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("language"
			    "computer_name" "dns" "search"
			    "addresses" "default" "configuration"))))

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
  (let ((name (form-value "name")))
    (and (catch/message
           (lambda()
             (write-interface name)))
         (form-replace "/net-eth/advanced" 'iface name))))

(define (wireless-interface)
  (format #t "wireless-interface:real_name=~S~%" (form-value "real_name"))
  (form-replace "/net-wifi/" 'iface (form-value "real_name")))

(define (commit-interface)
  (catch/message
    (lambda()
      (write-interface (or (form-value "name") ""))
      (woo-write "/net-eth" 'commit #t))))

(define (reset-interface)
  (catch/message
    (lambda()
      (woo-write "/net-eth" 'reset #t)

      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces" 'language (form-value "language")))
      (read-interface "")
      (form-update-value "prev_name"  ""))))

(define (init-interface)
  (catch/message
    (lambda()
      (form-update-enum "mask" (woo-list "/net-eth/avail_masks" 'language (form-value "language")))
      (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations" 'language (form-value "language")))
      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces" 'language (form-value "language")))

      (read-interface (or (form-value "iface") ""))
      (form-update-value "prev_name" (or (form-value "iface") "")))))

(define (ui-append-address)
    (if (regexp-exec (make-regexp (string-append "^" "([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])([.]([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9+]|25[0-5])){3}" "$") regexp/extended) (form-value "ip"))
	(begin
	    (form-update-value "addresses" (string-append (form-value "addresses") (if (string-null? (form-value "addresses")) "" "\n") (form-value "ip") "/" (form-value "mask")))
	    (form-update-visibility "invalid_ip_message" #f)
	)
	(form-update-visibility "invalid_ip_message" #t)
    )
)

(define (init)
 (init-interface)
  (form-update-visibility "invalid_ip_message" #f)

 (form-bind "name" "change" update-interface)
 (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration"))))
 (form-bind "advanced" "click" advanced-interface)
 (form-bind "wireless" "click" wireless-interface)
 (form-bind "add-ip" "click" ui-append-address)

 (form-bind "commit" "click" commit-interface)
 (form-bind "reset" "click" reset-interface))

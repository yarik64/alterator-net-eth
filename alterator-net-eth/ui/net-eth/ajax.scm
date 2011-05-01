(define-module (ui net-eth ajax)
    :use-module (srfi srfi-1)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

;;; low level

(define (update-configuration configuration)
    (form-update-activity
      '("addresses" "ip" "add-mask" "add-ip" "default")
      (string=? configuration "static")))

(define (read-interface-address name)
    (catch/message (lambda()
       (form-update-enum "addresses"
           (apply woo "list_iface_address" "/net-eth" 'name name)))))

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
    (read-interface-address name)
    (form-update-value-list
      '("adaptor" "add-mask" "default" "configuration")
      cmd)

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("language"
			    "computer_name" "dns" "search"
			    "default" "configuration"))))

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
    (begin
        (catch/message
	(lambda()
	    (write-interface (or (form-value "name") ""))
	    (woo-write "/net-eth" 'commit #t))))
)

(define (reset-interface)
  (form-update-visibility "invalid_ip_message" #f)
  (form-update-visibility "invalid_addresses_list" #f)
  (catch/message
    (lambda()
      (woo-write "/net-eth" 'reset #t)

      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces" 'language (form-value "language")))
      (read-interface "")
      (form-update-value "prev_name"  ""))))

(define (init-interface)
  (catch/message
    (lambda()
      (form-update-enum "add-mask" (woo-list "/net-eth/avail_masks" 'language (form-value "language")))
      (form-update-value "add-mask" "24")
      (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations" 'language (form-value "language")))
      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces" 'language (form-value "language")))

      (read-interface (or (form-value "iface") ""))
      (form-update-value "prev_name" (or (form-value "iface") "")))))

(define (ui-append-address)
    (if (regexp-exec (make-regexp (string-append "^" "([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])([.]([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9+]|25[0-5])){3}" "$") regexp/extended) (form-value "add-ip"))
	(begin
	    (apply woo "add_iface_address" "/net-eth" (form-value-list '("real_name" "add-ip" "add-mask")))
	    (form-update-visibility "invalid_ip_message" #f)
	    (read-interface-address (form-value "real_name"))
	    (form-update-value "add-ip" "")
	)
	(form-update-visibility "invalid_ip_message" #t)
    )
)

(define (ui-delete-address)
    (catch/message (lambda()
       (apply woo "del_iface_address" "/net-eth"
           (form-value-list '("language" "real_name" "addresses")))))
    (read-interface-address (form-value "real_name"))
)

(define (check-addresses-list)
  (or (and (string? (form-value "addresses")) (string-null? (form-value "addresses")))
      (and (string? (form-value "addresses"))
           (every (lambda(x)
        	    (regexp-exec
        		(make-regexp
        		    (string-append "^" "([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])([.]([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9+]|25[0-5])){3}" "/([1-9]|[12][0-9]|3[0-1])$")
        		    regexp/extended)
        		x))
                  (string-tokenize (form-value "addresses") (char-set-complement char-set:whitespace))))))

(define (init)
 (init-interface)
  (form-update-visibility "invalid_ip_message" #f)

 (form-bind "name" "change" update-interface)
 (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration"))))
 (form-bind "advanced" "click" advanced-interface)
 (form-bind "wireless" "click" wireless-interface)
 (form-bind "btn-del-ip" "click" ui-delete-address)
 (form-bind "btn-add-ip" "click" ui-append-address)

 (form-bind "commit" "click" commit-interface)
 (form-bind "reset" "click" reset-interface))

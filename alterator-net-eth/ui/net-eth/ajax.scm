(define-module (ui net-eth ajax)
    :use-module (srfi srfi-1)
    :use-module (alterator ajax)
	:use-module (alterator algo)
    :use-module (alterator woo)
    :export (init))

;;; low level

(define *prev_ipv* (make-cell ""))

(define (update-configuration-activity configuration)
    (form-update-activity
      '("addresses" "ip" "add-mask" "add-ip" "default" "btn-add-ip" "btn-del-ip" "ipl_label"
		"search_comment" "dns" "search")
      (and (form-value "ipv_enabled") (string=? configuration "static"))))

(define (update-ipv-activity)
   (form-update-activity "configuration" (form-value "ipv_enabled"))
   (update-configuration-activity (form-value "configuration")))

(define (ipv_changed)
  (let ((name (form-value "name"))
		(ipv (form-value "ipv"))
		(prev_ipv (cell-ref *prev_ipv*)))
	(if (not (string=? ipv prev_ipv))
	  (begin
		(cell-set! *prev_ipv* ipv)
		; write configuration for previous ipv
		(catch/message
		  (lambda()
			(write-interface name prev_ipv)))
		; and read for current ipv
		(catch/message
		  (lambda()
			(reset-enums ipv)
			(read-interface name ipv)))))))

(define (read-interface-address name)
    (catch/message (lambda()
       (form-update-enum "addresses"
           (woo-list "/net-eth/avail_iface_address" 'name name 'ipv (form-value "ipv"))))))

(define (read-interface name ipv)
  (let ((cmd (woo-read-first "/net-eth" 'name name 'ipv ipv 'language (form-value "language"))))
   (form-update-visibility
      "wireless"
      (and (woo-get-option cmd 'wireless)
	   (string=? (woo-get-option cmd 'controlled) "etcnet")))
    (form-update-value-list
      '("name" "real_name" "ipv_enabled")
      cmd)
    (form-update-value-list
      '("computer_name" "dns" "search")
      cmd)
    (read-interface-address name)
    (form-update-value-list
      '("adaptor" "add-mask" "default" "configuration")
      cmd)
	(update-ipv-activity)))

(define (write-interface name ipv)
  (apply woo-write
	 "/net-eth"
	 'name name
	 'ipv ipv
	 (form-value-list '("language" "ipv_enabled"
			    "computer_name" "dns" "search"
			    "default" "configuration"))))

;;; high level

(define (update-interface)
  (or (catch/message
	(lambda()
	  (let ((name (form-value "name"))
			(ipv (form-value "ipv")))
	    (write-interface (form-value "prev_name") ipv)
	    (read-interface name ipv)
	    (form-update-value "prev_name" name))))
      (form-update-value "name" (form-value "prev_name"))))

(define (advanced-interface)
  (let ((name (form-value "name"))
		(ipv (form-value "ipv")))
    (and (catch/message
           (lambda()
             (write-interface name ipv)))
         (form-replace "/net-eth/advanced" 'iface name))))

(define (wireless-interface)
  (format #t "wireless-interface:real_name=~S~%" (form-value "real_name"))
  (form-replace "/net-wifi/" 'iface (form-value "real_name")))

(define (commit-interface)
    (begin
        (catch/message
	(lambda()
	    (write-interface (or (form-value "name") "") (form-value "ipv"))
	    (woo-write "/net-eth" 'commit #t))))
)

(define (reset-enums ipv)
  (form-update-enum "add-mask" (woo-list "/net-eth/avail_masks" 'ipv ipv 'language (form-value "language")))
  (form-update-value "add-mask" (if (string=? ipv "4") "24" "64"))
  (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations" 'ipv ipv 'language (form-value "language")))
  (form-update-enum "name" (woo-list "/net-eth/avail_ifaces" 'language (form-value "language"))))

(define (reset-interface)
  (catch/message
    (lambda()
	  (let ((ipv (form-value "ipv")))
      (woo-write "/net-eth" 'reset #t)

	  (reset-enums ipv)
      (read-interface "" ipv)
      (form-update-value "prev_name"  "")))))

(define (init-interface)
  (form-update-enum "ipv" (woo-list "/net-eth/avail_ipv"))
  (form-update-value "ipv" "4")
  (cell-set! *prev_ipv* "4")
  (catch/message
	(lambda()
	  (reset-enums "4")
	  (read-interface (or (form-value "iface") "") "4")
	  (form-update-value "prev_name" (or (form-value "iface") "")))))

(define (ui-append-address)
  (and (catch/message (lambda()
						(apply woo "add_iface_address" "/net-eth" 
							   (form-value-list '("language" "ipv" "name" "add-ip" "add-mask")))))
	   (read-interface-address (form-value "name"))
	   (form-update-value "add-ip" "")))

(define (ui-delete-address)
    (catch/message (lambda()
       (apply woo "del_iface_address" "/net-eth"
           (form-value-list '("language" "ipv" "name" "addresses")))))
    (read-interface-address (form-value "name"))
)

(define (init)
 (init-interface)
 (form-bind "name" "change" update-interface)
 (form-bind "ipv" "change" ipv_changed)
 (form-bind "ipv_enabled" "change" update-ipv-activity)
 (form-bind "configuration" "change" (lambda() (update-configuration-activity (form-value "configuration"))))
 (form-bind "advanced" "click" advanced-interface)
 (form-bind "wireless" "click" wireless-interface)
 (form-bind "btn-del-ip" "click" ui-delete-address)
 (form-bind "btn-add-ip" "click" ui-append-address)

 (form-bind "commit" "click" commit-interface)
 (form-bind "reset" "click" reset-interface))

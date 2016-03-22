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

(define (read-interface-current-address name)
    (catch/message (lambda()
       (form-update-enum "addresses"
           (woo-list "/net-eth/list_current_iface_address" 'name name 'ipv (form-value "ipv"))))))

(define (read-interface name ipv)
  (let* ((cmd (woo-read-first "/net-eth" 'name name 'ipv ipv 'language (form-value "language")))
         (iface-type (woo-get-option cmd 'iface_type "eth"))
         (is-vlan (if (string-ci=? iface-type "vlan") #t #f))
		 (has-bond-module (woo-get-option cmd 'bond_module_installed))
		 (has-wifi-module (woo-get-option cmd 'wifi_module_installed))
		 (has-vlan-module (woo-get-option cmd 'vlan_module_installed))
		 (has-bridge-module (woo-get-option cmd 'bridge_module_installed))
		 (is-bond (if (string-ci=? iface-type "bond") #t #f))
		 (is-bridge (if (string-ci=? iface-type "bri") #t #f)))

    (form-update-visibility "vlan" (string-ci=? iface-type "eth"))

    (for-each
      (lambda(lst)
        (form-update-visibility lst has-bond-module))
      '("bond_new" "bond_del" "bond_ch"))

	(if has-bond-module
	  (for-each
		(lambda(lst)
		  (form-update-activity lst is-bond))
		'("bond_del" "bond_ch")))

    (for-each
      (lambda(lst)
        (form-update-visibility lst has-bridge-module))
      '("bridge_new" "bridge_del" "bridge_ch"))

	(if has-bridge-module
	  (for-each
		(lambda(lst)
		  (form-update-activity lst is-bridge))
		'("bridge_del" "bridge_ch")))

	(form-update-visibility
	  "vlan"
	  (and
		has-vlan-module
		(not is-vlan)
		(not is-bond)))

   (form-update-visibility
      "wireless"
      (and
        has-wifi-module
        (woo-get-option cmd 'wireless)
        (string=? (woo-get-option cmd 'controlled) "etcnet")))
    (form-update-value-list
      '("name" "ipv_enabled")
      cmd)
    (form-update-value-list
      '("computer_name" "dns" "search")
      cmd)
    (form-update-activity "computer_name" (not (woo-get-option cmd 'altdomain)))

	(if (string=? (woo-get-option cmd 'configuration) "static")
	  (read-interface-address name)
	  (read-interface-current-address name))

    (form-update-value-list
      '("iface_info" "add-mask" "default" "configuration")
      cmd)
	(update-ipv-activity)))

(define (write-interface name ipv)
  (apply woo-write
	 "/net-eth"
	 'name name
	 'ipv ipv
	 ; stanv@: 'form-value-list' skips values for checkboxes == #f (unchecked)
	 ; put checkboxes separately
	 'ipv_enabled (form-value "ipv_enabled")
	 (form-value-list '("language"
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
  (format #t "wireless-interface:name=~S~%" (form-value "name"))
  (form-replace "/net-wifi/" 'iface (form-value "name")))

(define (vlan-interface)
  (let ((name (form-value "name"))
		(ipv (form-value "ipv")))
    (and (catch/message
           (lambda()
             (write-interface name ipv)))
         (form-replace "/net-vlan" 'iface name))))

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
  (let* ( ; has form as : ("/net-eth/avail_ipv" name "4" label "IPv4")
          (available-ip-versions (woo-list "/net-eth/avail_ipv"))
          (only-ipv4-available (= (length available-ip-versions) 1)))
    (form-update-enum "ipv" available-ip-versions)
    (form-update-visibility "area-ip-version-select" (not only-ipv4-available)))
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

(define (bond-new)
  (form-replace "/net-bond"))

(define (bond-del)
  (catch/message (lambda()
     (woo-write "/net-bond/rm_bond" 'bond (form-value "name" 'language (form-value "language")))))
  (form-update-value "iface" "")
  (init-interface))

(define (bond-ch)
  (form-replace "/net-bond" 'iface (form-value "name")))

(define (bridge-new)
  (form-replace "/net-bridge"))

(define (bridge-del)
  (catch/message (lambda()
     (woo-write "/net-bridge/rm_bridge" 'bridge (form-value "name" 'language (form-value "language")))))
  (form-update-value "iface" "")
  (init-interface))

(define (bridge-ch)
  (form-replace "/net-bridge" 'iface (form-value "name")))

(define (init)
 (init-interface)
 (form-bind "name" "change" update-interface)
 (form-bind "ipv" "change" ipv_changed)
 (form-bind "ipv_enabled" "change" update-ipv-activity)
 (form-bind "configuration" "change" (lambda() (update-configuration-activity (form-value "configuration"))))
 (form-bind "wireless" "click" wireless-interface)
 (form-bind "advanced" "click" advanced-interface)
 (form-bind "vlan"     "click" vlan-interface)
 (form-bind "btn-del-ip" "click" ui-delete-address)
 (form-bind "btn-add-ip" "click" ui-append-address)

 (form-bind "bond_new" "click" bond-new)
 (form-bind "bond_del" "click" bond-del)
 (form-bind "bond_ch" "click" bond-ch)

 (form-bind "bridge_new" "click" bridge-new)
 (form-bind "bridge_del" "click" bridge-del)
 (form-bind "bridge_ch" "click" bridge-ch)

 (form-bind "commit" "click" commit-interface)
 (form-bind "reset" "click" reset-interface))

; vim: ft=lisp expandtab

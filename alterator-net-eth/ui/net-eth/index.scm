(document:surround "/std/frame")

;;; Functions

(define *prev_ipv* (make-cell "4"))

(define (update-configuration-activity configuration)
    (form-update-activity
      '("addresses" "default" "btn-del-ip" "ipl_label" "dns" "search" "search_comment")
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
  (let* ((cmd (woo-read-first "/net-eth" 'name name 'ipv ipv))
	 (iface-type (woo-get-option cmd 'iface_type "eth"))
	 (is-vlan (if (string-ci=? iface-type "vlan") #t #f)))
   (form-update-visibility
      "wireless"
      (and (woo-get-option cmd 'wireless)
	   (string=? (woo-get-option cmd 'controlled) "etcnet")
	   (not is-vlan)))

   (form-update-visibility "vlan" (string-ci=? iface-type "eth"))

   (for-each
     (lambda(lst)
	   (form-update-visibility lst is-vlan))
     '("box_vlan_caption" "box_vlan_info"))

   (for-each
     (lambda(lst)
       (form-update-visibility lst (not is-vlan)))
     '("adaptor" "advanced"))

   (form-update-value-list
      '("label_vlan_host" "label_vlan_vid")
      cmd)

    (form-update-value-list
      '("name" "real_name" "ipv_enabled")
      cmd)
    (form-update-value-list
      '("computer_name" "dns" "search")
      cmd)
	(form-update-activity "computer_name" (not (woo-get-option cmd 'altdomain)))
    (form-update-value-list
      '("add-mask" "adaptor" "default" "configuration")
      cmd)
    (read-interface-address name)
    (form-update-value-list
      '("configuration")
      cmd)
	(update-ipv-activity)
    )
)

(define (write-interface name ipv)
  (apply woo-write
	 "/net-eth"
	 'name name
	 'ipv ipv
	 (form-value-list '("ipv_enabled" "computer_name" "dns" "search" "default" "configuration"))
    ))

(define (commit-interface)
	(catch/message
	    (lambda()
		(write-interface (or (form-value "name") "") (form-value "ipv"))
		(woo-write "/net-eth" 'commit #t))))

(define (reset-enums ipv)
  (form-update-enum "add-mask" (woo-list "/net-eth/avail_masks" 'ipv ipv))
  (form-update-value "add-mask" (if (string=? ipv "4") "24" "64"))
  (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations" 'ipv ipv))
  (form-update-enum "name" (append
                             (woo-list "/net-eth/avail_ifaces")
                             (woo-list "/net-eth/list_vlans"))))

(define (reset-interface)
  (catch/message
    (lambda()
	  (let ((ipv (form-value "ipv")))
		(woo-write "/net-eth" 'reset #t 'ipv ipv)
		(reset-enums ipv)
		(read-interface "" ipv)
		(form-update-value "prev_name" (form-value "name"))))))

(define (init-interface)
      (form-update-enum "ipv" (woo-list "/net-eth/avail_ipv"))
      (form-update-value "ipv" "4")
	  (reset-interface))

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
         (begin
           (form-popup "/net-eth/advanced" 'name name)
           (form-update-enum "name" (append
                                      (woo-list "/net-eth/avail_ifaces")))
           (read-interface name ipv)
           (form-update-value "prev_name" (or (form-value "name") ""))))))

(define (wireless-interface)
  (format #t "wireless-interface:real_name=~S~%" (form-value "real_name"))
  (form-popup "/net-wifi/" 'iface (form-value "real_name")))

(define (vlan-interface)
  (let ((name (form-value "name"))
		(ipv (form-value "ipv")))
    (and (catch/message
           (lambda()
             (write-interface name ipv)))
         (begin
           (form-popup "/net-eth/vlan" 'name name)
           (form-update-enum "name" (append
                                      (woo-list "/net-eth/avail_ifaces")
                                      (woo-list "/net-eth/list_vlans")))
           (read-interface name ipv)
           (form-update-value "prev_name" (or (form-value "name") ""))))))

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

;;; UI

(edit name "prev_name" text "" visibility #f)
(edit name "real_name" text "" visibility #f)
(gridbox
  columns "0;100"
  margin 10

  (gridbox
    colspan 2
    columns "0;40;60"

    ;;
    (label text (_ "Computer name:") nameref "computer_name" align "right")
    (edit name "computer_name")
    (spacer)
   )

  (separator colspan 2)

  (label colspan 2 text (bold (_ "Interfaces")))

  (listbox name "name" max-width 155)
  (gridbox
    columns "0;100"

    ;;
    (textbox colspan 2 name "adaptor" max-height 70 alterability #f)

    ;;
    (label colspan 2 text (bold (_ "VLAN")) align "center" nameref "box_vlan_caption")

    ;;
    (hbox colspan 2 align "center" nameref "box_vlan_info"
          (label text (_ "Host:"))
          (label name "label_vlan_host")
          (label text (_ "VID:"))
          (label name "label_vlan_vid"))

	;;
	(hbox align "left" colspan 2
		(label text (_ "Select IP version:"))
		(combobox name "ipv")
		(checkbox name "ipv_enabled" text (_ "Enable")))
    ;;
    (label text (_ "Configuration:") align "right" nameref "configuration")
    (combobox name "configuration")

    ;;
    ;(spacer)(separator)

    ;;
    (label name "ipl_label" text (_ "IP addresses:") align "right" nameref "addresses")
    (gridbox columns "100;0"
	(document:id ui-addresses (listbox name "addresses" max-height 70))
	(button (_ "Delete") name "btn-del-ip" nameref "addresses")
    )
    (spacer)
    (gridbox columns "0;50;50;0" nameref "addresses"
	(label text (_ "IP:"))
	(document:id ui-add-ip (edit name "add-ip"))
	(document:id ui-add-mask (combobox name "add-mask"))
	(button (_ "Add") name "btn-add-ip")
    )

    ;;
    ;(spacer)(separator)

    ;;
    (label text (_ "Default gateway:") align "right" nameref "default")
    (edit name "default")

    ;;
    (label text (_ "DNS servers:") nameref "dns" align "right")
    (edit name "dns")

    ;;
    (label text (_ "Search domains:") nameref "search" align "right")
    (edit name "search")

    ;;
    (spacer)(label name "search_comment" text (small (_ "(multiple values should be space separated)")))

    ;;
    (button text (_ "Wireless settings...") name "wireless" align "right" visibility #f)
    (hbox align "right"
      (button text (_ "Advanced...") name "advanced" align "right")
      (button text (_ "Vlan...") name "vlan" align "right")))

  ;;
  (or (global 'frame:next)
      (label colspan 2))

  ;;
  (or (global 'frame:next)
      (hbox align "left"
	    colspan 2
	    (button (_ "Apply") name "apply")
	    (button (_ "Reset") name "reset"))))

;;;;;;;;;;;;;;;;;;

(document:root
  (when loaded
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
    (or (global 'frame:next)
      (begin (form-bind "apply" "click" commit-interface)
	     (form-bind "reset" "click" reset-interface)))))

(frame:on-back (lambda() (or (commit-interface) 'cancel)))
(frame:on-next (lambda() (or (commit-interface) 'cancel)))

; vim: ft=lisp expandtab

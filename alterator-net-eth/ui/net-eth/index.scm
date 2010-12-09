(document:surround "/std/frame")

;;; Functions

(define (update-configuration configuration)
    (form-update-activity
      '("addresses" "default")
      (string=? configuration "static")))

(define (read-interface name)
  (let ((cmd (woo-read-first "/net-eth" 'name name)))

   (form-update-visibility
      "wireless"
      (and (woo-get-option cmd 'wireless)
	   (string=? (woo-get-option cmd 'controlled) "etcnet")))
    (form-update-value-list
      '("name" "real_name")
      cmd)
    (form-update-value-list
      '("computer_name" "addresses" "dns" "search")
      cmd)
    (form-update-value-list
      '("mask" "adaptor" "default" "configuration")
      cmd)

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("computer_name" "dns" "search"
			    "addresses" "default" "configuration"))))

(define (commit-interface)
  (catch/message
    (lambda()
      (write-interface (or (form-value "name") ""))
      (woo-write "/net-eth" 'commit #t))))

(define (reset-interface)
  (catch/message
    (lambda()
      (woo-write "/net-eth" 'reset #t)

      (form-update-enum "mask" (woo-list "/net-eth/avail_masks"))
      (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations"))
      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces"))

      (read-interface "")
      (form-update-value "prev_name" (form-value "name")))))

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
         (begin
           (form-popup "/net-eth/advanced" 'name name)
           (form-update-enum "name" (woo-list "/net-eth/avail_ifaces"))
           (read-interface name)
           (form-update-value "prev_name" (or (form-value "name") ""))))))

(define (wireless-interface)
  (format #t "wireless-interface:real_name=~S~%" (form-value "real_name"))
  (form-popup "/net-wifi/" 'iface (form-value "real_name")))


(define (ui-append-address)
    (if (regexp-exec (make-regexp (string-append "^" "([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])([.]([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9+]|25[0-5])){3}" "$") regexp/extended) (ui-add-ip value))
	(ui-addresses value (string-append (ui-addresses value) (if (string-null? (ui-addresses value)) "" "\n") (ui-add-ip value) "/" (ui-add-mask value)))
	(document:popup-critical (_ "invalid IP address") 'ok)
    )
)

(define (ui-delete-address)
	(document:popup-warning (_ "not implemented") 'ok)
)

;;; UI

(edit name "prev_name" text "" visibility #f)
(edit name "real_name" text "" visibility #f)
(gridbox
  columns "0;100"
  margin 20

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
    (textbox colspan 2 name "adaptor" max-height 80 alterability #f)

    ;;
    (label text (_ "Configuration:") align "right" nameref "configuration")
    (combobox name "configuration")

    ;;
    (spacer)(separator)

    ;;
    (label text (_ "IP addresses:") align "right" nameref "addresses")
    (gridbox columns "100;0"
	(document:id ui-addresses (textbox name "addresses" max-height 100))
	(button (_ "Delete") name "del-ip" nameref "addresses" visibility #f)
    )
    (spacer)
    (gridbox columns "0;50;50;0" nameref "addresses"
	(label text (_ "IP:"))
	(document:id ui-add-ip (edit))
	(document:id ui-add-mask (combobox name "mask"))
	(button (_ "Add") name "add-ip")
    )
    (spacer)(label text (small (_ "(multiple values should be one per row)")))

    ;;
    (spacer)(separator)

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
    (spacer)(label text (small (_ "(multiple values should be space separated)")))
    (label colspan 2)

    ;;
    (spacer)
    (button text (_ "Wireless settings...") name "wireless" align "right" visibility #f)

    ;;
    (spacer)
    (button text (_ "Advanced...") name "advanced" align "right"))

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
    (reset-interface)
    (form-bind "name" "change" update-interface)
    (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration"))))
    (form-bind "advanced" "click" advanced-interface)
    (form-bind "wireless" "click" wireless-interface)
    (form-bind "del-ip" "click" ui-delete-address)
    (form-bind "add-ip" "click" ui-append-address)
    (or (global 'frame:next)
      (begin (form-bind "apply" "click" commit-interface)
	     (form-bind "reset" "click" reset-interface)))))

(frame:on-back (lambda() (or (commit-interface) 'cancel)))
(frame:on-next (lambda() (or (commit-interface) 'cancel)))

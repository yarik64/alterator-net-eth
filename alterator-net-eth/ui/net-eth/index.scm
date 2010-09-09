(document:surround "/std/frame")

;;; Functions

;      '("addresses" "default")
(define (update-configuration configuration)
    (form-update-activity
      '("default")
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
      '("adaptor" "default" "configuration")
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
    (form-popup "/net-eth/advanced" 'name name)
    (form-update-enum "name" (woo-list "/net-eth/avail_ifaces"))
    (read-interface name)))

(define (wireless-interface)
  (format #t "wireless-interface:real_name=~S~%" (form-value "real_name"))
  (form-popup "/net-wifi/" 'iface (form-value "real_name")))

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

  (listbox name "name")
  (gridbox
    columns "0;100"

    ;;
    (textbox colspan 2 name "adaptor" max-height 60 alterability #f)

    ;;
    (label text (_ "Configuration:") align "right" nameref "configuration")
    (combobox name "configuration")

    ;;
    (label text (_ "Default gateway:") align "right" nameref "default")
    (edit name "default")

    ;;
    (separator colspan 2)
    (spacer)(label text (small (_ "(multiple values should be space separated)")))

    ;;
    (label text (_ "IP addresses:") align "right" nameref "addresses")
    (edit name "addresses")


    ;;
    (label text (_ "DNS servers:") nameref "dns" align "right")
    (edit name "dns")

    ;;
    (label text (_ "Search domains:") nameref "search" align "right")
    (edit name "search")

    ;;
    (separator colspan 2)
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
    (or (global 'frame:next)
      (begin (form-bind "apply" "click" commit-interface)
	     (form-bind "reset" "click" reset-interface)))))

(frame:on-back (lambda() (or (commit-interface) 'cancel)))
(frame:on-next (lambda() (or (commit-interface) 'cancel)))

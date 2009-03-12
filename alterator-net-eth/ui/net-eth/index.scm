(document:surround "/std/frame")

;;; Functions

(define (update-configuration configuration)
    (form-update-activity
      '("ip" "mask" "default")
      (string=? configuration "static")))

(define (read-interface name)
  (let ((cmd (woo-read-first "/net-eth" 'name name)))

    (form-update-value-list
      '("name")
      cmd)
    (form-update-value-list
      '("computer_name" "dns" "search")
      cmd)
    (form-update-value-list
      '("adaptor" "ip" "mask" "default" "hw_binding" "controlled" "configuration")
      cmd)

    (form-update-visibility
      "wireless"
      (woo-get-option cmd 'wireless))

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("computer_name" "dns" "search"
			    "ip" "mask" "default" "hw_binding" "controlled" "configuration"))))


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
      (form-update-enum "hw_binding" (woo-list "/net-eth/avail_hw_bindings"))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled"))
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

(define (wireless-interface)
 (form-popup "/net-wifi/" 'interface (form-value "name")))

;;; UI

(edit name "prev_name" text "" visibility #f)
(gridbox
  columns "0;100"
  margin 20

  (gridbox
    colspan 2
    columns "0;40;60"

    ;;
    (label text (_ "Computer name:") name "computer_name" align "right")
    (edit name "computer_name")
    (spacer)

    ;;
    (label colspan 3)

    ;;
    (label text (_ "DNS servers:") name "dns" align "right")
    (edit name "dns")
    (spacer)

    ;;
    (label text (_ "Search domains:") name "search" align "right")
    (edit name "search")
    (spacer)

    (spacer)
    (label text (string-append "<small>("
			       (_ "multiple values should be space separated")
			       ")<small>"))
    (spacer))

  (separator colspan 2)

  (label colspan 2 text (bold (_ "Interfaces")))

  (listbox name "name")
  (gridbox
    columns "0;100"

    ;;
    (textbox colspan 2 name "adaptor" max-height 60 alterability #f)

	;;
    (label text (_ "Controlled by:") align "right" name "controlled")
    (combobox name "controlled")
    ;;
    (label text (_ "Configuration:") align "right" name "configuration")
    (combobox name "configuration")
    ;;
    (label text (_ "IP address:") align "right" name "ip")
    (edit name "ip")

    ;;
    (label text (_ "Netmask:") align "right" name "mask")
    (combobox name "mask")

    ;;
    (label text (_ "Default gateway:") align "right" name "default")
    (edit name "default")

    ;;
    (label text (_ "Hardware binding:") align "right")
    (combobox name "hw_binding")

    ;;
    (spacer)
    (button text (_ "Wireless settings...")
	    name "wireless"
	    align "left"
	    visibility #f))

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
    (form-bind "wireless" "click" wireless-interface)
    (or (global 'frame:next)
      (begin (form-bind "apply" "click" commit-interface)
	     (form-bind "reset" "click" reset-interface)))))

(frame:on-back (lambda() (or (commit-interface) 'cancel)))
(frame:on-next (lambda() (or (commit-interface) 'cancel)))

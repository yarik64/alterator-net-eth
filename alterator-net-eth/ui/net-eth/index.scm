(document:surround "/std/frame")

;;; Functions

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
	 (form-value-list '("hostname" "dns" "search"
			    "ip" "mask" "default" "hw_binding" "configuration"))))


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
    (label text (_ "Host name:") name "hostname" align "right")
    (edit name "hostname")
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

  (label text (bold (_ "Interfaces")))
  (spacer)

  (listbox name "name")
  (gridbox
    columns "0;100"
    ;;
    (label text (_ "Status:") align "right")
    (label name "info")

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
    (label colspan 2)

    ;;
    (if (global 'frame:next)
      (label)
      (hbox align "left"
	    (button (_ "Apply") name "apply")
	    (button (_ "Reset") name "reset")))
    (spacer))

;;;;;;;;;;;;;;;;;;

(document:root
  (when loaded
    (reset-interface)
    (form-bind "name" "change" update-interface)
    (form-bind "configuration" "change" (lambda() (update-configuration (form-value "configuration"))))
    (form-bind "apply" "click" commit-interface)
    (form-bind "reset" "click" reset-interface)
    (form-bind "wireless" "click" wireless-interface)))

(frame:on-back (lambda() (or (commit-interface) 'cancel)))
(frame:on-next (lambda() (or (commit-interface) 'cancel)))

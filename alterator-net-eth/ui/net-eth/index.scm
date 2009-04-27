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
      '("adaptor" "ip" "mask" "default" "configuration")
      cmd)

    (update-configuration (woo-get-option cmd 'configuration))))

(define (write-interface name)
  (apply woo-write
	 "/net-eth"
	 'name name
	 (form-value-list '("computer_name" "dns" "search"
			    "ip" "mask" "default" "configuration"))))

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
 (form-popup "/net-eth/advanced" 'name (form-value "name")))

;;; UI

width 400
height 200

(edit name "prev_name" text "" visibility #f)
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
    (label text (_ "IP address:") align "right" nameref "ip")
    (edit name "ip")

    ;;
    (label text (_ "Netmask:") align "right" nameref "mask")
    (combobox name "mask")

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
    (spacer)
    (label text (small (_ "(multiple values should be space separated)")))

    ;;
    (label colspan 2)

    ;;
    (spacer)
    (button text (_ "Advanced...") name "advanced" align "right")

    ;;
    (or (global 'frame:next)
		(label colspan 2)))

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
    (or (global 'frame:next)
      (begin (form-bind "apply" "click" commit-interface)
	     (form-bind "reset" "click" reset-interface)))))

(frame:on-back (lambda() (or (commit-interface) 'cancel)))
(frame:on-next (lambda() (or (commit-interface) 'cancel)))

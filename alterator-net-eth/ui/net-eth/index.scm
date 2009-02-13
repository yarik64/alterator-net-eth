(document:surround "/std/frame")
(document:insert "/std/functions")

;;; Functions

(define *prev-current* (make-cell 0))

(define (read-interface name)
  (and (string? name)
       (let ((cmd (woo-read-first "/net-eth" 'ifname name)))

         (form-update-value-list
	   '("hostname" "dns" "search")
	   cmd)
	 (form-update-value-list
	   '("info" "ip" "mask" "default" "hw_binding" "configuration")
	   cmd)

	 (form-apply "configuration" selected)

	 (w-button activity (woo-get-option cmd 'wireless)))))

(define (write-interface name)
    (and (string? name)
	 (apply woo-write
		"/net-eth"
		'ifname name
		(form-value-list))))

(define (commit-interface)
  (let ((name (or (ifaces value) "")))
    (catch/message
      (thunk
       (write-interface name)
       (woo-write "/net-eth" 'commit #t)))))

(define (reset-interface)
  (catch/message
    (thunk
      (woo-write "/net-eth" 'reset #t)
      (and (global 'frame:next) (woo-write "/net-eth" 'reset #t))

      (form-update-enum "mask" (woo-list "/net-eth/avail_masks"))
      (form-update-enum "hw_binding" (woo-list "/net-eth/avail_hw_bindings"))
      (form-update-enum "configuration" (woo-list "/net-eth/avail_configurations"))
      (form-update-enum "name" (woo-list "/net-eth/avail_ifaces"))


      (read-interface "")
      (cell-set! *prev-current* "")
      (or (zero? (ifaces count)) (ifaces current 0)))))

;;; UI

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
     (spacer)
     )


  (separator colspan 2)

  (label text (bold (_ "Interfaces")))
  (spacer)

  (document:id ifaces (listbox
			name "name"
			(when selected
			  (or (catch/message
				(thunk
				  (write-interface (cell-ref *prev-current*))
				  (read-interface (ifaces value))
				  (cell-set! *prev-current* (ifaces value))
				  (update-effect)))
			      (ifaces value (cell-ref *prev-current*))))))
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
    (document:id w-button (button text (_ "Wireless settings...")
				  align "left"
				  activity #f
				  (when clicked
				    (let ((name (ifaces value)))
				      (and (not-empty-string? name)
					   (document:popup "/net-wifi/" 'interface name))))))


    )

    ;;
    (label colspan 2)

    ;;
    (if (global 'frame:next)
      (label)
      (hbox align "left"
	    (button (_ "Apply") (when clicked (commit-interface)))
	    (button (_ "Reset") (when clicked (reset-interface) (update-effect)))))
    (spacer)
    )


;;;;;;;;;;;;;;;

;; TODO: replace with effect-enable
(effect-enable "ip" "configuration" "static")
(effect-enable "mask" "configuration" "static")
(effect-enable "default" "configuration" "static")

;;;;;;;;;;;;;;;;;;

(document:root
  (when loaded (reset-interface)
               (init-effect)))

(frame:on-back (thunk (or (commit-interface) 'cancel)))
(frame:on-next (thunk (or (commit-interface) 'cancel)))

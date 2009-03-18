(document:surround "/std/frame")

(define *name* (global 'name))

(define (wireless-interface)
 (form-popup "/net-wifi/" 'interface *name*))

;;; Functions
(define (ui-read)
  (catch/message
    (lambda()
      (let ((cmd (woo-read-first "/net-eth" 'name *name*)))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled"))
      (form-update-enum "hw_binding" (woo-list "/net-eth/avail_hw_bindings"))
      (form-update-value "name" *name*)
      (form-update-visibility "wireless" (woo-get-option cmd 'wireless))

      (form-update-value-list '("controlled" "hw_binding") cmd)))))

(define (ui-exit)
  (document:end))

(define (ui-write)
  (catch/message
    (lambda()
      (woo-write "/net-eth"
		 'name *name*
		 'hw_binding (form-value "hw_binding")
		 'controlled (form-value "controlled"))
      (ui-exit))))

;;; UI

width 500
height 300

(gridbox
  columns "0;100"
  margin "10"

  ;;
  (label text (_ "Interface:") align "right")
  (label name "name")

  ;;
  (label text (_ "Network subsystem:") align "right" name "controlled")
  (combobox name "controlled")

  ;;
  (label text (_ "Hardware binding:") align "right")
  (combobox name "hw_binding")

  ;;
  (spacer)
  (button text (_ "Wireless settings...")
	  name "wireless"
	  align "left"
	  visibility #f)

  ;;
  (label colspan 2)

  ;;
  (hbox align "left"
	colspan 2
	(button (_ "OK") name "ok")
	(button (_ "Cancel") name "cancel")))

;;
(document:root
  (when loaded
    (ui-read)
    (form-bind "wireless" "click" wireless-interface)
    (form-bind "ok" "click" ui-write)
    (form-bind "cancel" "click" ui-exit)))

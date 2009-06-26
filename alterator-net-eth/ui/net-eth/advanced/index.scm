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
      (form-update-value "name" *name*)
      (form-update-visibility "wireless" (woo-get-option cmd 'wireless))

      (form-update-value-list '("controlled") cmd)))))

(define (ui-exit)
  (document:end))

(define (ui-write)
  (catch/message
    (lambda()
      (woo-write "/net-eth"
		 'name *name*
		 'controlled (form-value "controlled"))
      (ui-exit))))

;;; UI

width 600
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
  (spacer)
  (button text (_ "Wireless settings...")
	  name "wireless"
	  align "left"
	  visibility #f)

  ;;
  (label colspan 2)

  ;;
  (spacer)
  (hbox align "left"
	(button (_ "OK") name "ok")
	(button (_ "Cancel") name "cancel")))

;;
(document:root
  (when loaded
    (ui-read)
    (form-bind "wireless" "click" wireless-interface)
    (form-bind "ok" "click" ui-write)
    (form-bind "cancel" "click" ui-exit)))

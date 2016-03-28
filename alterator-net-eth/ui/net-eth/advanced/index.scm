(document:surround "/std/frame")

(define *name* (global 'name))

;;; Functions
(define (ui-read)
  (catch/message
	(lambda()
	  (let ((cmd (woo-read-first "/net-eth" 'name  *name*)))
		(form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'name *name*))
		(form-update-value-list '("name" "controlled" "onboot") cmd)))))

(define (ui-exit)
  (document:end))

(define (ui-write)
  (catch/message
    (lambda()
      (woo-write "/net-eth"
		 'name (form-value "name")
		 'controlled (form-value "controlled")
		 'onboot (form-value "onboot"))
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
  (label text (_ "Startup this interface at boot") align "right")
  (checkbox name "onboot")
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
    (form-bind "ok" "click" ui-write)
    (form-bind "cancel" "click" ui-exit)))

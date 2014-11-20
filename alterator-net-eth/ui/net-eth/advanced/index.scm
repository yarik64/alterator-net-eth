(document:surround "/std/frame")

(define *name* (global 'name))

;;; Functions
(define (ui-read)
  (catch/message
    (lambda()
      (let* ((cmd (woo-read-first "/net-eth" 'name  *name*))
             (is_bridge (woo-get-option cmd 'bridge)))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'bridge is_bridge))

      (form-update-value-list '("name" "controlled" "bridge") cmd)))))

(define (ui-exit)
  (document:end))

(define (ui-write)
  (catch/message
    (lambda()
      (woo-write "/net-eth"
		 'name (form-value "name")
		 'controlled (form-value "controlled")
         'bridge (form-value "bridge"))
      (ui-exit))))

(define (bridge-changed)
  (let* ((name (form-value "name"))
         (is_bridge (form-value "bridge"))
         (new-name (if is_bridge
                     (string-append "br" name)
                     (substring name 2)))
         (cmd (woo-read-first "/net-eth/controlled" 'name name 'bridge is_bridge)))
    (form-update-value "name" new-name)
    (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'bridge is_bridge))
    (form-update-value "controlled" (woo-get-option cmd 'controlled))))

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
  (label text (_ "Use interface as bridge") align "right" name "bridge")
  (checkbox name "bridge")
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
    (form-bind "bridge" "change" bridge-changed)
    (form-bind "ok" "click" ui-write)
    (form-bind "cancel" "click" ui-exit)))

(define-module (ui net-eth advanced ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

(define (ui-read name)
  (catch/message
    (lambda()
      (let* ((cmd (woo-read-first "/net-eth" 'name name))
	     (is_bridge (woo-get-option cmd 'bridge))
		 (iface-type (woo-get-option cmd 'iface_type)))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'bridge is_bridge 'name name))
      (form-update-value "iface" name)
	  (form-update-activity "bridge" (or (string-ci=? iface-type "eth")
										 (string-ci=? iface-type "bri")))
      (form-update-value-list '("name" "controlled" "bridge") cmd)))))


(define (ui-exit)
  (form-replace (format #f "/net-eth?iface=~A" (form-value "name"))))

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
    (form-update-value "iface" new-name)
    (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'bridge is_bridge))
    (form-update-value "controlled" (woo-get-option cmd 'controlled))))

(define (init)
  (ui-read (form-value "iface"))
  (form-bind "bridge" "change" bridge-changed)
  (form-bind "ok" "click" ui-write)
  (form-bind "cancel" "click" ui-exit))

(define-module (ui net-eth vlan ajax)
    :use-module (alterator str)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

(define (ui-read name)
      (form-update-value "iface" name))

; FILL VLAN LIST
(define (refresh-vlan)
  ;(format #t "[net-eth/vlan] item ~S\n" (form-value "list"))
  (form-update-enum "list"
    (woo-list "/net-eth/list_host_vlans2"
      'name (form-value "iface")
      'language (form-value "language")))
  (if
      (not (form-value "list"))
      (begin
	(form-update-value "name" "")
	(form-update-value "vlan_name" "")
	(form-update-value "vlan_vid" "")
        (form-update-activity "vlan_name" #f)
        (form-update-activity "vlan_vid" #f)
        (form-update-activity "apply" #f)
        (form-update-activity "reset" #f))
      (begin
	(form-update-value "name" (form-value "list"))

	; Read vlan info
	(form-update-value "vlan_name" (form-value "list"))
	(form-update-value "vlan_vid"
	  (woo-get-option
	    (woo-read-first "/net-eth"
		'name (form-value "list")
		'language (form-value "language"))
	    'label_vlan_vid))

        (form-update-activity "vlan_name" #t)
        (form-update-activity "vlan_vid" #t)
        (form-update-activity "apply" #t)
        (form-update-activity "reset" #t))))


; REMOVE SELECTED VLAN INTERFACE
(define (vlan-remove)
  (if
    (not-empty-string? (form-value "name"))
      (begin
	(woo "vlan_remove_infants" "/net-eth"
	  'name (form-value "iface")
	  'infants (list (form-value "name") ))
	; Update interface fields
	(form-update-value "name" "")
	(form-update-value "vlan_name" "")
	(form-update-value "vlan_vid" "")
	(form-update-activity "vlan_name" #f)
	(form-update-activity "vlan_vid" #f)
	(form-update-activity "apply" #f)
	(form-update-activity "reset" #f)
	(refresh-vlan))))


; ADD NEW VLAN INTERFACE FOR SELECTED HOST
(define (vlan-add)
  (form-update-value "name" "")
  (form-update-value "vlan_name" "")
  (form-update-value "vlan_vid" "")
  (form-update-activity "vlan_name" #t)
  (form-update-activity "vlan_vid" #t)
  (form-update-activity "apply" #t)
  (form-update-activity "reset" #t))

; VLAN PARAMETERS APPLY
(define (vlan-apply)
  ;(format #t "[net-eth/vlan] ~S\n" (form-value "name"))
  (if
    (empty-string? (form-value "name"))
      (begin ; add new vlan
	;(format #t "[net-eth/vlan] vlan_add_new\n")
	(woo "vlan_add_new" "/net-eth"
		'name (form-value "iface")
		'vlan_new_name (form-value "vlan_name")
		'vlan_new_vid (form-value "vlan_vid"))
	(form-update-value "name" (form-value "vlan_name")))
      (begin ; modify existing vlan
	;(format #t "[net-eth/vlan] vlan_edit\n")
	(woo "vlan_edit" "/net-eth"
		'name (form-value "iface")
		'vlan_old_name (form-value "name")
		'vlan_new_name (form-value "vlan_name")
		'vlan_new_vid  (form-value "vlan_vid"))))
  (refresh-vlan))

; ON VLAN CHANGE
(define (on-vlan-change)
  (refresh-vlan))

; RETURN TO MAIN WINDOW
(define (ui-exit)
  (form-replace (format #f "/net-eth?iface=~A" (form-value "iface"))))

; INIT
(define (init)
  (ui-read (form-value "iface"))
  (form-bind "back" "click" ui-exit)
  (form-bind "list"  "change" on-vlan-change)
  (form-bind "add_vlan" "click" vlan-add)
  (form-bind "remove_vlan" "click" vlan-remove)
  (form-bind "apply" "click" vlan-apply)
  (on-vlan-change))

; vim: ft=lisp expandtab

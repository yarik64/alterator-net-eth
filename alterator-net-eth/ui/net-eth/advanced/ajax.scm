(define-module (ui net-eth advanced ajax)
    :use-module (alterator ajax)
    :use-module (alterator woo)
    :export (init))

; DEBUG STRING (PASTE IN FOR YOUR NEEDS).
; (format #t "XXX: ~S\n" rdp_profile_name)
; (format #t "[ajax, module] action info\n")

; TO DEBUG DO:
; 1) define *debug* to #t
; 2) start ahttpd -l -d
(define *debug* #f)
(define (dmsg msg . args)
  (if *debug*
    (format #t "[ajax, net-eth-advanced]: ~S ~S\n" msg args)))


(define (ui-read name)
  (catch/message
    (lambda()
      (let* ((cmd (woo-read-first "/net-eth" 'name name))
       (has_infant_vlans (woo-get-option cmd 'has_infant_vlans #f))
	     (is_bridge (woo-get-option cmd 'bridge)))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'bridge is_bridge))
      (form-update-value "iface" name)
      (form-update-visibility "area-vlan-list" has_infant_vlans)
      (form-update-visibility "area-vlan-edit" has_infant_vlans)
      (form-update-activity "bridge" (not has_infant_vlans))
      (form-update-visibility "label_bridge" has_infant_vlans)
      (form-update-value-list '("name" "controlled" "bridge") cmd)

      ; VLAN
      (and
        has_infant_vlans

        ; FILL LISTS
        (for-each
          (lambda(lst)
            (let ((data (woo-list (string-append "/net-eth/" lst)
                                  'name name
                                  'language (form-value "language"))))
              (form-update-enum lst data)
              ; take first, make it active
              (and
                (pair? data)
                ; select first item only for general enums, not multi-select lists
                (= (length (car data)) 5)
                (let ((xxx (woo-get-option (car data) 'name)))
                  (form-update-value lst (if (string? xxx) xxx ""))))
              ))
          '("list_host_vlans" "list_host_vlans2"))

        ; REBIND ACTIONS ON LISTS
        ;(form-bind "list_host_vlans2" "change" vlan_selected)

        ; RESET INTERFACE
        (for-each (lambda(p) (form-update-value p ""))
                  '("vlan_new_name"
                    "vlan_new_vid"))

        (vlan_selected)))

			)))




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


; REMOVE SELECTED VLAN INTERFACES
(define (vlan-remove-selected)

  (dmsg "vlan-remove-selected() -- enter")

  (catch/message
    (lambda()
      (woo "vlan_remove_infants" "/net-eth"
                 'name (form-value "name")
                 'infants (form-value "list_host_vlans"))))

  (ui-read (form-value "iface")))


; ADD NEW VLAN INTERFACE FOR SELECTED HOST
(define (vlan-add-new)

  (dmsg "vlan-add-new() -- enter")

  (catch/message
    (lambda()
      (woo "vlan_add_new" "/net-eth"
                 'name (form-value "name")
                 'vlan_new_name (form-value "vlan_new_name")
                 'vlan_new_vid (form-value "vlan_new_vid"))))

  (ui-read (form-value "iface")))


; VLAN EDIT
(define (vlan-edit)

  (dmsg "vlan-edit() -- enter")

  (catch/message
    (lambda()
      (woo "vlan_edit" "/net-eth"
                 'name (form-value "name")
                 'vlan_old_name (form-value "list_host_vlans2")
                 'vlan_new_name (form-value "vlan_new_name_edit")
                 'vlan_new_vid (form-value "vlan_new_vid_edit"))))

  (ui-read (form-value "iface")))


; REACT TO SELECTED VLAN
(define (vlan_selected)

  (dmsg "vlan_selected() -- enter")

  (let ((vlan (form-value "list_host_vlans2" "")))
    (if (not (string-null? vlan))
      (catch/message
        (lambda()
          (let* ((cmd (woo "vlan_info" "/net-eth" 'name vlan))
                 (vid (woo-get-option (car cmd) 'vid))
                 (host (woo-get-option (car cmd) 'host))
                 (name (woo-get-option (car cmd) 'name)))
            (form-update-value "vlan_new_name_edit" name)
            (form-update-value "vlan_new_vid_edit" vid)))))))


; INIT
(define (init)
  (ui-read (form-value "iface"))
  (form-bind "bridge" "change" bridge-changed)
  (form-bind "list_host_vlans2" "change" vlan_selected)
  (form-bind "ok" "click" ui-write)
  (form-bind "btn_remove_selected_vlans" "click" vlan-remove-selected)
  (form-bind "btn_vlan_add_new" "click" vlan-add-new)
  (form-bind "btn_vlan_edit" "click" vlan-edit)
  (form-bind "cancel" "click" ui-exit))

; vim: ft=lisp expandtab

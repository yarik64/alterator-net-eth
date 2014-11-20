(document:surround "/std/frame")

(define *name* (global 'name))


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

;;; Functions
(define (ui-read)
  (catch/message
    (lambda()
      (let* ((cmd (woo-read-first "/net-eth" 'name  *name*))
             (has_infant_vlans (woo-get-option cmd 'has_infant_vlans #f))
             (is_bridge (woo-get-option cmd 'bridge))
             (is_etcnet (if (string-ci=? (woo-get-option cmd 'controlled) "etcnet") #t #f))
             (is_wireless (woo-get-option cmd 'wireless #f)))
      (form-update-enum "controlled" (woo-list "/net-eth/avail_controlled" 'bridge is_bridge))
      (form-update-visibility "area-vlan" (and is_etcnet (not is_wireless)))
      (form-update-visibility "area-vlan-list" (and is_etcnet has_infant_vlans))
      (form-update-visibility "area-vlan-edit" (and is_etcnet has_infant_vlans))
      (form-update-visibility "label_bridge" has_infant_vlans)
      (form-update-activity "bridge" (not has_infant_vlans))
      (form-update-activity "controlled" (not has_infant_vlans))
      (form-update-value-list '("name" "controlled" "bridge") cmd)

      ; VLAN
      (and
        has_infant_vlans

        ; FILL LISTS
        (for-each
          (lambda(lst)
            (let* ((name (car lst))
                   (select_first (car (cdr lst)))
                   (data (woo-list (string-append "/net-eth/" name)
                                   'name *name*
                                   'language (form-value "language"))))
              (form-update-enum name data)
              ; take first, make it active
              (and
                select_first
                (pair? data)
                ; select first item only for general enums, not multi-select lists
                (= (length (car data)) 5)
                (let ((xxx (woo-get-option (car data) 'name)))
                  (form-update-value lst (if (string? xxx) xxx ""))))
              ))
          '(("list_host_vlans_single" #f) ("list_host_vlans2" #t)))

        ; REBIND ACTIONS ON LISTS
        (form-bind "list_host_vlans2" "change" vlan_selected)

        ; RESET INTERFACE
        (for-each (lambda(p) (form-update-value p ""))
                  '("vlan_new_name"
                    "vlan_new_vid"))

        (vlan_selected))))))

; REMOVE SELECTED VLAN INTERFACES
(define (vlan-remove-selected)

  (dmsg "vlan-remove-selected() -- enter")

  (catch/message
    (lambda()
      (woo "vlan_remove_infants" "/net-eth"
           'name (form-value "name")
           'infants (form-value "list_host_vlans_single"))))

  (ui-read))


; ADD NEW VLAN INTERFACE FOR SELECTED HOST
(define (vlan-add-new)

  (dmsg "vlan-add-new() -- enter")

  (catch/message
    (lambda()
      (woo "vlan_add_new" "/net-eth"
           'name (form-value "name")
           'vlan_new_name (form-value "vlan_new_name")
           'vlan_new_vid (form-value "vlan_new_vid"))))

  (ui-read))


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

  (ui-read))


; REACT TO SELECTED VLAN
(define (vlan_selected)

  (dmsg "vlan_selected() -- enter")

  (let ((vlan (form-value "list_host_vlans2")))
    (if (not (string-null? vlan))
      (catch/message
        (lambda()
          (let* ((cmd (woo "vlan_info" "/net-eth" 'name vlan))
                 (vid (woo-get-option (car cmd) 'vid))
                 (host (woo-get-option (car cmd) 'host))
                 (name (woo-get-option (car cmd) 'name)))
            (form-update-value "vlan_new_name_edit" name)
            (form-update-value "vlan_new_vid_edit" vid)))))))



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
height 500

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
  (hbox align "left" (checkbox name "bridge")
       (label text (_ "- remove infants to change") name "label_bridge"))
  ;;
  (label colspan 2)

  ;;
  (spacer)
  (hbox align "left"
	(button (_ "OK") name "ok")
	(button (_ "Cancel") name "cancel")))

  ;;
  (vbox colspan 2 align "center" name "area-vlan"
        (separator)
        (label align "center" text (bold (_ "Add new child VLAN interface")))
        (hbox
          (label text (_ "Name for new interface:"))
          (edit name "vlan_new_name")
          (label text (_ "VID (1-4095):"))
          (edit name "vlan_new_vid")
          (button (_ "Add") name "btn_vlan_add_new")
          ))

  (separator)

  ;;
  (vbox colspan 2 align "center" name "area-vlan-list"
      (label align "center" text (bold (_ "List of infant VLAN interfaces")))

      (document:id vlans (checklistbox name "list_host_vlans_single"))
      (vlans columns 1)
      (vlans header (vector (_ "Infant interface name -- VLAN ID")))

      ;    form-update-enum

      (button (_ "Remove selected vlans") align "center" name "btn_remove_selected_vlans"))

  ;;
  (separator)

  ;;
  (hbox colspan 2 align "center" name "area-vlan-edit"
      (combobox name "list_host_vlans2")
      (label text (_ "Rename to:"))
      (edit name "vlan_new_name_edit")
      (label text (_ "VID (1-4095):"))
      (edit name "vlan_new_vid_edit")
      (button (_ "Save") align "center" name "btn_vlan_edit"))


;;
(document:root
  (when loaded
    (ui-read)
    (form-bind "bridge" "change" bridge-changed)
    (form-bind "list_host_vlans2" "change" vlan_selected)
    (form-bind "btn_remove_selected_vlans" "click" vlan-remove-selected)
    (form-bind "btn_vlan_add_new" "click" vlan-add-new)
    (form-bind "btn_vlan_edit" "click" vlan-edit)
    (form-bind "ok" "click" ui-write)
    (form-bind "cancel" "click" ui-exit)))

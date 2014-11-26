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
    (format #t "[ajax, net-eth-vlan]: ~S ~S\n" msg args)))


(define (ui-read name)
      (form-update-value "iface_label" (bold name))
      (form-update-value "iface" name))

; FILL VLAN LIST
(define (refresh-vlan)
  (let ((name (form-value "list")))
  ;(format #t "[net-eth/vlan] item ~S\n" name)
  (form-update-enum "list"
    (woo-list "/net-eth/list_host_vlans2"
      'name (form-value "iface")
      'language (form-value "language")))
  (if
      (not name)
      (begin
	(form-update-value "name" "")
	(form-update-value "vlan_name" "")
	(form-update-value "vlan_vid" "")
        (form-update-activity "vlan_name" #f)
        (form-update-activity "vlan_vid" #f)
        (form-update-activity "apply" #f)
        (form-update-activity "reset" #f))
      (begin
	(form-update-value "name" name)

	; Read vlan info
	(form-update-value "vlan_name" name)
	(form-update-value "vlan_vid"
	  (woo-get-option
	    (woo-read-first "/net-eth"
		'name name
		'language (form-value "language"))
	    'label_vlan_vid))

        (form-update-activity "vlan_name" #t)
        (form-update-activity "vlan_vid" #t)
        (form-update-activity "apply" #t)
        (form-update-activity "reset" #t)))))


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

(define (ui-exit)
  (document:end))

;;; UI

width 600
height 500

(hbox align "left"
	  (label text (bold (_ "VLAN Configuration for Interface ")))(label name "iface_label"))
(label name "iface" visibility #f)
(label name "name" visibility #f)
(button (_ "Back") name "back")

(separator)
(gridbox
  columns "60;40"
(listbox name "list" max-width 155)
  ;;
(gridbox
  columns "0;100"
          (label text (_ "VLAN Name:"))
          (edit name "vlan_name")
          (label text (_ "VID (1-4095):"))
          (edit name "vlan_vid")
		  (spacer)
  (hbox align "right"
		(button (_ "Apply") name "apply")
		(button (_ "Reset") name "reset"))
          )
  ;;
  (hbox align "left"
	  (button (_ "Add VLAN") name "add_vlan")
      (button (_ "Remove") name "remove_vlan")))

;;
(document:root
  (when loaded
  (ui-read *name*)
  (form-bind "back" "click" ui-exit)
  (form-bind "list"  "change" on-vlan-change)
  (form-bind "add_vlan" "click" vlan-add)
  (form-bind "remove_vlan" "click" vlan-remove)
  (form-bind "apply" "click" vlan-apply)
  (on-vlan-change)))

; ============================================================================
; CONTROL FREAK SYSTEM BUILDER
; Command: CFBUILD
;
; Type what you want, pick a point, and the blocks are placed and wired:
;   1 eDS10 4P, 2 UBi, 1 0-10V
;   eDS10 6P, 3 UBi, 2x LIDA D, SLAMMO, HDR60
;   eDS10 4P, 2 UBi, 2 UBi @DA2
;
; - Items are separated by commas; the quantity is optional (default 1)
;   and can be written "2" or "2x"
; - Exactly one controller (eDS10 4P / 6P)
; - A power supply (DEFAULT_PSU) is added unless you list one (HDR15/30/60)
; - DALI devices go on controller line DEFAULT_DALI_LINE, DMX devices on
;   DEFAULT_DMX_LINE. Add "@DAn" to an item to use controller line n instead
; - Each device connects with its own data line 1 (e.g. UBi line 1 = in)
; - Everything is placed left to right: power supply, controller, devices
;   in the order typed. One undo (U / Ctrl+Z) removes the whole build.
;
; Requires: SmartJumper_Lib, AutoPowerBus, DataLineBus (loaded by ControlFreak.lsp)
; ============================================================================

(vl-load-com)

; --- CONFIGURATION ----------------------------------------------------------

(setq cfb:*default-psu*       "HDR30")  ; Catalog key added when no PSU is listed
(setq cfb:*default-dali-line* 1)        ; Controller line for DALI devices
(setq cfb:*default-dmx-line*  2)        ; Controller line for DMX devices
(setq cfb:*gap*               25.0)     ; Horizontal gap between blocks (mm)

(setq cfb:*prompt* "\nSystem, e.g. 1 eDS10 4P, 2 UBi, 1 0-10V  (? = products, F2 = full guide): ")

; DIN rail, drawn behind the units on its own layer (ignored by wire bridging)
(setq cfb:*rail-offset*       25.27)    ; Rail bottom edge above the units' base points (mm)
(setq cfb:*rail-overhang*     20.0)     ; Rail beyond the first and last unit (mm)
(setq cfb:*rail-layer*        "CF-DINRAIL")

; Frame and header. The frame margin scales with the frame; the header is always
; inserted at real size (scale 1), as are its inset and gap
(setq cfb:*frame-block*       "FRAME_BLOCK")
; Frame outline at scale 1 relative to its base point: ((minX minY) (maxX maxY)).
; Set here rather than measured - CAD extents of the frame block can include
; its attribute text and come out bigger than the visible frame.
(setq cfb:*frame-outline*     '((-210.0 -148.5) (210.0 148.5)))   ; A3, base point at centre
(setq cfb:*header-block*      "CF_HEADER")
(setq cfb:*frame-margin*      15.0)     ; Content to frame edge (mm)
(setq cfb:*header-inset*      12.0)     ; Header to frame edge (mm)
(setq cfb:*header-gap*        5.0)      ; Header to content (mm)
(setq cfb:*frame-scales*      '(1.0 1.25 1.5 2.0 2.5 3.0 4.0 5.0 6.0 8.0 10.0))

; Product catalog: (key block role protocol (aliases...))
;   role: CONTROLLER, DEVICE or PSU. protocol: DALI / DMX (devices only)
; Aliases are matched ignoring case, spaces, hyphens, underscores and dots,
; so "eDS10 4P", "EDS10-4P" and "eds104p" all match "EDS10 4P".
(setq cfb:*catalog*
  '(("eDS10 4P"     "CF_EDIDIO_4P"    "CONTROLLER" nil    ("EDS10 4P" "EDIDIO 4P" "S10 4P" "4P"))
    ("eDS10 6P"     "CF_EDIDIO_6P"    "CONTROLLER" nil    ("EDS10 6P" "EDIDIO 6P" "S10 6P" "6P"))
    ("UBi"          "CF_UBI_TEST"     "DEVICE"     "DALI" ("UBI"))
    ("UBi Power"    "CF_UBI_POWER"    "DEVICE"     "DALI" ("UBI POWER" "UBI PWR" "UBIP"))
    ("0-10V"        "CF_DALI_0_10_V2" "DEVICE"     "DALI" ("0-10V" "0-10" "010V" "DALI 0-10V" "0-10V V2"))
    ("0-10V V1"     "CF_DALI_0_10"    "DEVICE"     "DALI" ("0-10V V1" "DALI 0-10V V1"))
    ("LIDA D"       "CF_LIDA_D"       "DEVICE"     "DALI" ("LIDA D" "LIDA"))
    ("SLAMMO V6"    "CF_SLAMMO_V6"    "DEVICE"     "DMX"  ("SLAMMO V6" "SLAMMO"))
    ("DMX Splitter" "CF_DMX_SPLITTER" "DEVICE"     "DMX"  ("DMX SPLITTER" "SPLITTER"))
    ("HDR15"        "ADM_HDR15-24"    "PSU"        nil    ("HDR15" "HDR15-24"))
    ("HDR30"        "ADM_HDR30-24"    "PSU"        nil    ("HDR30" "HDR30-24"))
    ("HDR60"        "ADM_HDR60-24"    "PSU"        nil    ("HDR60" "HDR60-24"))))

; Catalog entry accessors
(defun cfb:key   (e) (nth 0 e))
(defun cfb:block (e) (nth 1 e))
(defun cfb:role  (e) (nth 2 e))
(defun cfb:proto (e) (nth 3 e))

; --- PARSING ----------------------------------------------------------------

; Uppercase without spaces, hyphens, underscores or dots
(defun cfb:norm (s)
  (vl-list->string
    (vl-remove-if '(lambda (c) (member c '(32 45 95 46)))
                  (vl-string->list (strcase s))))
)

(defun cfb:words (s / res w)
  (foreach c (vl-string->list (strcat s " "))
    (if (= c 32)
      (progn (if w (setq res (cons (vl-list->string (reverse w)) res))) (setq w nil))
      (setq w (cons c w))
    )
  )
  (reverse res)
)

(defun cfb:split (str delim / pos res)
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res))
    (setq str (substr str (+ pos 1 (strlen delim))))
  )
  (reverse (cons str res))
)

(defun cfb:find (name / n)
  (setq n (cfb:norm name))
  (vl-some '(lambda (e)
              (if (or (= n (cfb:norm (cfb:key e)))
                      (vl-some '(lambda (a) (= n (cfb:norm a))) (nth 4 e)))
                e))
           cfb:*catalog*)
)

; One item, e.g. "2 UBi @DA2" -> (entry qty line) ; line nil = default
; Returns a string (error message) if it can't be understood
(defun cfb:parse-item (item / words qty line nameWords entry)
  (setq words (cfb:words (vl-string-trim " \t" item)))
  (setq qty 1)

  ; Leading quantity: "2" or "2x"
  (if (and words (wcmatch (strcase (car words)) "#,##,#X,##X"))
    (setq qty (atoi (car words))
          words (cdr words))
  )

  ; "@DA2" / "@2" -> controller line override; the rest is the product name
  (foreach w words
    (if (= (substr w 1 1) "@")
      (setq line (atoi (vl-string-left-trim "@DALINEdaline" w)))
      (setq nameWords (append nameWords (list w)))
    )
  )

  (cond
    ((not nameWords) (strcat "'" item "' - no product name"))
    ((< qty 1) (strcat "'" item "' - quantity must be at least 1"))
    ((and line (< line 1)) (strcat "'" item "' - use @DA1, @DA2..."))
    ((not (setq entry (cfb:find (cfb:join-words nameWords))))
      (strcat "'" (cfb:join-words nameWords) "' - unknown product"))
    (T (list entry qty line))
  )
)

(defun cfb:join-words (words / res)
  (foreach w words (setq res (if res (strcat res " " w) w)))
  res
)

(defun cfb:product-list ( / res)
  (foreach e cfb:*catalog*
    (setq res (if res (strcat res ", " (cfb:key e)) (cfb:key e)))
  )
  res
)

(defun cfb:show-guide ()
  (princ "\n\n=== CONTROL FREAK SYSTEM BUILDER ===")
  (princ "\nList the products, separated by commas. For example:")
  (princ "\n    1 eDS10 4P, 2 UBi, 1 0-10V")
  (princ "\n    eDS10 6P, 3 UBi, 2x LIDA D, SLAMMO, HDR60")
  (princ "\n    eDS10 4P, 2 UBi, 2 UBi @DA2")
  (princ "\nRules:")
  (princ "\n  - One controller: eDS10 4P or eDS10 6P")
  (princ "\n  - Quantity before the name: 2 UBi  or  2x UBi  (default 1)")
  (princ "\n  - Power supply: add HDR15, HDR30 or HDR60, otherwise ")
  (princ (strcat cfb:*default-psu* " is added"))
  (princ (strcat "\n  - DALI devices go on controller DA" (itoa cfb:*default-dali-line*)
                 ", DMX devices on DA" (itoa cfb:*default-dmx-line*)))
  (princ "\n  - Add @DA2 (etc.) after an item to put it on a different controller line")
  (princ "\n  - Names ignore case, spaces and hyphens. Type ? to list them all.")
  (princ "\nAfter you press Enter you'll pick the bottom-left point. U undoes the whole build.")
)

(defun cfb:show-products ( / aliases)
  (princ "\n\nProduct          Type        Also accepted as")
  (foreach e cfb:*catalog*
    (setq aliases nil)
    (foreach a (nth 4 e)
      (setq aliases (if aliases (strcat aliases ", " a) a))
    )
    (princ (strcat "\n  " (cfb:pad (cfb:key e) 15)
                   (cfb:pad (cond ((= (cfb:role e) "CONTROLLER") "controller")
                                  ((= (cfb:role e) "PSU") "power")
                                  (T (cfb:proto e)))
                            12)
                   aliases))
  )
  (princ "\n")
)

(defun cfb:pad (s n)
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s
)

; --- PLACEMENT --------------------------------------------------------------

(defun cfb:insert (blockName pt) (cfb:insert-scaled blockName pt 1.0))

(defun cfb:insert-scaled (blockName pt scl / obj)
  (setq obj (vl-catch-all-apply 'vla-insertblock
              (list (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                    (vlax-3d-point pt) blockName scl scl scl 0.0)))
  (if (vl-catch-all-error-p obj)
    (progn
      (princ (strcat "\nError: couldn't insert " blockName ": " (vl-catch-all-error-message obj)))
      nil
    )
    (vlax-vla-object->ename obj)
  )
)

(defun cfb:set-layer (ent layer / d)
  (setq d (entget ent))
  (entmod (subst (cons 8 layer) (assoc 8 d) d))
)

(defun cfb:ensure-layer (name col)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
  )
)

; Sets an attribute value on an inserted block (tag match is case-insensitive)
(defun cfb:set-att (ent tag value / e d)
  (setq e (entnext ent))
  (while (and e (= "ATTRIB" (cdr (assoc 0 (setq d (entget e))))))
    (if (= (strcase (cdr (assoc 2 d))) (strcase tag))
      (progn
        (entmod (subst (cons 1 value) (assoc 1 d) d))
        (entupd ent)
      )
    )
    (setq e (entnext e))
  )
)

; Today as dd/mm/yyyy
(defun cfb:today ( / s)
  (setq s (rtos (getvar "CDATE") 2 6))
  (strcat (substr s 7 2) "/" (substr s 5 2) "/" (substr s 1 4))
)

; --- DIN RAIL ---------------------------------------------------------------

; Oblong slot 15 x 5.2 centred on cx,cy (inside a block definition)
(defun cfb:rail-slot (cx cy)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                 '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                 (list 10 (- cx 4.9) (- cy 2.6)) '(42 . 0.0)
                 (list 10 (+ cx 4.9) (- cy 2.6)) '(42 . 1.0)
                 (list 10 (+ cx 4.9) (+ cy 2.6)) '(42 . 0.0)
                 (list 10 (- cx 4.9) (+ cy 2.6)) '(42 . 1.0)))
)

; Block for a 35mm top-hat rail of the given length (rounded to 1mm):
; outline, the two fold lines and 25mm-pitch slots. Created once per length.
; Base point is the bottom-left corner. Returns the block name.
(defun cfb:rail-block (len / name x)
  (setq len (float (fix (+ len 0.5))))
  (setq name (strcat "CF_DIN_RAIL_" (itoa (fix len))))
  (if (not (tblsearch "BLOCK" name))
    (progn
      (entmake (list '(0 . "BLOCK") (cons 2 name) '(70 . 0) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(62 . 0)
                     '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                     '(10 0.0 0.0) (list 10 len 0.0) (list 10 len 35.0) '(10 0.0 35.0)))
      (entmake (list '(0 . "LINE") '(8 . "0") '(62 . 0) '(10 0.0 5.0 0.0) (list 11 len 5.0 0.0)))
      (entmake (list '(0 . "LINE") '(8 . "0") '(62 . 0) '(10 0.0 30.0 0.0) (list 11 len 30.0 0.0)))
      (setq x 12.5)
      (while (<= (+ x 7.5) (- len 5.0))
        (cfb:rail-slot x 17.5)
        (setq x (+ x 25.0))
      )
      (entmake '((0 . "ENDBLK")))
    )
  )
  name
)

; --- FRAME AND HEADER -------------------------------------------------------

; Combined extents of every entity created after marker: ((x0 y0) (x1 y1))
(defun cfb:extents-after (marker / e box x0 y0 x1 y1)
  (setq e (if marker (entnext marker) (entnext)))
  (while e
    (if (not (member (cdr (assoc 0 (entget e))) '("ATTRIB" "SEQEND" "VERTEX")))
      (if (setq box (cf:bbox e))
        (setq x0 (if x0 (min x0 (caar box)) (caar box))
              y0 (if y0 (min y0 (cadar box)) (cadar box))
              x1 (if x1 (max x1 (caadr box)) (caadr box))
              y1 (if y1 (max y1 (cadadr box)) (cadadr box)))
      )
    )
    (setq e (entnext e))
  )
  (if x0 (list (list x0 y0) (list x1 y1)))
)

(defun cfb:overlap-p (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)
  (and (< ax0 bx1) (< bx0 ax1) (< ay0 by1) (< by0 ay1))
)

; Finds the smallest frame scale where the content fits inside the margins
; with the header in a free corner, then inserts frame and header.
; Content sits against the left margin, at the bottom or the top.
; Returns the frame scale, or nil if there is no frame block.
; Scales an insert (and its attributes) about its insertion point so its
; scale factor becomes s
(defun cfb:set-scale (ent s / d cur)
  (setq d (entget ent))
  (setq cur (cond ((cdr (assoc 41 d))) (1.0)))
  (if (> (abs (- cur s)) 1e-6)
    (vl-catch-all-apply 'vla-scaleentity
      (list (vlax-ename->vla-object ent)
            (vlax-3d-point (cdr (assoc 10 d)))
            (/ s cur)))
  )
)

; Moves an entity by (dx dy)
(defun cfb:move (ent delta)
  (if (or (> (abs (car delta)) 1e-6) (> (abs (cadr delta)) 1e-6))
    (vl-catch-all-apply 'vla-move
      (list (vlax-ename->vla-object ent)
            (vlax-3d-point '(0.0 0.0 0.0))
            (vlax-3d-point (list (car delta) (cadr delta) 0.0))))
  )
)

(defun cfb:place-frame (box / fext hext baseFW baseFH baseHW baseHH W H s M I G FW FH HW HH
                              found cy hx hy corner scales ll ent fb hent hb)
  (setq fext (if (tblsearch "BLOCK" cfb:*frame-block*) cfb:*frame-outline*))
  (setq hext (if (tblsearch "BLOCK" cfb:*header-block*) (cf:block-extents cfb:*header-block*)))
  (cond
    ((not box) nil)
    ((not fext)
      (princ (strcat "\nFrame: " cfb:*frame-block* " not found - no frame added."))
      nil)
    (T
      ; NB AutoLISP names ignore case: baseFW (scale 1) must not be called fw, or it
      ; would be the same variable as FW (scaled) and grow on every pass
      (setq baseFW (- (caadr fext) (caar fext))  baseFH (- (cadadr fext) (cadar fext)))
      (setq baseHW (if hext (- (caadr hext) (caar hext)) 0.0)
            baseHH (if hext (- (cadadr hext) (cadar hext)) 0.0))
      (setq W (- (caadr box) (caar box))  H (- (cadadr box) (cadar box)))

      (setq scales cfb:*frame-scales*)
      (while (and scales (not found))
        (setq s (car scales)  scales (cdr scales))
        (setq FW (* baseFW s)  FH (* baseFH s)
              M (* cfb:*frame-margin* s)  I cfb:*header-inset*  G cfb:*header-gap*
              HW baseHW  HH baseHH)   ; header stays real size
        (if (and (<= W (- FW M M)) (<= H (- FH M M)))
          (foreach cyTry (list M (- FH M H))                       ; content at bottom, then top
            (foreach c '("TR" "BR" "TL" "BL")
              (if (not found)
                (progn
                  (setq hx (if (wcmatch c "?R") (- FW I HW) I))
                  (setq hy (if (wcmatch c "T?") (- FH I HH) I))
                  (if (or (not hext)
                          (not (cfb:overlap-p (- M G) (- cyTry G) (+ M W G) (+ cyTry H G)
                                              hx hy (+ hx HW) (+ hy HH))))
                    (setq found T  cy cyTry  corner c)
                  )
                )
              )
            )
          )
        )
      )

      (if (not found)
        (progn
          (princ "\nFrame: system is too big for the largest frame scale - frame added anyway.")
          (setq cy M  corner "TR"
                hx (- FW I HW)  hy (- FH I HH))
        )
      )

      ; Frame lower-left corner in the drawing
      (setq ll (list (- (caar box) M) (- (cadar box) cy)))

      (if (setq ent (cfb:insert-scaled cfb:*frame-block*
                      (list (- (car ll) (* (caar fext) s)) (- (cadr ll) (* (cadar fext) s)) 0.0)
                      s))
        (progn
          (cfb:set-att ent "DATE" (cfb:today))

          ; Some CADs ignore the insert scale on dynamic blocks - check the
          ; insert's own scale factor and fix it if needed
          (if (> (abs (- (cond ((cdr (assoc 41 (entget ent)))) (1.0)) s)) 1e-6)
            (cfb:set-scale ent s)
          )
        )
      )
      ; Header corner in the drawing (frame-local position + frame corner)
      (setq hx (+ (car ll) hx)  hy (+ (cadr ll) hy))
      (if hext
        (progn
          (setq hent (cfb:insert-scaled cfb:*header-block*
                       (list (- hx (caar hext)) (- hy (cadar hext)) 0.0)
                       1.0))
          ; Same check for the header
          (if (and hent (setq hb (cf:bbox hent)) (> (abs (- (- (caadr hb) (caar hb)) HW)) 1.0))
            (progn
              (cfb:set-scale hent 1.0)
              (if (setq hb (cf:bbox hent))
                (cfb:move hent (list (- hx (caar hb)) (- hy (cadar hb)))))
            )
          )
        )
      )
      (princ (strcat "\nFrame: scale " (rtos s 2 2) ", header in the "
                     (cdr (assoc corner '(("TR" . "top-right") ("BR" . "bottom-right")
                                          ("TL" . "top-left") ("BL" . "bottom-left"))))
                     " corner."))
      s
    )
  )
)

; --- MAIN COMMAND -----------------------------------------------------------

(defun c:cfbuild ( / *error* doc input items errors parsed controllers psus devices
                     needed missing groups key grp line proto conflict
                     basePt x extCache ext ins ent placed ctrlEnt ss topY level
                     wired order autoPsu powered marker slots railX0 railX1 frameScale res
                     measured known box)

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (vl-catch-all-apply 'vla-endundomark (list doc))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nCFBUILD error: " msg))
    )
    (princ)
  )

  ; --- DEPENDENCIES ---
  (if (not (and cf:draw-bus cf:wire-power cf:wire-data))
    (progn
      (alert "CFBUILD needs the other Control Freak scripts.\nLoad ControlFreak.lsp first.")
      (exit)
    )
  )

  ; --- 1. WHAT TO BUILD ---
  (cfb:show-guide)
  ; Ask until we get something: "?" lists products, an empty answer (e.g. a
  ; stray Enter) just asks again. Esc cancels.
  (setq input "")
  (while (member (vl-string-trim " " input) '("" "?"))
    (setq input (getstring T cfb:*prompt*))
    (cond
      ((= (vl-string-trim " " input) "?") (cfb:show-products))
      ((= (vl-string-trim " " input) "")
        (princ "\nNothing entered - type the system (e.g. 1 eDS10 4P, 2 UBi, 1 0-10V) or press Esc to cancel."))
    )
  )

  (foreach item (cfb:split input ",")
    (if (/= (vl-string-trim " \t" item) "")
      (progn
        (setq parsed (cfb:parse-item item))
        (if (= (type parsed) 'STR)
          (setq errors (cons parsed errors))
          (setq items (append items (list parsed)))
        )
      )
    )
  )

  ; Sort into controller / power supply / devices
  (foreach it items
    (cond
      ((= (cfb:role (car it)) "CONTROLLER") (setq controllers (append controllers (list it))))
      ((= (cfb:role (car it)) "PSU")        (setq psus (append psus (list it))))
      (T                                    (setq devices (append devices (list it))))
    )
  )

  (cond
    ((not controllers) (setq errors (cons "no controller - add eDS10 4P or eDS10 6P" errors)))
    ((or (> (length controllers) 1) (> (cadr (car controllers)) 1))
      (setq errors (cons "only one controller per build is supported" errors)))
  )
  (if (> (length psus) 1)
    (setq errors (cons "list at most one power supply" errors)))
  (if (and psus (> (cadr (car psus)) 1))
    (setq errors (cons "only one power supply per build is supported" errors)))

  ; Group devices by controller line; a line can't carry both DALI and DMX
  (foreach it devices
    (setq proto (cfb:proto (car it)))
    (setq line (cond ((caddr it))
                     ((= proto "DMX") cfb:*default-dmx-line*)
                     (T cfb:*default-dali-line*)))
    (setq grp (assoc line groups))
    (cond
      ((and grp (/= (cadr grp) proto))
        (setq conflict line))
      (grp
        (setq groups (subst (list line proto (append (caddr grp) (list it))) grp groups)))
      (T
        (setq groups (cons (list line proto (list it)) groups)))
    )
  )
  (if conflict
    (setq errors (cons (strcat "controller line DA" (itoa conflict)
                               " has both DALI and DMX devices - use @DAn to separate them")
                       errors)))

  (if errors
    (progn
      (alert (strcat "CFBUILD can't build that:\n"
                     (apply 'strcat (mapcar '(lambda (s) (strcat "\n- " s)) (reverse errors)))
                     "\n\nKnown products: " (cfb:product-list)))
      (exit)
    )
  )

  ; Default power supply
  (if (not psus)
    (setq psus (list (list (cfb:find cfb:*default-psu*) 1 nil))
          autoPsu T))

  (setq groups (vl-sort groups '(lambda (a b) (< (car a) (car b)))))

  ; --- 2. MAKE SURE THE BLOCKS ARE IN THE DRAWING ---
  (foreach it (append psus controllers devices)
    (if (not (member (cfb:block (car it)) needed))
      (setq needed (cons (cfb:block (car it)) needed))))
  (setq missing (vl-remove-if '(lambda (b) (tblsearch "BLOCK" b)) needed))
  ; Frame and header are optional, but worth importing if they're missing
  (if (and (or missing
               (not (tblsearch "BLOCK" cfb:*frame-block*))
               (not (tblsearch "BLOCK" cfb:*header-block*)))
           c:cfblocks)
    (progn
      (princ "\nImporting product blocks...")
      ; Wrapped so a problem inside the import can't stop the build
      (setq res (vl-catch-all-apply 'c:cfblocks nil))
      (if (vl-catch-all-error-p res)
        (princ (strcat "\nImport reported: " (vl-catch-all-error-message res) " - continuing.")))
      (setq missing (vl-remove-if '(lambda (b) (tblsearch "BLOCK" b)) needed))
      (princ "\nImport finished, continuing build...")
    )
  )
  (if missing
    (progn
      (alert (strcat "These blocks aren't in the drawing or the product library:\n"
                     (cfb:join-words missing)))
      (exit)
    )
  )

  ; --- 3. CONFIRM AND PICK A POINT ---
  (princ "\nBuilding:")
  (princ (strcat "\n  " (cfb:key (car (car psus))) " power supply"
                 (if autoPsu " (default - list HDR15/30/60 to choose)" "")))
  (princ (strcat "\n  " (cfb:key (car (car controllers))) " controller"))
  (foreach g groups
    (foreach it (caddr g)
      (princ (strcat "\n  " (itoa (cadr it)) " x " (cfb:key (car it))
                     "  -> " (cadr g) " on controller DA" (itoa (car g))))
    )
  )

  (setq basePt (getpoint "\nPick bottom-left insertion point: "))
  (if (not basePt) (exit))
  (setq basePt (trans basePt 1 0))

  (vl-catch-all-apply 'vla-startundomark (list doc))

  ; Everything created from here on is measured afterwards to fit the frame
  (setq marker (entlast))

  ; --- 4. WORK OUT POSITIONS LEFT TO RIGHT ---
  ; Order: power supply, controller, then devices grouped by controller line
  (setq order (list (car psus) (car controllers)))
  (foreach g groups (setq order (append order (caddr g))))

  (setq x (car basePt))
  (foreach it order
    (setq key (cfb:block (car it)))
    (if (not (setq ext (cdr (assoc key extCache))))
      (progn
        (setq ext (cf:block-extents key))
        (if (not ext) (setq ext '((0.0 0.0) (50.0 100.0))))
        (setq extCache (cons (cons key ext) extCache))
      )
    )
    (repeat (cadr it)
      ; Left edge of the block at x, base points on one line
      (setq ins (list (- x (caar ext)) (cadr basePt) 0.0))
      (setq slots (append slots (list (list key ins it))))
      (setq x (+ (car ins) (caadr ext) cfb:*gap*))
    )
  )

  ; --- 5. DIN RAIL (drawn first so it sits behind the units) ---
  (setq railX0 (- (car basePt) cfb:*rail-overhang*))
  (setq railX1 (+ (- x cfb:*gap*) cfb:*rail-overhang*))
  (cfb:ensure-layer cfb:*rail-layer* 8)
  (if (setq ent (cfb:insert (cfb:rail-block (- railX1 railX0))
                            (list railX0 (+ (cadr basePt) cfb:*rail-offset*) 0.0)))
    (cfb:set-layer ent cfb:*rail-layer*)
  )

  ; --- 6. INSERT THE UNITS ---
  (foreach s slots
    (if (setq ent (cfb:insert (car s) (cadr s)))
      (setq placed (append placed (list (list ent (caddr s)))))
    )
  )

  (setq ctrlEnt (car (vl-some '(lambda (p) (if (= (cfb:role (car (cadr p))) "CONTROLLER") p)) placed)))

  ; --- 7. POWER ---
  (princ "\n--- Power ---")
  (setq ss (ssadd))
  (foreach p placed (ssadd (car p) ss))
  (setq topY (cf:wire-power ss))
  (setq powered (and topY T))

  ; Data buses start above the power bus (or above the tallest block)
  (if (not topY)
    (foreach p placed
      (setq ext (cf:bbox (car p)))
      (if ext (setq topY (if topY (max topY (cadadr ext)) (cadadr ext))))
    )
  )
  (setq level (+ topY 5.0))

  ; --- 8. DATA BUSES (one per controller line) ---
  (foreach g groups
    (princ (strcat "\n--- " (cadr g) " on controller DA" (itoa (car g)) " ---"))
    (setq grp (list (list ctrlEnt (car g))))
    (foreach p placed
      (if (member (cadr p) (caddr g))
        (setq grp (append grp (list (list (car p) 1))))   ; each device uses its line 1
      )
    )
    (if (setq topY (cf:wire-data grp (cadr g) level))
      (setq level (+ topY 5.0)
            wired (1+ (if wired wired 0)))
    )
  )

  ; --- 9. FRAME AND HEADER AROUND EVERYTHING ---
  ; Size of the system: what's measured in the drawing, combined with what we
  ; know we placed (rail ends, unit bodies, wires routed below the units up
  ; to 20mm, top bus) - in case the CAD under-reports an object's extents
  (setq measured (cfb:extents-after marker))
  (setq known (list (list railX0
                          (- (apply 'min (mapcar '(lambda (s)
                                                    (+ (cadr (cadr s))
                                                       (cadar (cdr (assoc (car s) extCache)))))
                                                 slots))
                             20.0))
                    (list railX1 level)))
  (setq box (if measured
              (list (list (min (caar measured) (caar known)) (min (cadar measured) (cadar known)))
                    (list (max (caadr measured) (caadr known)) (max (cadadr measured) (cadadr known))))
              known))
  (princ (strcat "\nFrame: system is " (rtos (- (caadr box) (caar box)) 2 0) " x "
                 (rtos (- (cadadr box) (cadar box)) 2 0) " mm"
                 (if measured
                   (strcat " (measured " (rtos (- (caadr measured) (caar measured)) 2 0) " x "
                           (rtos (- (cadadr measured) (cadar measured)) 2 0) ")")
                   "")))
  (setq frameScale (cfb:place-frame box))

  (vl-catch-all-apply 'vla-endundomark (list doc))

  (princ (strcat "\n\nCFBUILD complete: " (itoa (length placed)) " block(s) placed"
                 (if powered ", power wired" ", power NOT wired")
                 (if wired (strcat ", " (itoa wired) " data bus(es) wired") "")
                 (if frameScale
                   (strcat ", frame at scale " (rtos frameScale 2 2)
                           (if (= frameScale 1.0) " (A3 at 1:1)" (strcat " (A3 at 1:" (rtos frameScale 2 2) ")")))
                   "")
                 ". Use U to undo the whole build."))
  (princ)
)

(princ "\nControl Freak builder loaded. Type CFBUILD to run.")
(princ)

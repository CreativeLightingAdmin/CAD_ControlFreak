; ============================================================================
; DATA LINE BUS WIRING (Using SmartJumper Library)
; Interactive data bus wiring (DALI/DMX) with per-device line selection
; User clicks devices and specifies which line for EACH device
;
; Command: DATALINE
; Core:    (cf:wire-data devices protocol refHeight) - no prompts, used by CFBUILD
; ============================================================================

(vl-load-com)

(defun c:dataline ( / *error* protocol refPt refHeight devices p1 ss blk deviceLine
                      daPlusAttr daMinusAttr daShieldAttr)

  ; --- CHECK DEPENDENCY ---
  (if (not smart-wire)
    (progn
      (alert "Error: SmartJumper_Lib.lsp is not loaded!")
      (exit)
    )
  )

  ; --- ERROR HANDLER ---
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ; 1. GET PROTOCOL SELECTION
  (princ "\n--- DATA LINE WIRING ---")
  (initget 1 "DALI DMX")  ; Require valid response
  (setq protocol (getkword "\nProtocol [DALI/DMX]: "))

  ; 2. GET REFERENCE HEIGHT
  (princ "\nY Offset (i.e. Negative Wire) - Adds 5mm...")
  (setq refPt (getpoint))
  (if (not refPt) (exit))
  (setq refHeight (cadr (trans refPt 1 0)))  ; Y in WCS (picked point is in UCS)
  (princ (strcat "\nReference height: " (rtos refHeight 2 2) " mm"))

  ; 3. COLLECT DEVICES (PER-DEVICE LINE SELECTION)
  ; Click order does not matter - bus spans and junction dots are
  ; worked out from terminal positions when drawing
  (setq p1 (getpoint "\nClick device #1 (or press Enter to finish): "))

  (while p1
    ; Get the block entity at click point
    (setq ss (ssget p1 '((0 . "INSERT"))))
    (if ss
      (progn
        (setq blk (ssname ss 0))

        (initget 1 "1 2 3 4 5 6")  ; Require valid response
        (setq deviceLine (getint "\nWhich data line for this device (1-6): "))

        (setq daPlusAttr (strcat "DA" (itoa deviceLine) "+"))
        (setq daMinusAttr (strcat "DA" (itoa deviceLine) "-"))
        (setq daShieldAttr (strcat "DA" (itoa deviceLine) "G"))

        ; Check now so the user can re-pick straight away
        (if (and (find-data-attribute blk daPlusAttr)
                 (find-data-attribute blk daMinusAttr)
                 (or (/= protocol "DMX") (find-data-attribute blk daShieldAttr)))
          (progn
            (setq devices (append devices (list (list blk deviceLine))))
            (princ (strcat "\n  Found data line " (itoa deviceLine)))
          )
          (princ (strcat "\n  Warning: " daPlusAttr "/" daMinusAttr
                         (if (= protocol "DMX") (strcat "/" daShieldAttr) "")
                         " not found on this device"))
        )
      )
      (princ "\n  Warning: No block found at click point")
    )

    (setq p1 (getpoint (strcat "\nClick device #"
                               (itoa (1+ (length devices)))
                               " (or press Enter to finish): ")))
  )

  (if (< (length devices) 2)
    (progn (princ "\nNeed at least 2 devices.") (exit))
  )

  (if (cf:wire-data devices protocol refHeight)
    (princ "\nData Line Bus Complete.")
  )
  (princ)
)

; ============================================================================
; CORE: Wire one data bus
;   devices   - ((blockEnt lineNumber) ...)
;   protocol  - "DALI" (Data+, Data-) or "DMX" (Data+, Data-, Shield)
;   refHeight - buses are drawn 5mm / 10mm (/ 15mm) above this Y
; Returns the Y of the top bus, or nil if fewer than 2 devices could be wired
; ============================================================================
(defun cf:wire-data (devices protocol refHeight / dataPlusBusY dataMinusBusY shieldBusY
                       ColPlus ColMinus ColShield BrRad DotRad dmx n plusPos minusPos shieldPos
                       deviceBlocks plusList minusList shieldList routes i
                       plusRoutes minusRoutes shieldRoutes plusDots minusDots shieldDots)

  (setq BrRad 2.0)      ; Bridge radius
  (setq DotRad 1.0)     ; Junction dot radius
  (setq dmx (= protocol "DMX"))

  ; BUS HEIGHTS AND COLOURS BASED ON PROTOCOL
  (if dmx
    (progn
      ; DMX: 3-wire system (bottom to top: Shield, Data-, Data+)
      (setq shieldBusY (+ refHeight 5.0))
      (setq dataMinusBusY (+ refHeight 10.0))
      (setq dataPlusBusY (+ refHeight 15.0))
      (setq ColShield 34)                       ; Brown
      (setq ColMinus 3)                         ; Green
      (setq ColPlus 7)                          ; White
    )
    (progn
      ; DALI: 2-wire system (bottom to top: Data+, Data-)
      (setq dataPlusBusY (+ refHeight 5.0))
      (setq dataMinusBusY (+ refHeight 10.0))
      (setq ColPlus 7)                          ; White
      (setq ColMinus 7)                         ; White
    )
  )

  ; FIND EACH DEVICE'S TERMINALS FOR ITS LINE
  (foreach dev devices
    (setq n (itoa (cadr dev)))
    (setq plusPos   (find-data-attribute (car dev) (strcat "DA" n "+")))
    (setq minusPos  (find-data-attribute (car dev) (strcat "DA" n "-")))
    (setq shieldPos (if dmx (find-data-attribute (car dev) (strcat "DA" n "G"))))
    (if (and plusPos minusPos (or (not dmx) shieldPos))
      (progn
        (setq deviceBlocks (append deviceBlocks (list (car dev))))
        (setq plusList (append plusList (list plusPos)))
        (setq minusList (append minusList (list minusPos)))
        (if dmx (setq shieldList (append shieldList (list shieldPos))))
      )
      (princ (strcat "\n  Warning: data line " n " terminals not found on "
                     (cdr (assoc 2 (entget (car dev)))) " - skipped"))
    )
  )

  (if (< (length deviceBlocks) 2)
    (progn
      (princ (strcat "\n" protocol ": need at least 2 devices to wire."))
      nil
    )
    (progn
      ; Route each device's wires: terminals facing the buses get a straight
      ; riser, terminals on the far side of the block go round it
      (setq i 0)
      (foreach blk deviceBlocks
        (setq routes (cf:device-routes blk
                       (if dmx
                         (list (nth i plusList) (nth i minusList) (nth i shieldList))
                         (list (nth i plusList) (nth i minusList)))
                       (list dataPlusBusY dataMinusBusY shieldBusY)))
        (setq plusRoutes  (append plusRoutes  (list (car routes))))
        (setq minusRoutes (append minusRoutes (list (cadr routes))))
        (if dmx (setq shieldRoutes (append shieldRoutes (list (caddr routes)))))
        (setq i (1+ i))
      )

      ; PHASE 1: DATA+ BUS WITH SMART BRIDGES (White for DALI and DMX)
      (princ (strcat "\nDrawing " protocol " Data+ Bus..."))
      (setq plusDots (cf:draw-bus plusRoutes dataPlusBusY ColPlus T BrRad))

      ; PHASE 2: DATA- BUS WITH SMART BRIDGES (White for DALI, Green for DMX)
      (princ (strcat "\nDrawing " protocol " Data- Bus..."))
      (setq minusDots (cf:draw-bus minusRoutes dataMinusBusY ColMinus T BrRad))

      ; PHASE 3: SHIELD BUS WITH SMART BRIDGES (DMX ONLY, Brown)
      (if dmx
        (progn
          (princ "\nDrawing DMX Shield Bus...")
          (setq shieldDots (cf:draw-bus shieldRoutes shieldBusY ColShield T BrRad))
        )
      )

      ; PHASE 4: JUNCTION DOTS
      (foreach pt plusDots   (draw-junction-dot pt DotRad ColPlus))
      (foreach pt minusDots  (draw-junction-dot pt DotRad ColMinus))
      (foreach pt shieldDots (draw-junction-dot pt DotRad ColShield))

      (max dataPlusBusY dataMinusBusY)
    )
  )
)

; ============================================================================
; HELPER: Find Data Line Attribute
; Searches block for specified data line attribute tag and returns position
; Returns: Attribute anchor point (terminal location) or nil
; ============================================================================
(defun find-data-attribute (blockEnt attrTag / attEnt attData tag pos)
  (setq attEnt (entnext blockEnt))
  (while (and attEnt
              (= "ATTRIB" (cdr (assoc 0 (entget attEnt)))))
    (setq attData (entget attEnt))
    (setq tag (cdr (assoc 2 attData)))

    ; Case-insensitive match with wildcards
    (if (wcmatch (strcase tag) (strcat "*" (strcase attrTag) "*"))
      (setq pos (cf:att-point attData))
    )
    (setq attEnt (entnext attEnt))
  )
  pos  ; Returns position or nil
)

(princ "\nData Line Bus loaded. Type DATALINE to run.")
(princ)

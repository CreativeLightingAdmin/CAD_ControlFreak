; ============================================================================
; DATA LINE BUS WIRING (Using SmartJumper Library)
; Interactive data bus wiring (DALI/DMX) with per-device line selection
; User clicks devices and specifies which line for EACH device
; ============================================================================

(vl-load-com)

(defun c:dataline ( / *error* protocol refPt refHeight dataPlusBusY dataMinusBusY
                      shieldBusY ColPlus ColMinus ColShield BrRad DotRad
                      dataPlusList dataMinusList shieldList p1 ss blk deviceLine
                      daPlusAttr daMinusAttr daShieldAttr daPlusPos daMinusPos daShieldPos
                      deviceCount plusDots minusDots shieldDots
                      deviceBlocks routes plusRoutes minusRoutes shieldRoutes i)

  ; --- CHECK DEPENDENCY ---
  (if (not smart-wire)
    (progn
      (alert "Error: SmartJumper_Lib.lsp is not loaded!")
      (exit)
    )
  )

  ; --- CONFIGURATION CONSTANTS ---
  (setq BrRad 2.0)      ; Bridge radius
  (setq DotRad 1.0)     ; Junction dot radius

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

  ; 3. CALCULATE BUS HEIGHTS AND COLORS BASED ON PROTOCOL
  (if (= protocol "DMX")
    (progn
      ; DMX: 3-wire system (bottom to top: Shield, Data-, Data+)
      (setq shieldBusY (+ refHeight 5.0))       ; Shield at Ref + 5mm
      (setq dataMinusBusY (+ refHeight 10.0))   ; Data- at Ref + 10mm
      (setq dataPlusBusY (+ refHeight 15.0))    ; Data+ at Ref + 15mm
      (setq ColShield 34)                       ; Brown
      (setq ColMinus 3)                         ; Green
      (setq ColPlus 7)                          ; White
      (princ (strcat "\nDMX Shield bus at: " (rtos shieldBusY 2 2) " mm (brown)"))
      (princ (strcat "\nDMX Data- bus at: " (rtos dataMinusBusY 2 2) " mm (green)"))
      (princ (strcat "\nDMX Data+ bus at: " (rtos dataPlusBusY 2 2) " mm (white)"))
    )
    (progn
      ; DALI: 2-wire system (bottom to top: Data+, Data-)
      (setq shieldBusY nil)                     ; No shield
      (setq dataPlusBusY (+ refHeight 5.0))     ; Data+ at Ref + 5mm
      (setq dataMinusBusY (+ refHeight 10.0))   ; Data- at Ref + 10mm
      (setq ColPlus 7)                          ; White
      (setq ColMinus 7)                         ; White
      (princ (strcat "\nDALI Data+ bus at: " (rtos dataPlusBusY 2 2) " mm (white)"))
      (princ (strcat "\nDALI Data- bus at: " (rtos dataMinusBusY 2 2) " mm (white)"))
    )
  )

  ; 4. COLLECT DEVICES AND TERMINALS (PER-DEVICE LINE SELECTION)
  ; Click order does not matter - bus spans and junction dots are
  ; worked out from terminal positions when drawing
  (setq dataPlusList '())
  (setq dataMinusList '())
  (setq shieldList '())
  (setq deviceCount 0)

  (setq p1 (getpoint "\nClick device #1 (or press Enter to finish): "))

  (while p1
    ; Get the block entity at click point
    (setq ss (ssget p1 '((0 . "INSERT"))))
    (if ss
      (progn
        (setq blk (ssname ss 0))

        ; PROMPT FOR LINE NUMBER FOR THIS DEVICE
        (initget 1 "1 2 3 4")  ; Require valid response
        (setq deviceLine (getint "\nWhich data line for this device (1-4): "))

        ; Build attribute names based on THIS device's line selection
        (setq daPlusAttr (strcat "DA" (itoa deviceLine) "+"))
        (setq daMinusAttr (strcat "DA" (itoa deviceLine) "-"))
        (setq daShieldAttr (strcat "DA" (itoa deviceLine) "G"))  ; G for ground/shield

        ; Find attribute positions
        (setq daPlusPos (find-data-attribute blk daPlusAttr))
        (setq daMinusPos (find-data-attribute blk daMinusAttr))

        ; For DMX, also find shield
        (if (= protocol "DMX")
          (setq daShieldPos (find-data-attribute blk daShieldAttr))
          (setq daShieldPos nil)
        )

        ; Validate based on protocol
        (if (= protocol "DMX")
          ; DMX requires all 3 wires
          (if (and daPlusPos daMinusPos daShieldPos)
            (progn
              (setq dataPlusList (append dataPlusList (list daPlusPos)))
              (setq dataMinusList (append dataMinusList (list daMinusPos)))
              (setq shieldList (append shieldList (list daShieldPos)))
              (setq deviceBlocks (append deviceBlocks (list blk)))
              (setq deviceCount (1+ deviceCount))
              (princ (strcat "\n  Found " daPlusAttr ", " daMinusAttr ", " daShieldAttr))
            )
            (princ (strcat "\n  Warning: DMX requires " daPlusAttr "/" daMinusAttr "/" daShieldAttr))
          )
          ; DALI requires only 2 wires
          (if (and daPlusPos daMinusPos)
            (progn
              (setq dataPlusList (append dataPlusList (list daPlusPos)))
              (setq dataMinusList (append dataMinusList (list daMinusPos)))
              (setq deviceBlocks (append deviceBlocks (list blk)))
              (setq deviceCount (1+ deviceCount))
              (princ (strcat "\n  Found " daPlusAttr " and " daMinusAttr))
            )
            (princ (strcat "\n  Warning: " daPlusAttr "/" daMinusAttr " not found on this device"))
          )
        )
      )
      (princ "\n  Warning: No block found at click point")
    )

    (setq p1 (getpoint (strcat "\nClick device #"
                               (itoa (1+ deviceCount))
                               " (or press Enter to finish): ")))
  )

  ; Validate we have enough devices
  (if (< deviceCount 2)
    (progn
      (princ "\nNeed at least 2 devices.")
      (exit)
    )
  )

  ; Route each device's wires: terminals facing the buses get a straight
  ; riser, terminals on the far side of the block go round it
  (setq i 0)
  (foreach blk deviceBlocks
    (setq routes (cf:device-routes blk
                   (if (= protocol "DMX")
                     (list (nth i dataPlusList) (nth i dataMinusList) (nth i shieldList))
                     (list (nth i dataPlusList) (nth i dataMinusList)))
                   (list dataPlusBusY dataMinusBusY shieldBusY)))
    (setq plusRoutes  (append plusRoutes  (list (car routes))))
    (setq minusRoutes (append minusRoutes (list (cadr routes))))
    (if (= protocol "DMX")
      (setq shieldRoutes (append shieldRoutes (list (caddr routes))))
    )
    (setq i (1+ i))
  )

  ; -----------------------------------------------------------
  ; PHASE 1: DATA+ BUS WITH SMART BRIDGES (White for DALI and DMX)
  ; -----------------------------------------------------------
  (princ "\nDrawing Data+ Bus...")
  (setq plusDots (cf:draw-bus plusRoutes dataPlusBusY ColPlus T BrRad))

  ; -----------------------------------------------------------
  ; PHASE 2: DATA- BUS WITH SMART BRIDGES (White for DALI, Green for DMX)
  ; -----------------------------------------------------------
  (princ "\nDrawing Data- Bus...")
  (setq minusDots (cf:draw-bus minusRoutes dataMinusBusY ColMinus T BrRad))

  ; -----------------------------------------------------------
  ; PHASE 3: SHIELD BUS WITH SMART BRIDGES (DMX ONLY, Brown)
  ; -----------------------------------------------------------
  (if (= protocol "DMX")
    (progn
      (princ "\nDrawing Shield Bus...")
      (setq shieldDots (cf:draw-bus shieldRoutes shieldBusY ColShield T BrRad))
    )
  )

  ; -----------------------------------------------------------
  ; PHASE 4: JUNCTION DOTS (PROTOCOL-AWARE)
  ; -----------------------------------------------------------
  (princ "\nAdding Junction Dots...")
  (foreach pt plusDots   (draw-junction-dot pt DotRad ColPlus))
  (foreach pt minusDots  (draw-junction-dot pt DotRad ColMinus))
  (foreach pt shieldDots (draw-junction-dot pt DotRad ColShield))

  (princ "\nData Line Bus Complete.")
  (princ)
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

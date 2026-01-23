; ============================================================================
; DATA LINE BUS WIRING (Using SmartJumper Library)
; Interactive data bus wiring (DALI/DMX) with per-device line selection
; User clicks devices and specifies which line for EACH device
; ============================================================================

(vl-load-com)

(defun c:dataline ( / *error* protocol refPt refHeight dataPlusBusY dataMinusBusY
                      shieldBusY ColPlus ColMinus ColShield
                      dataPlusList dataMinusList shieldList p1 ss blk deviceLine
                      daPlusAttr daMinusAttr daShieldAttr daPlusPos daMinusPos daShieldPos
                      oldSnap oldCmd deviceCount i x y ptStart ptTop len
                      startX endX dataMinusX shieldX)

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
    (if oldSnap (setvar "OSMODE" oldSnap))
    (if oldCmd (setvar "CMDECHO" oldCmd))
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
  (setq refHeight (cadr refPt))  ; Extract Y coordinate

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

  ; 4. SETUP ENVIRONMENT
  (setq oldSnap (getvar "OSMODE"))
  (setq oldCmd (getvar "CMDECHO"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ; -----------------------------------------------------------
  ; PHASE 1: DATA+ BUS WITH SMART BRIDGES
  ; -----------------------------------------------------------
  (princ "\nDrawing Data+ Bus...")

  ; A. Vertical Risers with Smart Bridges
  (foreach pt dataPlusList
    (setq x (car pt))
    (setq y (cadr pt))

    (setq ptStart (list x (+ y 5.0) 0.0))
    (setq ptTop (list x dataPlusBusY 0.0))

    ; All devices use smart bridge detection (White for both DALI and DMX)
    (smart-wire ptStart ptTop BrRad ColPlus)
  )

  ; B. Horizontal Bus
  (setvar "CECOLOR" (itoa ColPlus))  ; Reset color after smart-wire
  (setq startX (car (car dataPlusList)))
  (setq endX (car (last dataPlusList)))
  (command "LINE" (list startX dataPlusBusY 0.0) (list endX dataPlusBusY 0.0) "")

  ; -----------------------------------------------------------
  ; PHASE 2: DATA- BUS WITH SMART BRIDGES
  ; -----------------------------------------------------------
  (princ "\nDrawing Data- Bus...")

  ; A. Vertical Risers with Smart Bridges
  (foreach dataMinusPt dataMinusList
    (setq x (car dataMinusPt))
    (setq y (cadr dataMinusPt))

    (setq ptStart (list x (+ y 5.0) 0.0))
    (setq ptTop (list x dataMinusBusY 0.0))

    ; Color: White for DALI, Green for DMX
    (smart-wire ptStart ptTop BrRad ColMinus)
  )

  ; B. Horizontal Bus
  (setvar "CECOLOR" (itoa ColMinus))  ; Reset color after smart-wire
  (setq startX (car (car dataMinusList)))
  (setq endX (car (last dataMinusList)))
  (command "LINE" (list startX dataMinusBusY 0.0) (list endX dataMinusBusY 0.0) "")

  ; -----------------------------------------------------------
  ; PHASE 3: SHIELD BUS WITH SMART BRIDGES (DMX ONLY)
  ; -----------------------------------------------------------
  (if (= protocol "DMX")
    (progn
      (princ "\nDrawing Shield Bus...")

      ; A. Vertical Risers with Smart Bridges
      (foreach shieldPt shieldList
        (setq x (car shieldPt))
        (setq y (cadr shieldPt))

        (setq ptStart (list x (+ y 5.0) 0.0))
        (setq ptTop (list x shieldBusY 0.0))

        ; Color: Brown for shield
        (smart-wire ptStart ptTop BrRad ColShield)
      )

      ; B. Horizontal Bus
      (setvar "CECOLOR" (itoa ColShield))  ; Reset color after smart-wire
      (setq startX (car (car shieldList)))
      (setq endX (car (last shieldList)))
      (command "LINE" (list startX shieldBusY 0.0) (list endX shieldBusY 0.0) "")
    )
  )

  ; -----------------------------------------------------------
  ; PHASE 4: JUNCTION DOTS (PROTOCOL-AWARE)
  ; -----------------------------------------------------------
  (princ "\nAdding Junction Dots...")

  ; Skip first and last devices
  (setq i 0)
  (setq len deviceCount)

  (foreach pt dataPlusList
    ; Only process if NOT first and NOT last
    (if (and (> i 0) (< i (- len 1)))
      (progn
        (setq x (car pt))
        (setq dataMinusX (car (nth i dataMinusList)))

        ; Data+ dot (White for both DALI and DMX)
        (setvar "CECOLOR" (itoa ColPlus))
        (draw-junction-dot (list x dataPlusBusY 0.0) DotRad)

        ; Data- dot (White for DALI, Green for DMX)
        (setvar "CECOLOR" (itoa ColMinus))
        (draw-junction-dot (list dataMinusX dataMinusBusY 0.0) DotRad)

        ; Shield dot (Brown, DMX only)
        (if (= protocol "DMX")
          (progn
            (setq shieldX (car (nth i shieldList)))
            (setvar "CECOLOR" (itoa ColShield))
            (draw-junction-dot (list shieldX shieldBusY 0.0) DotRad)
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ; Restore Settings
  (setvar "OSMODE" oldSnap)
  (setvar "CECOLOR" "BYLAYER")
  (setvar "CMDECHO" 1)
  (princ "\nData Line Bus Complete.")
  (princ)
)

; ============================================================================
; HELPER: Find Data Line Attribute
; Searches block for specified data line attribute tag and returns position
; Returns: Attribute insertion point (terminal location) or nil
; ============================================================================
(defun find-data-attribute (blockEnt attrTag / attEnt attData tag pos)
  (setq attEnt (entnext blockEnt))
  (while (and attEnt
              (= "ATTRIB" (cdr (assoc 0 (entget attEnt)))))
    (setq attData (entget attEnt))
    (setq tag (cdr (assoc 2 attData)))

    ; Case-insensitive match with wildcards
    (if (wcmatch (strcase tag) (strcat "*" (strcase attrTag) "*"))
      (progn
        (setq pos (cdr (assoc 10 attData)))
        ; Force Z=0 for reliability
        (setq pos (list (car pos) (cadr pos) 0.0))
      )
    )
    (setq attEnt (entnext attEnt))
  )
  pos  ; Returns position or nil
)

; ============================================================================
; HELPER: DRAW JUNCTION DOT
; Uses a Circle + Solid Hatch (with color fix)
; ============================================================================
(defun draw-junction-dot (cenPt rad / lastEnt hatchEnt entData currentColor)
  ; Get the current color setting
  (setq currentColor (getvar "CECOLOR"))

  ; Draw the circle
  (command "CIRCLE" cenPt rad)
  (setq lastEnt (entlast))

  ; Hatch it Solid
  ; Note: -HATCH is the command line version.
  (if (fnob "-HATCH") ; Check if command exists (ProgeCAD safety)
    (progn
      (command "-HATCH" "P" "SOLID" "S" lastEnt "" "")
      ; Get the hatch entity and set its color explicitly
      (setq hatchEnt (entlast))
      (setq entData (entget hatchEnt))
      ; Replace or add color code (62) with current color
      (setq entData (subst (cons 62 (atoi currentColor)) (assoc 62 entData) entData))
      ; If no color code exists, add it
      (if (not (assoc 62 entData))
        (setq entData (append entData (list (cons 62 (atoi currentColor)))))
      )
      (entmod entData)
    )
    ; Fallback for older CAD versions: Donut with 0 ID
    (command "DONUT" 0.0 (* rad 2.0) cenPt "")
  )
)

; Check if a command exists (Safety for ProgeCAD versions)
(defun fnob (x) (if (getcname x) T nil))

(princ "\nData Line Bus loaded. Type DATALINE to run.")
(princ)

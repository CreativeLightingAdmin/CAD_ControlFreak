; ============================================================================
; AUTO POWER BUS - Automatic Power Bus Wiring from Smart Blocks
; Reads block attributes (PWR_POS, PWR_NEG) to automatically wire power buses
; Uses SmartJumper_Lib for automatic bridge detection
; ============================================================================

(vl-load-com)

(defun c:autopowerbus ( / *error* ss blockList posTermList negTermList skippedCount
                         maxY i x y yPosBus yNegBus negX oldSnap len)

  ; --- CHECK DEPENDENCY ---
  (if (not smart-wire)
    (progn
      (alert "Error: SmartJumper_Lib.lsp is not loaded!")
      (exit)
    )
  )

  ; --- CONFIGURATION CONSTANTS ---
  (setq ArcOffset 20.0)        ; Height of bus above highest click
  (setq NegBusOffset 5.0)      ; Height of Negative bus above Positive
  (setq BrRad 2.0)             ; Jumper Bridge Radius
  (setq DotRad 1.0)            ; Junction Dot Radius
  (setq ColPos 1)              ; Red
  (setq ColNeg 5)              ; Blue

  ; --- ERROR HANDLER ---
  (defun *error* (msg)
    (if oldSnap (setvar "OSMODE" oldSnap))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ; 1. COLLECT BLOCKS WITH ATTRIBUTES
  (princ "\n--- AUTO POWER BUS WIRING ---")
  (princ "\nSelect blocks to wire together...")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss)
    (progn (princ "\nNo blocks selected.") (exit))
  )

  (setq blockList (collect-power-blocks ss))
  (setq skippedCount (- (sslength ss) (length blockList)))

  ; Display results
  (if (> skippedCount 0)
    (princ (strcat "\nWarning: " (itoa skippedCount) " block(s) skipped (missing PWR_POS/PWR_NEG attributes)"))
  )
  (princ (strcat "\nFound " (itoa (length blockList)) " valid block(s)"))

  ; Check if we have enough blocks
  (if (< (length blockList) 2)
    (progn (princ "\nNeed at least 2 blocks with PWR_POS/PWR_NEG attributes.") (exit))
  )

  ; 2. SORT BLOCKS LEFT-TO-RIGHT
  (setq blockList (sort-blocks-left-to-right blockList))

  ; 3. EXTRACT TERMINAL LISTS AND CALCULATE MAX HEIGHT
  (setq posTermList '())
  (setq negTermList '())
  (setq maxY -1000000.0)

  (foreach blkItem blockList
    (setq posTermList (append posTermList (list (caddr blkItem))))   ; posTerm
    (setq negTermList (append negTermList (list (cadddr blkItem))))  ; negTerm

    ; Track highest Y from positive terminals
    (if (> (cadr (caddr blkItem)) maxY)
      (setq maxY (cadr (caddr blkItem)))
    )
  )

  ; 4. CALCULATE BUS HEIGHTS
  (setq yPosBus (+ maxY ArcOffset))
  (setq yNegBus (+ yPosBus NegBusOffset))

  ; Setup Environment
  (setq oldSnap (getvar "OSMODE"))
  (setvar "OSMODE" 0) ; Turn off snaps for precision drawing
  (setvar "CMDECHO" 0)

  ; -----------------------------------------------------------
  ; PHASE 1: POSITIVE BUS (RED)
  ; -----------------------------------------------------------
  (princ "\nDrawing Positive Bus...")
  (setvar "CECOLOR" (itoa ColPos)) ; Set Red

  ; A. Vertical Risers
  (foreach pt posTermList
    (setq x (car pt))
    (setq y (cadr pt))
    ; Draw from (y+5) up to yPosBus
    (command "LINE" (list x (+ y 5.0) 0.0) (list x yPosBus 0.0) "")
  )

  ; B. Horizontal Bus
  (setq startX (car (car posTermList)))
  (setq endX (car (last posTermList)))
  (command "LINE" (list startX yPosBus 0.0) (list endX yPosBus 0.0) "")

  ; -----------------------------------------------------------
  ; PHASE 2: NEGATIVE BUS (BLUE)
  ; -----------------------------------------------------------
  (princ "\nDrawing Negative Bus...")

  ; A. Vertical Risers (Using SmartJumper Library for automatic bridge detection!)
  (setq i 0)
  (foreach negPt negTermList
    (setq negX (car negPt))
    (setq y (cadr negPt))

    ; Define the vertical path
    (setq ptStart (list negX (+ y 5.0) 0.0))
    (setq ptTop   (list negX yNegBus 0.0))

    ; First device gets a simple line (power feed), others get smart detection
    (if (= i 0)
      ; First Item: Simple line (no bridge - this is the power feed)
      (progn
        (setvar "CECOLOR" (itoa ColNeg))
        (command "LINE" ptStart ptTop "")
      )
      ; Other Items: Library will detect positive bus crossing and add bridge automatically
      (smart-wire ptStart ptTop BrRad ColNeg)
    )
    (setq i (1+ i))
  )

  ; B. Horizontal Bus
  (setvar "CECOLOR" (itoa ColNeg)) ; Set Blue again
  (setq startNegX (car (car negTermList)))
  (setq endNegX   (car (last negTermList)))
  (command "LINE" (list startNegX yNegBus 0.0) (list endNegX yNegBus 0.0) "")

  ; -----------------------------------------------------------
  ; PHASE 3: JUNCTION DOTS (Hatched Circles)
  ; -----------------------------------------------------------
  (princ "\nAdding Junction Dots...")

  ; We skip the first (index 0) and last (index length-1)
  (setq i 0)
  (setq len (length posTermList))

  (foreach pt posTermList
    ; Only process if NOT first and NOT last
    (if (and (> i 0) (< i (- len 1)))
      (progn
        (setq x (car pt))
        (setq negX (car (nth i negTermList)))

        ; 1. Red Dot (Positive Junction)
        (setvar "CECOLOR" (itoa ColPos))
        (draw-junction-dot (list x yPosBus 0.0) DotRad)

        ; 2. Blue Dot (Negative Junction)
        (setvar "CECOLOR" (itoa ColNeg))
        (draw-junction-dot (list negX yNegBus 0.0) DotRad)
      )
    )
    (setq i (1+ i))
  )

  ; Restore Settings
  (setvar "OSMODE" oldSnap)
  (setvar "CECOLOR" "BYLAYER")
  (setvar "CMDECHO" 1)
  (princ "\nAuto Power Bus Complete.")
  (princ)
)

; ============================================================================
; HELPER: Collect blocks with PWR_POS/PWR_NEG attributes
; Returns: ((blockEnt blockInsPoint posTermPoint negTermPoint) ...)
; ============================================================================
(defun collect-power-blocks (ss / blockList i ent entData blockInsPoint
                                  attEnt attData attTag attPos posTerm negTerm)
  (setq blockList '())
  (setq i 0)

  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq entData (entget ent))

    ; Verify it's an INSERT
    (if (= "INSERT" (cdr (assoc 0 entData)))
      (progn
        (setq blockInsPoint (cdr (assoc 10 entData)))
        (setq posTerm nil)
        (setq negTerm nil)

        ; Iterate through attributes
        (setq attEnt (entnext ent))
        (while (and attEnt (= "ATTRIB" (cdr (assoc 0 (entget attEnt)))))
          (setq attData (entget attEnt))
          (setq attTag (cdr (assoc 2 attData)))      ; Attribute tag
          (setq attPos (cdr (assoc 10 attData)))     ; Attribute position (TERMINAL LOCATION!)

          ; Force Z=0 for reliability
          (setq attPos (list (car attPos) (cadr attPos) 0.0))

          ; Check for PWR_POS attribute (case-insensitive)
          (if (wcmatch (strcase attTag) "*PWR_POS*")
            (setq posTerm attPos)
          )

          ; Check for PWR_NEG attribute (case-insensitive)
          (if (wcmatch (strcase attTag) "*PWR_NEG*")
            (setq negTerm attPos)
          )

          (setq attEnt (entnext attEnt))
        )

        ; If both terminals found, add to list
        (if (and posTerm negTerm)
          (setq blockList (cons (list ent blockInsPoint posTerm negTerm) blockList))
        )
      )
    )
    (setq i (1+ i))
  )

  ; Reverse list to maintain selection order (cons builds in reverse)
  (reverse blockList)
)

; ============================================================================
; HELPER: Sort blocks by insertion point X-coordinate (left to right)
; ============================================================================
(defun sort-blocks-left-to-right (blockList)
  (vl-sort blockList
    '(lambda (a b)
       (< (car (cadr a)) (car (cadr b)))  ; Compare X of block insertion points
     )
  )
)

; ============================================================================
; HELPER: DRAW JUNCTION DOT
; Uses a Circle + Solid Hatch (Cleaner than VBA's line segments)
; ============================================================================
(defun draw-junction-dot (cenPt rad / lastEnt hatchEnt entData currentColor)
  ; Get the current color setting
  (setq currentColor (getvar "CECOLOR"))

  ; Draw the circle
  (command "CIRCLE" cenPt rad)
  (setq lastEnt (entlast))

  ; Hatch it Solid
  ; Note: -HATCH is the command line version.
  ; "s" selects object, "lastEnt" is the circle, "" finishes selection
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

(princ "\nAuto Power Bus loaded. Type AUTOPOWERBUS to run.")
(princ)

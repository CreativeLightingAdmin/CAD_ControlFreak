; ============================================================================
; AUTO POWER BUS - Automatic Power Bus Wiring from Smart Blocks
; Reads block attributes (PWR_POS, PWR_NEG) to automatically wire power buses
; Uses SmartJumper_Lib for drawing and automatic bridge detection
;
; Command: AUTOPOWERBUS (select blocks)
; Core:    (cf:wire-power ss) - no prompts, used by CFBUILD
; ============================================================================

(vl-load-com)

(defun c:autopowerbus ( / *error* ss)

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

  (princ "\n--- AUTO POWER BUS WIRING ---")
  (princ "\nSelect blocks to wire together...")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss)
    (progn (princ "\nNo blocks selected.") (exit))
  )

  (if (cf:wire-power ss)
    (princ "\nAuto Power Bus Complete.")
  )
  (princ)
)

; ============================================================================
; CORE: Wire the power bus between the blocks in selection set ss
; Returns the Y of the top (negative) bus, or nil if fewer than 2 blocks
; have PWR_POS/PWR_NEG
; ============================================================================
(defun cf:wire-power (ss / blockList posTermList skippedCount maxY yPosBus yNegBus
                           posDots negDots routes posRoutes negRoutes box
                           ArcOffset NegBusOffset BrRad DotRad ColPos ColNeg)

  ; --- CONFIGURATION CONSTANTS ---
  (setq ArcOffset 20.0)        ; Height of bus above highest terminal
  (setq NegBusOffset 5.0)      ; Height of Negative bus above Positive
  (setq BrRad 2.0)             ; Jumper Bridge Radius
  (setq DotRad 1.0)            ; Junction Dot Radius
  (setq ColPos 1)              ; Red
  (setq ColNeg 5)              ; Blue

  ; 1. COLLECT BLOCKS WITH ATTRIBUTES
  (setq blockList (collect-power-blocks ss))
  (setq skippedCount (- (sslength ss) (length blockList)))

  (if (> skippedCount 0)
    (princ (strcat "\nPower: " (itoa skippedCount) " block(s) skipped (no PWR_POS/PWR_NEG attributes)"))
  )
  (princ (strcat "\nPower: " (itoa (length blockList)) " block(s) to wire"))

  (if (< (length blockList) 2)
    (progn
      (princ "\nPower: need at least 2 blocks with PWR_POS/PWR_NEG attributes.")
      nil
    )
    (progn
      ; 2. SORT BLOCKS LEFT-TO-RIGHT
      (setq blockList (sort-blocks-left-to-right blockList))

      ; 3. CALCULATE MAX HEIGHT
      (setq posTermList (mapcar 'caddr blockList))
      (setq maxY (apply 'max (mapcar 'cadr posTermList)))

      ; A block with its power terminals on the bottom must still sit below the bus
      (foreach blkItem blockList
        (if (setq box (cf:bbox (car blkItem)))
          (if (< (cadr (caddr blkItem)) (/ (+ (cadar box) (cadadr box)) 2.0))
            (setq maxY (max maxY (cadadr box)))
          )
        )
      )

      ; 4. CALCULATE BUS HEIGHTS
      (setq yPosBus (+ maxY ArcOffset))
      (setq yNegBus (+ yPosBus NegBusOffset))

      ; 5. ROUTE EACH DEVICE (terminals on the bottom of a block go round it)
      (foreach blkItem blockList
        (setq routes (cf:device-routes (car blkItem)
                                       (list (caddr blkItem) (cadddr blkItem))
                                       (list yPosBus yNegBus)))
        (setq posRoutes (append posRoutes (list (car routes))))
        (setq negRoutes (append negRoutes (list (cadr routes))))
      )

      ; PHASE 1: POSITIVE BUS (RED)
      ; Bus spans leftmost to rightmost terminal, whatever the block order
      (princ "\nDrawing Positive Bus...")
      (setq posDots (cf:draw-bus posRoutes yPosBus ColPos nil BrRad))

      ; PHASE 2: NEGATIVE BUS (BLUE)
      ; Every riser uses smart-wire, so any riser crossing the positive
      ; bus gets a bridge - including the first device's
      (princ "\nDrawing Negative Bus...")
      (setq negDots (cf:draw-bus negRoutes yNegBus ColNeg T BrRad))

      ; PHASE 3: JUNCTION DOTS
      (foreach pt posDots (draw-junction-dot pt DotRad ColPos))
      (foreach pt negDots (draw-junction-dot pt DotRad ColNeg))

      yNegBus
    )
  )
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
          (setq attPos (cf:att-point attData))       ; Attribute anchor (TERMINAL LOCATION!)

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

(princ "\nAuto Power Bus loaded. Type AUTOPOWERBUS to run.")
(princ)

; ============================================================================
; AUTO INPUT DEVICE - Automatic Input Device Placement and Wiring
;
; PURPOSE:
;   Automatically places and wires input devices (switches) to a control block
;   based on user-specified input types. Supports momentary/latching switches,
;   spares (skip), and fanouts.
;
; DEPENDENCIES:
;   - SmartJumper_Lib.lsp (REQUIRED - must be loaded first)
;
; USAGE:
;   1. Ensure SmartJumper_Lib.lsp is loaded
;   2. Type: AUTOINPUT
;   3. Select control device block (must have IN1, IN2, IN3... attributes)
;   4. Enter input types when prompted (M/L/S/F separated by spaces)
;   5. Function will place switches below control device and wire them
;
; INPUT TYPES:
;   M = Momentary switch (places BLOCK_MOMENTARY and wires to INx)
;   L = Latching switch (places BLOCK_LATCHING and wires to INx)
;   S = Spare (skips this input entirely - no block, no wire)
;   F = Fanout (short drop + label at INx for connection to other devices)
;
; REQUIREMENTS:
;   Control Device Block:
;     - Must have attributes named: IN1, IN2, IN3, IN4... (sequential)
;     - Attributes mark terminal locations for wiring
;
;   Switch Blocks:
;     - Must have attributes named OUT and GND on the two terminals
;       (not "Constant" attributes)
;
; LAYOUT:
;   - First MAX_INPUTS_PER_COLUMN inputs: column LEFT of the control device,
;     next MAX_INPUTS_PER_COLUMN: column RIGHT of it
;   - Rows are ordered so that no wires cross: in the left column the
;     leftmost input is on top, in the right column the rightmost is on top
;   - Column positions and row pitch are worked out from the switch block's
;     actual size, so any switch symbol fits
;   - Each wire: vertical drop -> 45 degree bend -> horizontal to the switch
;     terminal facing the device; the other terminal goes to a ground bus
;
; AUTHOR: Mike
; VERSION: 2.0
; DATE: 2026-10-14
; ============================================================================

(vl-load-com)

; ============================================================================
; CONFIGURATION CONSTANTS
; ============================================================================

; --- Block Names ---
(setq BLOCK_MOMENTARY "SWITCHM")       ; Momentary switch block name
(setq BLOCK_LATCHING "SWITCHL")        ; Latching switch block name

; --- Layout Configuration ---
(setq MAX_INPUTS_PER_COLUMN 6)         ; Maximum switches per vertical stack
(setq SWITCH_SPACING_VERTICAL 20.0)    ; Minimum row pitch (mm)
(setq SWITCH_GAP_VERTICAL 5.0)         ; Minimum clear gap between switches (mm)
(setq COLUMN_GAP 20.0)                 ; Gap from outermost input drop to switch edge (mm)
(setq TOP_CLEARANCE 60.0)              ; Lowest IN point down to top of first switch row (mm)

; --- Wiring Configuration ---
(setq WIRE_DROP_START 5.0)             ; Start wire 5mm south of IN point
(setq WIRE_BEND 5.0)                   ; Size of the 45 degree bend (mm)
(setq FANOUT_DROP 50.0)                ; Fanout wire length below IN point (mm)
(setq WIRE_COLOR 7)                    ; White (general purpose)

; --- Label Configuration ---
(setq LABEL_HEIGHT 2.5)                ; Text height (mm)
(setq LABEL_GAP 1.0)                   ; Gap between switch and its label (mm)
(setq LABEL_OFFSET_X_FANOUT 3.0)       ; Horizontal offset from fanout (mm)
(setq LABEL_COLOR 7)                   ; White

; --- Ground Bus Configuration ---
(setq GND_BUS_COLOR 3)                 ; Green
(setq GND_BUS_HORIZONTAL_LENGTH 20.0)  ; Bus distance beyond outermost GND terminal (mm)
(setq GND_BUS_DOT_RADIUS 1.0)          ; Junction dot radius

; --- Block Insertion Parameters ---
(setq BLOCK_SCALE 1.0)                 ; Scale factor for inserted blocks
(setq BLOCK_ROTATION 0.0)              ; Rotation angle (0 = no rotation)

; ============================================================================
; HELPER FUNCTIONS
; ============================================================================

; ----------------------------------------------------------------------------
; GET CONTROL DEVICE
; Prompts user to select control block
; Returns: Block entity name or nil
; ----------------------------------------------------------------------------
(defun get-control-device ( / ss)
  (princ "\nSelect control device block: ")
  (setq ss (ssget ":S" '((0 . "INSERT"))))
  (if ss (ssname ss 0))
)

; ----------------------------------------------------------------------------
; EXTRACT INPUT ATTRIBUTES
; Finds all INx attributes in control block and returns sorted list
; Returns: ((number tag position) ...) sorted by number
; ----------------------------------------------------------------------------
(defun extract-input-attributes (blockEnt / attrList attEnt attData attTag)
  (setq attEnt (entnext blockEnt))
  (while (and attEnt (= "ATTRIB" (cdr (assoc 0 (entget attEnt)))))
    (setq attData (entget attEnt))
    (setq attTag (cdr (assoc 2 attData)))

    ; Tag is "IN" followed by digits
    (if (wcmatch (strcase attTag) "IN#*")
      (setq attrList (cons (list (atoi (substr attTag 3)) attTag (cf:att-point attData))
                           attrList))
    )
    (setq attEnt (entnext attEnt))
  )
  (vl-sort attrList '(lambda (a b) (< (car a) (car b))))
)

; ----------------------------------------------------------------------------
; PARSE INPUT STRING (WITH AUTO-FILL)
; "M M L" with numInputs=6 -> ("M" "M" "L" "S" "S" "S"), or nil on error
; ----------------------------------------------------------------------------
(defun parse-input-string (inputStr numInputs / charList currentChar i)
  (setq inputStr (strcase inputStr))
  (setq i 1)

  (while (<= i (strlen inputStr))
    (setq currentChar (substr inputStr i 1))
    (if (/= currentChar " ")
      (if (wcmatch currentChar "M,L,S,F")
        (setq charList (append charList (list currentChar)))
        (progn
          (princ (strcat "\nError: Invalid character '" currentChar "'"))
          (setq charList nil)
          (setq i (strlen inputStr))  ; Exit loop
        )
      )
    )
    (setq i (1+ i))
  )

  (if charList
    (progn
      (if (> (length charList) numInputs)
        (progn
          (princ (strcat "\nWarning: Too many inputs (" (itoa (length charList))
                        "), using first " (itoa numInputs)))
          (setq charList (reverse (nthcdr (- (length charList) numInputs) (reverse charList))))
        )
      )
      (while (< (length charList) numInputs)
        (setq charList (append charList (list "S")))
      )
    )
  )
  charList
)

; ----------------------------------------------------------------------------
; MISSING SWITCH BLOCKS
; Names of the switch blocks the input types need but the drawing doesn't have
; ----------------------------------------------------------------------------
(defun missing-switch-blocks (inputTypes / needed)
  (if (member "M" inputTypes) (setq needed (cons BLOCK_MOMENTARY needed)))
  (if (member "L" inputTypes) (setq needed (cons BLOCK_LATCHING needed)))
  (vl-remove-if '(lambda (b) (tblsearch "BLOCK" b)) needed)
)

(defun cf:join (strs sep / res)
  (foreach s strs
    (setq res (if res (strcat res sep s) s))
  )
  res
)

; ----------------------------------------------------------------------------
; INSERT SWITCH BLOCK
; Returns: VLA object of the inserted block or nil
; ----------------------------------------------------------------------------
(defun insert-switch-block (blockName insertPt / obj)
  (setq obj (vl-catch-all-apply 'vla-InsertBlock
              (list (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                    (vlax-3d-point insertPt)
                    blockName
                    BLOCK_SCALE BLOCK_SCALE BLOCK_SCALE
                    BLOCK_ROTATION)))
  (if (vl-catch-all-error-p obj)
    (progn
      (princ (strcat "\nWarning: Failed to insert block '" blockName "': "
                     (vl-catch-all-error-message obj)))
      nil
    )
    obj
  )
)

; ----------------------------------------------------------------------------
; SWITCH EXTENTS
; Size of the switch blocks relative to their insertion point, measured by
; inserting each at 0,0 and deleting it again.
; Returns: ((minX minY) (maxX maxY)) covering all given blocks, or nil
; ----------------------------------------------------------------------------
(defun switch-extents (blockNames / obj res mn mx loX loY hiX hiY)
  (foreach name blockNames
    (if (setq obj (insert-switch-block name '(0.0 0.0 0.0)))
      (progn
        (setq res (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
        (vla-delete obj)
        (if (not (vl-catch-all-error-p res))
          (progn
            (setq mn (vlax-safearray->list mn))
            (setq mx (vlax-safearray->list mx))
            (setq loX (if loX (min loX (car mn)) (car mn)))
            (setq loY (if loY (min loY (cadr mn)) (cadr mn)))
            (setq hiX (if hiX (max hiX (car mx)) (car mx)))
            (setq hiY (if hiY (max hiY (cadr mx)) (cadr mx)))
          )
        )
      )
    )
  )
  (if loX (list (list loX loY) (list hiX hiY)))
)

; ----------------------------------------------------------------------------
; FIND SWITCH CONNECTION TERMINAL
; Finds the OUT or GND attribute position in a switch block
; Returns: Terminal position (x y z) or nil
; ----------------------------------------------------------------------------
(defun find-switch-connection-terminal (switchEnt attrName / attEnt attData pos)
  (setq attEnt (entnext switchEnt))
  (while (and attEnt
              (= "ATTRIB" (cdr (assoc 0 (entget attEnt))))
              (not pos))
    (setq attData (entget attEnt))
    (if (wcmatch (strcase (cdr (assoc 2 attData))) (strcat "*" (strcase attrName) "*"))
      (setq pos (cf:att-point attData))
    )
    (setq attEnt (entnext attEnt))
  )
  pos
)

; ----------------------------------------------------------------------------
; CREATE INPUT LABEL
; Left-justified "INx: " text in WCS, rotation in degrees
; ----------------------------------------------------------------------------
(defun create-input-label (inputNumber position rotation)
  (entmake (list '(0 . "TEXT")
                 (cons 8 (getvar "CLAYER"))
                 (cons 62 LABEL_COLOR)
                 (cons 7 (getvar "TEXTSTYLE"))
                 (cons 10 (list (car position) (cadr position) 0.0))
                 (cons 40 LABEL_HEIGHT)
                 (cons 50 (* rotation (/ pi 180.0)))
                 (cons 1 (strcat "IN" (itoa inputNumber) ": "))))
  T
)

; ----------------------------------------------------------------------------
; CALCULATE PLACEMENT POSITIONS
; Returns: ((index type insertPt inxPos side) ...)  side = -1 left, 1 right
; Left column: IN1..IN6, leftmost input on the top row.
; Right column: IN7..IN12, rightmost input on the top row.
; This ordering means no wire ever has to cross another.
; ----------------------------------------------------------------------------
(defun calculate-placement-positions (inputAttrs inputTypes ext / xs minX maxX minY
                                       pitch topY leftX rightX rightCount
                                       i side row inxPos result)
  (setq xs   (mapcar '(lambda (a) (car (caddr a))) inputAttrs))
  (setq minX (apply 'min xs))
  (setq maxX (apply 'max xs))
  (setq minY (apply 'min (mapcar '(lambda (a) (cadr (caddr a))) inputAttrs)))

  ; Row pitch from block height, never less than the configured minimum
  (setq pitch (max SWITCH_SPACING_VERTICAL
                   (+ (- (cadadr ext) (cadar ext)) SWITCH_GAP_VERTICAL)))

  ; Top row: block top edge TOP_CLEARANCE below the lowest IN point
  (setq topY (- minY TOP_CLEARANCE (cadadr ext)))

  ; Columns: block's near edge COLUMN_GAP clear of the outermost input drops
  (setq leftX  (- minX COLUMN_GAP (caadr ext)))
  (setq rightX (- (+ maxX COLUMN_GAP) (caar ext)))

  (setq rightCount (max 0 (min MAX_INPUTS_PER_COLUMN
                               (- (length inputAttrs) MAX_INPUTS_PER_COLUMN))))

  (setq i 0)
  (foreach inputType inputTypes
    (setq inxPos (caddr (nth i inputAttrs)))
    (if (< i MAX_INPUTS_PER_COLUMN)
      (setq side -1
            row  i)
      (setq side 1
            row  (- rightCount 1 (- i MAX_INPUTS_PER_COLUMN)))
    )
    (setq result (cons (list i inputType
                             (list (if (= side -1) leftX rightX) (- topY (* row pitch)) 0.0)
                             inxPos side)
                       result))
    (setq i (1+ i))
  )
  (reverse result)
)

; ----------------------------------------------------------------------------
; WIRE SWITCH
; Vertical drop from INx -> 45 degree bend -> horizontal to the switch
; terminal facing the device. Returns the other (ground) terminal or nil.
; ----------------------------------------------------------------------------
(defun wire-switch (switchEnt inxPos side / outPos gndPos inTerm gndTerm p0 p1 p2)
  (setq outPos (find-switch-connection-terminal switchEnt "OUT"))
  (setq gndPos (find-switch-connection-terminal switchEnt "GND"))

  ; The terminal nearer the device takes the input, the other goes to ground
  (cond
    ((and outPos gndPos)
      (if (< (abs (- (car outPos) (car inxPos))) (abs (- (car gndPos) (car inxPos))))
        (setq inTerm outPos gndTerm gndPos)
        (setq inTerm gndPos gndTerm outPos)
      )
    )
    (T (setq inTerm (if outPos outPos gndPos)))
  )

  (if inTerm
    (progn
      (setq p0 (list (car inxPos) (- (cadr inxPos) WIRE_DROP_START) 0.0))
      (setq p1 (list (car inxPos) (+ (cadr inTerm) WIRE_BEND) 0.0))
      (setq p2 (list (+ (car inxPos) (* side WIRE_BEND)) (cadr inTerm) 0.0))
      (cf:line p0 p1 WIRE_COLOR)
      (cf:line p1 p2 WIRE_COLOR)
      (cf:line p2 inTerm WIRE_COLOR)
    )
    (princ "\nWarning: OUT/GND attributes not found on a switch block")
  )
  gndTerm
)

; ----------------------------------------------------------------------------
; DRAW GROUND BUS
; Horizontal from each ground terminal (away from the device) to a vertical
; bus GND_BUS_HORIZONTAL_LENGTH beyond the outermost terminal.
; ----------------------------------------------------------------------------
(defun draw-ground-bus (gndTerms side / xs busX ys)
  (if gndTerms
    (progn
      (setq xs (mapcar 'car gndTerms))
      (setq busX (if (= side -1)
                   (- (apply 'min xs) GND_BUS_HORIZONTAL_LENGTH)
                   (+ (apply 'max xs) GND_BUS_HORIZONTAL_LENGTH)))
      (setq ys (mapcar 'cadr gndTerms))

      (foreach g gndTerms
        (cf:line g (list busX (cadr g) 0.0) GND_BUS_COLOR)
        (draw-junction-dot (list busX (cadr g) 0.0) GND_BUS_DOT_RADIUS GND_BUS_COLOR)
      )
      (if (> (length gndTerms) 1)
        (cf:line (list busX (apply 'min ys) 0.0) (list busX (apply 'max ys) 0.0) GND_BUS_COLOR)
      )
    )
  )
)

; ============================================================================
; MAIN COMMAND FUNCTION
; ============================================================================

(defun c:autoinput ( / *error* controlBlock inputAttrs inputTypes numInputs userInput
                      missing ext placementList currentType currentPos inxPos side
                      blockName switchObj switchEnt gndTerm gndLeft gndRight)

  ; --- ERROR HANDLER ---
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ; --- DEPENDENCY CHECK ---
  (if (not cf:line)
    (progn
      (alert "Error: SmartJumper_Lib.lsp is not loaded!\nLoad ControlFreak.lsp first.")
      (exit)
    )
  )

  (princ "\n--- AUTO INPUT DEVICE PLACEMENT ---")
  (princ "\n  M = Momentary switch   L = Latching switch")
  (princ "\n  S = Spare (skip)       F = Fanout")

  ; --- STEP 1: SELECT CONTROL DEVICE ---
  (setq controlBlock (get-control-device))
  (if (not controlBlock)
    (progn (princ "\nNo block selected. Command cancelled.") (exit))
  )

  ; --- STEP 2: EXTRACT INPUT ATTRIBUTES ---
  (setq inputAttrs (extract-input-attributes controlBlock))
  (if (not inputAttrs)
    (progn
      (alert "Error: No INx attributes found on selected block!\nControl device must have IN1, IN2, IN3... attributes.")
      (exit)
    )
  )
  (if (> (length inputAttrs) (* 2 MAX_INPUTS_PER_COLUMN))
    (progn
      (princ (strcat "\nWarning: only the first " (itoa (* 2 MAX_INPUTS_PER_COLUMN))
                     " inputs can be laid out; the rest are ignored."))
      (setq inputAttrs (reverse (nthcdr (- (length inputAttrs) (* 2 MAX_INPUTS_PER_COLUMN))
                                        (reverse inputAttrs))))
    )
  )

  (setq numInputs (length inputAttrs))
  (princ (strcat "\nFound " (itoa numInputs) " inputs: "))
  (foreach attr inputAttrs (princ (strcat (cadr attr) " ")))

  ; --- STEP 3: GET USER INPUT STRING ---
  (setq userInput (getstring T (strcat "\nEnter up to " (itoa numInputs)
                                       " input types (M/L/S/F) separated by spaces: ")))
  (if (not userInput) (setq userInput ""))

  ; --- STEP 4: PARSE AND VALIDATE INPUT (WITH AUTO-FILL) ---
  (setq inputTypes (parse-input-string userInput numInputs))
  (if (not inputTypes)
    (progn
      (alert "Error: Invalid input string!\nUse only M, L, S, or F separated by spaces.")
      (exit)
    )
  )
  (if (< (length (vl-remove 32 (vl-string->list userInput))) numInputs)
    (princ "\nAuto-filled remaining inputs as 'S' (spare)")
  )

  ; --- STEP 5: CHECK SWITCH BLOCKS ARE AVAILABLE ---
  ; Import them from the product library if needed, otherwise stop with a
  ; clear message rather than silently skipping every switch
  (setq missing (missing-switch-blocks inputTypes))
  (if (and missing c:cfblocks)
    (progn
      (princ "\nSwitch block(s) not in this drawing - importing product library...")
      (c:cfblocks)
      (setq missing (missing-switch-blocks inputTypes))
    )
  )
  (if missing
    (progn
      (alert (strcat "Switch block(s) not found: " (cf:join missing ", ")
                     "\n\nAdd them to the product library (with OUT and GND attributes),"
                     "\nor change BLOCK_MOMENTARY / BLOCK_LATCHING at the top of AutoInputDevice.lsp."))
      (exit)
    )
  )

  ; --- STEP 6: MEASURE SWITCH BLOCKS AND CALCULATE LAYOUT ---
  (setq ext (switch-extents
              (vl-remove-if-not '(lambda (b) (tblsearch "BLOCK" b))
                                (list BLOCK_MOMENTARY BLOCK_LATCHING))))
  (if (not ext)
    (progn
      (princ "\nWarning: couldn't measure the switch blocks - assuming 20 x 20mm.")
      (setq ext '((-10.0 -10.0) (10.0 10.0)))
    )
  )
  (setq placementList (calculate-placement-positions inputAttrs inputTypes ext))

  ; --- STEP 7: PLACE AND WIRE ---
  (princ "\nPlacing and wiring...")
  (foreach placement placementList
    (setq currentType (nth 1 placement))
    (setq currentPos  (nth 2 placement))
    (setq inxPos      (nth 3 placement))
    (setq side        (nth 4 placement))

    (cond
      ((member currentType '("M" "L"))
        (setq blockName (if (= currentType "M") BLOCK_MOMENTARY BLOCK_LATCHING))
        (if (setq switchObj (insert-switch-block blockName currentPos))
          (progn
            (setq switchEnt (vlax-vla-object->ename switchObj))
            (setq gndTerm (wire-switch switchEnt inxPos side))
            (if gndTerm
              (if (= side -1)
                (setq gndLeft (cons gndTerm gndLeft))
                (setq gndRight (cons gndTerm gndRight))
              )
            )
            ; Label at the top-right corner of the switch
            (create-input-label (1+ (car placement))
                                (list (+ (car currentPos) (caadr ext) LABEL_GAP)
                                      (+ (cadr currentPos) (cadadr ext) (- LABEL_HEIGHT))
                                      0.0)
                                0)
          )
        )
      )
      ((= currentType "F")
        ; Short vertical drop, ready to connect to other devices
        (cf:line (list (car inxPos) (- (cadr inxPos) WIRE_DROP_START) 0.0)
                 (list (car inxPos) (- (cadr inxPos) FANOUT_DROP) 0.0)
                 WIRE_COLOR)
        (create-input-label (1+ (car placement))
                            (list (+ (car inxPos) LABEL_OFFSET_X_FANOUT)
                                  (- (cadr inxPos) FANOUT_DROP)
                                  0.0)
                            90)
      )
      ; Type "S" - no action
    )
  )

  ; --- STEP 8: GROUND BUSES (one per column, on the outer side) ---
  (princ "\nDrawing ground buses...")
  (draw-ground-bus gndLeft -1)
  (draw-ground-bus gndRight 1)

  (princ "\nAuto Input Device Placement Complete.")
  (princ)
)

(princ "\nAuto Input Device loaded. Type AUTOINPUT to run.")
(princ)

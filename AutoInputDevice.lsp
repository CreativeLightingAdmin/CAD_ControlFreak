; ============================================================================
; AUTO INPUT DEVICE - Automatic Input Device Placement and Wiring
;
; PURPOSE:
;   Automatically places and wires input devices (switches) to a control block
;   based on user-specified input types. Supports momentary/latching switches,
;   spares (skip), and fanouts (junction dots).
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
;   F = Fanout (places junction dot at INx for connection to other devices)
;
; CONFIGURATION:
;   Before first use, edit the configuration constants below to match your
;   block library:
;     - BLOCK_MOMENTARY: Name of your momentary switch block
;     - BLOCK_LATCHING: Name of your latching switch block
;
; REQUIREMENTS:
;   Control Device Block:
;     - Must have attributes named: IN1, IN2, IN3, IN4... (sequential)
;     - Attributes mark terminal locations for wiring
;
;   Switch Blocks:
;     - Must have attribute named: OUT (output terminal)
;     - Must have attribute named: GND (ground terminal - future use)
;
; LAYOUT:
;   - Switches placed vertically below control device
;   - 10mm spacing between switches (configurable)
;   - Left-to-right alignment with control device inputs
;   - Wires use smart-wire for automatic bridge detection
;
; AUTHOR: Mike
; VERSION: 1.0
; DATE: 2025-12-10
; ============================================================================

(vl-load-com)

; ============================================================================
; CONFIGURATION CONSTANTS
; ============================================================================
; IMPORTANT: Customize these values to match your block library!

; --- Block Names (USER MUST CUSTOMIZE THESE) ---
(setq BLOCK_MOMENTARY "SWITCHM")       ; Momentary switch block name
(setq BLOCK_LATCHING "SWITCHL")        ; Latching switch block name

; --- Layout Configuration ---
(setq MAX_INPUTS_PER_COLUMN 6)         ; Maximum switches per vertical stack
(setq SWITCH_SPACING_VERTICAL 20.0)    ; Vertical spacing between switches (mm)
(setq COLUMN_SPACING_HORIZONTAL 30.0)  ; Horizontal spacing between columns (mm)

; --- Wiring Configuration ---
(setq WIRE_DROP_START 5.0)             ; Start wire 5mm south of IN point
(setq WIRE_DROP_END 61.86)             ; End wire 61.86mm south of IN point (aligns with OUT terminal)
(setq WIRE_DROP_LENGTH 56.86)          ; Total vertical drop (61.86 - 5 = 56.86mm)
(setq BEND_RADIUS 2.0)                 ; 90° bend radius
(setq WIRE_COLOR 7)                    ; White (general purpose)
(setq BRIDGE_RADIUS 2.0)               ; Bridge radius for smart-wire

; --- Junction Dot Configuration ---
(setq DOT_RADIUS 1.0)                  ; Junction dot radius for fanouts
(setq FANOUT_COLOR 7)                  ; White for fanout junction dots

; --- Label Configuration ---
(setq LABEL_HEIGHT 2.5)                ; Text height (mm)
(setq LABEL_OFFSET_X_SWITCH 5.0)       ; Horizontal offset from switch (mm)
(setq LABEL_OFFSET_X_FANOUT 3.0)       ; Horizontal offset from fanout (mm)
(setq LABEL_COLOR 7)                   ; White

; --- Ground Bus Configuration (Column 0 only for now) ---
(setq GND_BUS_COLOR 3)                 ; Green
(setq GND_BUS_HORIZONTAL_LENGTH 20.0) ; Horizontal extension from switch (mm)
(setq GND_BUS_DOT_RADIUS 1.0)          ; Junction dot radius

; --- Block Insertion Parameters ---
(setq BLOCK_SCALE 1.0)                 ; Scale factor for inserted blocks
(setq BLOCK_ROTATION 0.0)              ; Rotation angle (0 = no rotation)

; ============================================================================
; HELPER FUNCTIONS
; ============================================================================

; ----------------------------------------------------------------------------
; Check if a command exists (Safety for ProgeCAD versions)
; ----------------------------------------------------------------------------
(defun fnob (x) (if (getcname x) T nil))

; ----------------------------------------------------------------------------
; DRAW JUNCTION DOT
; Uses a Circle + Solid Hatch (Cleaner than VBA's line segments)
; ----------------------------------------------------------------------------
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

; ----------------------------------------------------------------------------
; GET CONTROL DEVICE
; Prompts user to select control block
; Returns: Block entity name or nil
; ----------------------------------------------------------------------------
(defun get-control-device ( / ss ent)
  (princ "\nSelect control device block: ")
  (setq ss (ssget ":S" '((0 . "INSERT"))))
  (if ss
    (setq ent (ssname ss 0))
    nil
  )
)

; ----------------------------------------------------------------------------
; EXTRACT INPUT ATTRIBUTES
; Finds all INx attributes in control block and returns sorted list
; Input: Block entity name
; Returns: List of attribute data: ((number tag position) ...)
;          Example: ((1 "IN1" (100.0 200.0 0.0)) (2 "IN2" (110.0 200.0 0.0)) ...)
; ----------------------------------------------------------------------------
(defun extract-input-attributes (blockEnt / attrList attEnt attData attTag attPos inputNum)
  (setq attrList '())

  ; Get first attribute
  (setq attEnt (entnext blockEnt))

  ; Loop through all attributes
  (while (and attEnt (= "ATTRIB" (cdr (assoc 0 (entget attEnt)))))
    (setq attData (entget attEnt))
    (setq attTag (cdr (assoc 2 attData)))      ; Attribute tag

    ; Check if tag matches pattern "IN" followed by digits
    (if (wcmatch (strcase attTag) "IN[0-9]*")
      (progn
        (setq attPos (cdr (assoc 10 attData)))   ; Attribute position (terminal location)
        ; Force Z=0 for reliability
        (setq attPos (list (car attPos) (cadr attPos) 0.0))
        ; Extract number from tag (e.g., "IN1" -> 1)
        (setq inputNum (atoi (substr attTag 3)))
        ; Store: (number tag position)
        (setq attrList (cons (list inputNum attTag attPos) attrList))
      )
    )

    (setq attEnt (entnext attEnt))
  )

  ; Sort list by number to ensure IN1, IN2, IN3... order
  (vl-sort attrList '(lambda (a b) (< (car a) (car b))))
)

; ----------------------------------------------------------------------------
; PARSE INPUT STRING (WITH AUTO-FILL)
; Converts user input string into list of input types
; Input: inputStr - String like "M M L" (can be partial)
;        numInputs - Total number of inputs to fill
; Returns: List of uppercase characters padded with "S" to match numInputs length
;          Example: "M M L" with numInputs=6 → ("M" "M" "L" "S" "S" "S")
;          or nil on error
; ----------------------------------------------------------------------------
(defun parse-input-string (inputStr numInputs / charList currentChar i)
  (setq inputStr (strcase inputStr))  ; Convert to uppercase
  (setq charList '())
  (setq i 1)

  ; Loop through each character
  (while (<= i (strlen inputStr))
    (setq currentChar (substr inputStr i 1))
    ; Only add non-space characters
    (if (not (= currentChar " "))
      (if (wcmatch currentChar "M,L,S,F")
        (setq charList (append charList (list currentChar)))
        (progn
          (princ (strcat "\nError: Invalid character '" currentChar "'"))
          (setq charList nil)
          (setq i (+ (strlen inputStr) 1))  ; Exit loop
        )
      )
    )
    (setq i (1+ i))
  )

  ; Auto-fill with "S" if needed, or truncate if too many
  (if charList
    (progn
      ; If too many inputs, truncate to numInputs
      (if (> (length charList) numInputs)
        (progn
          (princ (strcat "\nWarning: Too many inputs (" (itoa (length charList))
                        "), using first " (itoa numInputs)))
          ; Take only first numInputs elements
          (setq charList (reverse (nthcdr (- (length charList) numInputs) (reverse charList))))
        )
      )
      ; If too few inputs, pad with "S"
      (while (< (length charList) numInputs)
        (setq charList (append charList (list "S")))
      )
    )
  )

  charList
)

; ----------------------------------------------------------------------------
; CALCULATE PLACEMENT POSITIONS (COLUMN-BASED LAYOUT)
; Calculates insertion points for switch blocks in vertical columns
; Max 6 switches per column, column 0 on LEFT, column 1 on RIGHT
; Input: Input attribute list from extract-input-attributes
;        Input type list from parse-input-string
; Returns: Association list: ((index type insertPt inxPos) ...)
; ----------------------------------------------------------------------------
(defun calculate-placement-positions (inputAttrs inputTypes / placementList minY minX leftX rightX
                                       baseX baseY i inputAttr inputType inxPos
                                       columnIndex rowIndex x y insertPt col1RefX)
  (setq placementList '())

  ; Find leftmost X coordinate and lowest Y coordinate from all INx attributes
  (setq minX 1000000.0)
  (setq minY 1000000.0)
  (foreach attr inputAttrs
    (setq x (car (caddr attr)))   ; Get X from position
    (setq y (cadr (caddr attr)))  ; Get Y from position
    (if (< x minX) (setq minX x))
    (if (< y minY) (setq minY y))
  )

  ; Calculate base X positions for each column
  (setq leftX (- minX 20.0))  ; Left column: 20mm to the left of leftmost IN point

  ; For right column, use the X coordinate of the 7th input (index 6) if it exists
  (if (>= (length inputAttrs) 7)
    (setq col1RefX (car (caddr (nth 6 inputAttrs))))  ; X of IN7
    (setq col1RefX minX)  ; Fallback if less than 7 inputs
  )
  (setq rightX (+ col1RefX 20.0))  ; Right column: 20mm to the right of IN7

  (setq baseY (- minY WIRE_DROP_END 10.0))  ; 61.86mm below + 10mm clearance

  ; Build placement list with column-based layout
  (setq i 0)
  (while (< i (length inputTypes))
    (setq inputAttr (nth i inputAttrs))
    (setq inputType (nth i inputTypes))
    (setq inxPos (caddr inputAttr))  ; INx position

    ; Calculate column and row
    (setq columnIndex (/ i MAX_INPUTS_PER_COLUMN))  ; Integer division
    (setq rowIndex (rem i MAX_INPUTS_PER_COLUMN))   ; Remainder (row in column)

    ; Calculate insertion point with FIXED X per column
    ; Column 0 (IN1-6): Fixed X at leftX, normal stacking (top to bottom)
    ; Column 1 (IN7-12): Fixed X at rightX, flipped stacking (bottom to top)
    (if (= columnIndex 0)
      (progn
        (setq x leftX)
        (setq y (- baseY (* rowIndex SWITCH_SPACING_VERTICAL)))
      )
      (progn
        (setq x rightX)
        (setq y (- baseY (* (- (- MAX_INPUTS_PER_COLUMN 1) rowIndex) SWITCH_SPACING_VERTICAL)))
      )
    )
    (setq insertPt (list x y 0.0))

    ; Add to placement list
    (setq placementList (append placementList
                                (list (list i inputType insertPt inxPos))))

    (setq i (1+ i))
  )

  placementList
)

; ----------------------------------------------------------------------------
; INSERT SWITCH BLOCK
; Inserts a switch block at specified position using VLA-COM with error handling
; Input: Block name, insertion point
; Returns: Inserted block entity name or nil
; ----------------------------------------------------------------------------
(defun insert-switch-block (blockName insertPt / blockDef doc mSpace blockObj result errObj)
  ; Check if block exists
  (setq blockDef (tblsearch "BLOCK" blockName))
  (if (not blockDef)
    (progn
      (princ (strcat "\nWarning: Block '" blockName "' not found in drawing. Skipping..."))
      nil
    )
    (progn
      ; Get the active document and modelspace
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (setq mSpace (vla-get-modelspace doc))

      ; Insert block using VLA-COM with error handling
      (setq errObj (vl-catch-all-apply 'vla-InsertBlock
                     (list mSpace
                           (vlax-3d-point insertPt)
                           blockName
                           BLOCK_SCALE
                           BLOCK_SCALE
                           BLOCK_SCALE
                           BLOCK_ROTATION)))

      ; Check if insertion was successful
      (if (vl-catch-all-error-p errObj)
        (progn
          (princ (strcat "\nWarning: Failed to insert block '" blockName "': "
                        (vl-catch-all-error-message errObj)))
          nil
        )
        (progn
          ; Convert VLA object to entity name
          (setq result (vlax-vla-object->ename errObj))
          result
        )
      )
    )
  )
)

; ----------------------------------------------------------------------------
; FIND SWITCH CONNECTION TERMINAL
; Finds the OUT or GND attribute position in a switch block
; Input: Switch block entity name, attribute name ("OUT" or "GND")
; Returns: Terminal position (x y z) or nil
; ----------------------------------------------------------------------------
(defun find-switch-connection-terminal (switchEnt attrName / attEnt attData attTag pos)
  (setq pos nil)

  ; Get first attribute
  (setq attEnt (entnext switchEnt))

  ; Loop through attributes
  (while (and attEnt
              (= "ATTRIB" (cdr (assoc 0 (entget attEnt))))
              (not pos))
    (setq attData (entget attEnt))
    (setq attTag (cdr (assoc 2 attData)))

    ; Find attribute with tag matching the requested attribute
    (if (wcmatch (strcase attTag) (strcat "*" (strcase attrName) "*"))
      (progn
        (setq pos (cdr (assoc 10 attData)))
        ; Force Z=0
        (setq pos (list (car pos) (cadr pos) 0.0))
      )
    )

    (setq attEnt (entnext attEnt))
  )

  pos
)

; ----------------------------------------------------------------------------
; CREATE INPUT LABEL
; Creates text label for input (format: "IN1: ", "IN2: ", etc.)
; Input: inputNumber - the input number (1, 2, 3...)
;        position - (x y z) where to place the label
;        rotation - rotation angle (0 = horizontal, 90 = vertical)
; Returns: T on success
; ----------------------------------------------------------------------------
(defun create-input-label (inputNumber position rotation / labelText)
  ; Format label text
  (setq labelText (strcat "IN" (itoa inputNumber) ": "))

  ; Set color
  (setvar "CECOLOR" (itoa LABEL_COLOR))

  ; Create text with specified rotation
  (command "TEXT" position LABEL_HEIGHT (itoa rotation) labelText)

  T
)

; ============================================================================
; MAIN COMMAND FUNCTION
; ============================================================================

(defun c:autoinput ( / *error* controlBlock inputAttrs inputTypes numInputs userInput
                      placementList oldSnap oldCmd oldColor i placement currentType
                      currentPos inxPos switchBlock outPos ptStart ptEnd rowIndex
                      dropDistance horizStart horizEnd columnIndex terminalAttr termPos
                      ptVertEnd pt45End verticalRemaining gndTermList gndPos gndBusX
                      minGndY maxGndY)

  ; --- ERROR HANDLER ---
  (defun *error* (msg)
    (if oldSnap (setvar "OSMODE" oldSnap))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldColor (setvar "CECOLOR" oldColor))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ; --- DEPENDENCY CHECK ---
  (if (not smart-wire)
    (progn
      (alert "Error: SmartJumper_Lib.lsp is not loaded!\nLoad it first with: (load \"SmartJumper_Lib.lsp\")")
      (exit)
    )
  )

  ; --- DISPLAY INSTRUCTIONS ---
  (princ "\n--- AUTO INPUT DEVICE PLACEMENT ---")
  (princ "\nInstructions:")
  (princ "\n  M = Momentary switch")
  (princ "\n  L = Latching switch")
  (princ "\n  S = Spare (skip)")
  (princ "\n  F = Fanout (junction dot)")

  ; --- STEP 1: SELECT CONTROL DEVICE ---
  (setq controlBlock (get-control-device))
  (if (not controlBlock)
    (progn
      (princ "\nNo block selected. Command cancelled.")
      (exit)
    )
  )

  ; --- STEP 2: EXTRACT INPUT ATTRIBUTES ---
  (setq inputAttrs (extract-input-attributes controlBlock))
  (if (= (length inputAttrs) 0)
    (progn
      (alert "Error: No INx attributes found on selected block!\nControl device must have IN1, IN2, IN3... attributes.")
      (exit)
    )
  )

  (setq numInputs (length inputAttrs))
  (princ (strcat "\nFound " (itoa numInputs) " inputs: "))
  (foreach attr inputAttrs
    (princ (strcat (cadr attr) " "))  ; Print IN1, IN2, IN3...
  )

  ; --- STEP 3: GET USER INPUT STRING ---
  (princ (strcat "\n(remaining will auto-fill as 'S')"))
  (setq userInput (getstring T (strcat "\nEnter up to " (itoa numInputs)
                                       " input types (M/L/S/F) separated by spaces: ")))
  (if (not userInput)
    (setq userInput "")  ; Empty input will auto-fill all as "S"
  )

  ; --- STEP 4: PARSE AND VALIDATE INPUT (WITH AUTO-FILL) ---
  (setq inputTypes (parse-input-string userInput numInputs))
  (if (not inputTypes)
    (progn
      (alert "Error: Invalid input string!\nUse only M, L, S, or F separated by spaces.")
      (exit)
    )
  )

  ; Display what was filled
  (if (< (length (vl-remove "S" (vl-remove " " (vl-string->list (strcase userInput)))))
         numInputs)
    (princ (strcat "\nAuto-filled remaining inputs as 'S' (spare)"))
  )

  ; --- STEP 5: CALCULATE PLACEMENT POSITIONS ---
  (setq placementList (calculate-placement-positions inputAttrs inputTypes))

  ; --- STEP 6: SAVE ENVIRONMENT AND SETUP ---
  (setq oldSnap (getvar "OSMODE"))
  (setq oldCmd (getvar "CMDECHO"))
  (setq oldColor (getvar "CECOLOR"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ; --- STEP 7: PLACE SWITCH BLOCKS ---
  (princ "\nPlacing switch blocks...")
  (setq i 0)
  (foreach placement placementList
    (setq currentType (cadr placement))
    (setq currentPos (caddr placement))

    (cond
      ((= currentType "M")
        (setq switchBlock (insert-switch-block BLOCK_MOMENTARY currentPos))
        (if switchBlock
          ; Store block entity in placement list for wiring phase
          (setq placementList
                (subst (append placement (list switchBlock))
                       placement
                       placementList))
        )
      )
      ((= currentType "L")
        (setq switchBlock (insert-switch-block BLOCK_LATCHING currentPos))
        (if switchBlock
          (setq placementList
                (subst (append placement (list switchBlock))
                       placement
                       placementList))
        )
      )
      ; S and F types: no blocks to place
    )
    (setq i (1+ i))
  )

  ; --- STEP 8: WIRE SWITCHES WITH 2-SEGMENT PATH (INCREMENTAL DROP) AND PLACE FANOUTS ---
  (princ "\nWiring switches...")
  (setq i 0)
  (foreach placement placementList
    (setq currentType (cadr placement))
    (setq currentPos (caddr placement))  ; Switch position
    (setq inxPos (cadddr placement))     ; Control device INx position

    (cond
      ((or (= currentType "M") (= currentType "L"))
        ; Get switch block entity (5th element if added in step 7)
        (if (> (length placement) 4)
          (progn
            (setq switchBlock (nth 4 placement))

            ; Determine which column and which terminal to use
            (setq columnIndex (/ i MAX_INPUTS_PER_COLUMN))
            (setq rowIndex (rem i MAX_INPUTS_PER_COLUMN))

            ; Column 0 (inputs 0-5): Connect to OUT (left side), wire goes LEFT
            ; Column 1 (inputs 6-11): Connect to GND (right side), wire goes RIGHT
            (if (= columnIndex 0)
              (setq terminalAttr "OUT")
              (setq terminalAttr "GND")
            )

            (setq termPos (find-switch-connection-terminal switchBlock terminalAttr))
            (if termPos
              (progn
                ; *** 3-SEGMENT WIRING PATH: VERTICAL → 45° ANGLE → HORIZONTAL ***
                ; Segment 1: Vertical drop from IN point (incremental per row)
                (setvar "CECOLOR" (itoa WIRE_COLOR))
                (setq ptStart (list (car inxPos) (- (cadr inxPos) WIRE_DROP_START) 0.0))
                (setq dropDistance (+ WIRE_DROP_END (* rowIndex SWITCH_SPACING_VERTICAL)))
                (setq ptVertEnd (list (car inxPos) (- (cadr inxPos) dropDistance) 0.0))
                (command "LINE" ptStart ptVertEnd "")

                ; Segment 2: 45-degree angle going down to terminal Y level
                ; Calculate 45° endpoint: go down to terminal Y, and left/right by same amount
                (setq verticalRemaining (- (cadr ptVertEnd) (cadr termPos)))  ; Distance down to terminal Y
                (if (= columnIndex 0)
                  ; Column 0: 45° down-left (225 degrees)
                  (setq pt45End (list (- (car ptVertEnd) verticalRemaining) (cadr termPos) 0.0))
                  ; Column 1: 45° down-right (315 degrees)
                  (setq pt45End (list (+ (car ptVertEnd) verticalRemaining) (cadr termPos) 0.0))
                )
                (command "LINE" ptVertEnd pt45End "")

                ; Segment 3: Horizontal to switch terminal
                (smart-wire pt45End termPos BRIDGE_RADIUS WIRE_COLOR)

                ; Create label (horizontal text for switches, 5mm offset)
                (create-input-label (+ i 1) (list (+ (car currentPos) LABEL_OFFSET_X_SWITCH) (cadr currentPos) 0.0) 0)
              )
              (princ (strcat "\nWarning: " terminalAttr " attribute not found on switch at index " (itoa i)))
            )
          )
        )
      )
      ((= currentType "F")
        ; Draw vertical drop to fanout (no junction dot)
        (setvar "CECOLOR" (itoa WIRE_COLOR))
        (setq ptStart (list (car inxPos) (- (cadr inxPos) WIRE_DROP_START) 0.0))
        (setq ptEnd (list (car inxPos) (- (cadr inxPos) WIRE_DROP_END) 0.0))
        (command "LINE" ptStart ptEnd "")

        ; Create label at fanout position (vertical text for fanouts, 3mm offset)
        (create-input-label (+ i 1) (list (+ (car inxPos) LABEL_OFFSET_X_FANOUT) (- (cadr inxPos) WIRE_DROP_END) 0.0) 90)
      )
      ; Type "S" - no action (completely skipped)
    )
    (setq i (1+ i))
  )

  ; --- STEP 9: GREEN GROUND BUS (COLUMN 0 ONLY) ---
  (princ "\nDrawing ground bus...")
  (setq gndTermList '())
  (setq gndBusX nil)

  ; Collect GND terminals from column 0 switches (first 6)
  (setq i 0)
  (foreach placement placementList
    (if (< i MAX_INPUTS_PER_COLUMN)  ; Only column 0
      (progn
        (setq currentType (cadr placement))
        ; Only M and L types have switches
        (if (or (= currentType "M") (= currentType "L"))
          (if (> (length placement) 4)
            (progn
              (setq switchBlock (nth 4 placement))
              (setq gndPos (find-switch-connection-terminal switchBlock "GND"))
              (if gndPos
                (setq gndTermList (append gndTermList (list gndPos)))
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ; Only draw ground bus if we have GND terminals
  (if (> (length gndTermList) 0)
    (progn
      ; Calculate vertical bus X position (20mm left of GND terminals)
      (setq gndBusX (- (car (car gndTermList)) GND_BUS_HORIZONTAL_LENGTH))

      ; Draw horizontal extensions from each GND terminal
      (setvar "CECOLOR" (itoa GND_BUS_COLOR))
      (foreach gndPos gndTermList
        (command "LINE"
                 gndPos
                 (list gndBusX (cadr gndPos) 0.0)
                 "")
      )

      ; Draw vertical bus connecting all horizontals
      (setq minGndY (cadr (car gndTermList)))
      (setq maxGndY (cadr (car gndTermList)))
      (foreach gndPos gndTermList
        (if (< (cadr gndPos) minGndY) (setq minGndY (cadr gndPos)))
        (if (> (cadr gndPos) maxGndY) (setq maxGndY (cadr gndPos)))
      )
      (command "LINE"
               (list gndBusX minGndY 0.0)
               (list gndBusX maxGndY 0.0)
               "")

      ; Add junction dots at each connection point
      (foreach gndPos gndTermList
        (draw-junction-dot (list gndBusX (cadr gndPos) 0.0) GND_BUS_DOT_RADIUS)
      )
    )
  )

  ; --- STEP 10: CLEANUP AND RESTORE ---
  (setvar "OSMODE" oldSnap)
  (setvar "CMDECHO" oldCmd)
  (setvar "CECOLOR" oldColor)

  (princ "\nAuto Input Device Placement Complete.")
  (princ)
)

(princ "\nAuto Input Device loaded. Type AUTOINPUT to run.")
(princ)

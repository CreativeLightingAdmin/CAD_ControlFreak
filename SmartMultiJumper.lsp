; ============================================================================
; SMART MULTI-JUMPER (Using SmartJumper Library)
; Draws multiple parallel lines with automatic bridge arcs
; ============================================================================

(defun c:smartmline ( / *error* startPt endPt numConns spacing brRad oldSnap
                        ang perpAng baseOffset i currentOffset p1_off p2_off)

  ; --- CHECK DEPENDENCY ---
  (if (not smart-wire)
    (progn
      (alert "Error: SmartJumper_Lib.lsp is not loaded!")
      (exit)
    )
  )

  ; Error Handler to clean up if user presses Esc
  (defun *error* (msg)
    (if oldSnap (setvar "OSMODE" oldSnap))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ; 1. GET SETTINGS
  ; ---------------------------------------------------------
  (initget 6) ; No zero/negatives
  (setq numConns (getint "\nNumber of connections <2>: "))
  (if (not numConns) (setq numConns 2))

  (initget 6)
  (setq spacing (getreal "\nOffset spacing (mm) <5.0>: "))
  (if (not spacing) (setq spacing 5.0))

  (initget 6)
  (setq brRad (getreal "\nBridge Radius <2.0>: "))
  (if (not brRad) (setq brRad 2.0))

  ; 2. GET COORDINATES
  ; ---------------------------------------------------------
  (setq startPt (getpoint "\nClick Start Point: "))
  (if (not startPt) (exit))

  (setq endPt (getpoint startPt "\nClick End Point: "))
  (if (not endPt) (exit))

  ; Picked points are in the current UCS - convert to WCS and flatten to Z=0
  (setq startPt (trans startPt 1 0))
  (setq startPt (list (car startPt) (cadr startPt) 0.0))
  (setq endPt (trans endPt 1 0))
  (setq endPt (list (car endPt) (cadr endPt) 0.0))

  ; 3. CALCULATION SETUP
  ; ---------------------------------------------------------
  (setq ang (angle startPt endPt))
  (setq perpAng (+ ang (/ pi 2.0)))

  ; Calculate base offset to center the group of lines
  ; Formula: -( (NumLines - 1) * Spacing ) / 2
  (setq baseOffset (* -1.0 (/ (* (- numConns 1) spacing) 2.0)))

  ; Save Snaps and turn OFF
  (setq oldSnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (princ "\nProcessing parallel lines...")

  ; 4. LOOP THROUGH EACH CONNECTION
  ; ---------------------------------------------------------
  (setq i 0)
  (repeat numConns
    (setq currentOffset (+ baseOffset (* i spacing)))

    ; Calculate offset start/end points
    (setq p1_off (polar startPt perpAng currentOffset))
    (setq p2_off (polar endPt perpAng currentOffset))

    ; USE THE LIBRARY FUNCTION!
    ; smart-wire handles intersection detection and bridge drawing
    (smart-wire p1_off p2_off brRad 7)  ; Color 7 = White

    (setq i (1+ i))
  )

  ; Restore Snaps
  (setvar "OSMODE" oldSnap)
  (princ "\nSmart MLINE Complete.")
  (princ)
)

(princ "\nSmart Multi-Jumper loaded. Type SMARTMLINE to run.")
(princ)

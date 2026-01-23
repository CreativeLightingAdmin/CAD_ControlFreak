(defun c:AUTOLAYOUT ( / ss i ent entData insPt rotAng scaleX atts layoutName safeLayoutName psBlockEnt vpObj vpCenter zoomHeight rotDeg allBlocks j blockEnt blockName msBlockName psBlockName vPortLayer defaultVP frameObj minPt maxPt bbCenter bbWidth bbHeight doc layouts currentLayout)
  (vl-load-com) 

  ;; 1. SETTINGS -----------------------------------------------------------
  (setq msBlockName "FRAME_BLOCK")      ; Your Model Space Frame
  (setq psBlockName "TITLE_BLOCK_A3")   ; Your Paper Space Title Block
  (setq vPortLayer  "Defpoints")        ; Layer for Viewport
  
  ;; 2. SELECTION ----------------------------------------------------------
  (prompt "\nLooking for Frame Blocks...")
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 msBlockName))))

  ;; Debug: Show all block names in drawing
  (if (not ss)
    (progn
      (prompt (strcat "\nBlock '" msBlockName "' not found."))
      (prompt "\nAll block references in drawing:")
      (setq allBlocks (ssget "X" '((0 . "INSERT"))))
      (if allBlocks
        (progn
          (setq j 0)
          (repeat (sslength allBlocks)
            (setq blockEnt (ssname allBlocks j))
            (setq blockName (cdr (assoc 2 (entget blockEnt))))
            (prompt (strcat "\n  - " blockName))
            (setq j (1+ j))
          )
        )
        (prompt "\n  No blocks found in drawing at all.")
      )
    )
  )

  (if ss
    (progn
      (setq i 0)
      (prompt (strcat "\nFound " (itoa (sslength ss)) " frames. Processing..."))

      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        
        ;; Extract Geometry
        (setq insPt (cdr (assoc 10 entData)))    ; Center Point (Model)
        (setq rotAng (cdr (assoc 50 entData)))   ; Rotation
        (setq scaleX (cdr (assoc 41 entData)))   ; Block Scale (e.g., 1.0 or 50.0)

        ;; Get bounding box of the frame block
        (setq frameObj (vlax-ename->vla-object ent))
        (vla-GetBoundingBox frameObj 'minPt 'maxPt)
        (setq minPt (vlax-safearray->list minPt))
        (setq maxPt (vlax-safearray->list maxPt))

        ;; Calculate center from bounding box
        (setq bbCenter (list (/ (+ (car minPt) (car maxPt)) 2.0)
                             (/ (+ (cadr minPt) (cadr maxPt)) 2.0)
                             0.0))

        ;; Calculate width and height
        (setq bbWidth (- (car maxPt) (car minPt)))
        (setq bbHeight (- (cadr maxPt) (cadr minPt)))

        ;; Debug: Show block geometry
        (prompt (strcat "\nFrame bounding box: (" (rtos (car minPt) 2 2) "," (rtos (cadr minPt) 2 2)
                       ") to (" (rtos (car maxPt) 2 2) "," (rtos (cadr maxPt) 2 2) ")"))
        (prompt (strcat "\nFrame center: " (rtos (car bbCenter) 2 2) ", " (rtos (cadr bbCenter) 2 2)))
        (prompt (strcat "\nFrame size: " (rtos bbWidth 2 2) " x " (rtos bbHeight 2 2)))
        (prompt (strcat "\nFrame scale: " (rtos scaleX 2 2)))
        (prompt (strcat "\nFrame rotation: " (rtos (* rotAng (/ 180.0 pi)) 2 2) " degrees"))
        
        ;; Extract Attributes
        (setq atts (GetAttributes ent))

        ;; Debug: Show what attributes were found
        (prompt "\nAttributes found: ")
        (foreach att atts
          (prompt (strcat "\n  " (car att) " = " (cdr att)))
        )

        ;; Define Layout Name
        (setq layoutName (cdr (assoc "DRAWING_NUMBER" atts)))
        (prompt (strcat "\nLayout name from DRAWING_NUMBER: " (if layoutName layoutName "nil")))
        (if (or (= layoutName nil) (= layoutName ""))
            (setq layoutName (strcat "Layout_" (itoa i)))
        )

        ;; Sanitize layout name (remove invalid characters)
        (setq layoutName (SanitizeLayoutName layoutName))
        (prompt (strcat "\nSanitized layout name: " layoutName))

        ;; Unique Name Check
        (setq safeLayoutName layoutName)
        (while (member safeLayoutName (layoutlist))
           (setq safeLayoutName (strcat safeLayoutName "_"))
        )
        (prompt (strcat "\nFinal unique layout name: " safeLayoutName))
        
        ;; 3. GENERATE LAYOUT ------------------------------------------------
        (command "._LAYOUT" "New" safeLayoutName)
        (command "._CTAB" safeLayoutName)

        ;; Configure Page Setup for A3 (420mm x 297mm)
        (setq doc (vla-get-activedocument (vlax-get-acad-object)))
        (setq layouts (vla-get-layouts doc))
        (setq currentLayout (vla-item layouts safeLayoutName))

        ;; Set paper size to A3 (420 x 297 mm)
        (vla-put-CanonicalMediaName currentLayout "ISO_A3_(297.00_x_420.00_MM)")
        (vla-put-StandardScale currentLayout acScaleToFit)

        ;; Set plot area to Layout
        (vla-put-PlotType currentLayout acLayout)

        ;; Center plot on paper
        (vla-put-PlotCentered currentLayout :vlax-true)

        ;; Clean Layout - Delete default viewport only
        (command "._MSPACE")
        (command "._PSPACE")
        (setq defaultVP (ssget "X" '((0 . "VIEWPORT"))))
        (if defaultVP
          (command "._ERASE" defaultVP "")
        )

        ;; 4. CREATE VIEWPORT ------------------------------------------------
        (command "._-LAYER" "Set" vPortLayer "")
        ;; Create viewport matching A3 Extents
        (command "._MVIEW" "0,0" "420,297") 
        
        ;; --- STEP A: ROTATION (Must be done before VLA locking) ---
        (command "._MSPACE") 
        (setq rotDeg (* rotAng (/ 180.0 pi)))
        (if (not (zerop rotDeg))
            (command "._DVIEW" "all" "" "TWist" (- rotDeg) "") 
        )
        
        ;; --- STEP B: SET SCALE FIRST ---
        (command "._PSPACE")
        (setq vpObj (vlax-ename->vla-object (entlast)))

        ;; Force Scale: 1 / BlockScale (must be done before centering)
        (vla-put-CustomScale vpObj (/ 1.0 scaleX))

        ;; --- STEP C: CENTER THE VIEW ON BOUNDING BOX ---
        (command "._MSPACE")
        ;; Use ZOOM Window with the bounding box coordinates
        (command "._ZOOM" "Window" minPt maxPt)
        (command "._PSPACE")

        ;; Lock Viewport
        (vla-put-DisplayLocked vpObj :vlax-true)

        ;; --- STEP D: INSERT TITLE BLOCK ---
        ;; Insert Title Block AFTER viewport is set up
        (if (tblsearch "BLOCK" psBlockName)
          (progn
             (command "._INSERT" psBlockName "0,0" "1" "1" "0")
             (setq psBlockEnt (entlast))
             (SyncAttributes psBlockEnt atts)
          )
          (prompt (strcat "\nWarning: Title block '" psBlockName "' not found"))
        )
        
        (setq i (1+ i))
      )
      (prompt "\nProcessing Complete.")
    )
    (prompt "\nNo FRAME_BLOCK instances found.")
  )
  (princ)
)

;;; HELPER: Get Attributes
(defun GetAttributes ( ent / attEnt attData attTag attVal attList )
  (setq attList '())
  (if (= (cdr (assoc 66 (entget ent))) 1) 
    (progn
      (setq attEnt (entnext ent))
      (while (/= (cdr (assoc 0 (entget attEnt))) "SEQEND")
        (setq attData (entget attEnt))
        (setq attTag (strcase (cdr (assoc 2 attData)))) 
        (setq attVal (cdr (assoc 1 attData)))
        (setq attList (cons (cons attTag attVal) attList))
        (setq attEnt (entnext attEnt))
      )
    )
  )
  attList
)

;;; HELPER: Sync Attributes
(defun SyncAttributes ( psEnt sourceAtts / attEnt attTag matchVal )
  (if (= (cdr (assoc 66 (entget psEnt))) 1)
    (progn
      (setq attEnt (entnext psEnt))
      (while (/= (cdr (assoc 0 (entget attEnt))) "SEQEND")
        (setq attTag (strcase (cdr (assoc 2 (entget attEnt)))))
        (setq matchVal (cdr (assoc attTag sourceAtts)))
        (if matchVal
          (progn
            (entmod (subst (cons 1 matchVal) (assoc 1 (entget attEnt)) (entget attEnt)))
            (entupd attEnt)
          )
        )
        (setq attEnt (entnext attEnt))
      )
    )
  )
)

;;; HELPER: Sanitize Layout Name
;;; Removes invalid characters from layout names
;;; Invalid: < > / \ " : ; ? * | , = `
(defun SanitizeLayoutName ( name / cleanName i char validChars )
  (setq cleanName "")
  (setq i 1)

  ;; Loop through each character
  (while (<= i (strlen name))
    (setq char (substr name i 1))

    ;; Check if character is valid (not in the invalid list)
    (if (not (wcmatch char "<>*/\\\":;?|,=`"))
      (setq cleanName (strcat cleanName char))
      (setq cleanName (strcat cleanName "_"))  ; Replace invalid chars with underscore
    )
    (setq i (1+ i))
  )

  ;; Ensure name isn't empty and isn't "Model" (reserved)
  (if (or (= cleanName "") (= (strcase cleanName) "MODEL"))
    (setq cleanName "Layout_1")
  )

  ;; Truncate to 255 characters (AutoCAD limit)
  (if (> (strlen cleanName) 255)
    (setq cleanName (substr cleanName 1 255))
  )

  cleanName
)
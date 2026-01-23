; ============================================================================
; SMART JUMPER LIBRARY (Engine V1.1 - ProgeCAD Fixed)
; Fixes: 'Not Variant: Nil' error when no intersection is found
; ============================================================================

(vl-load-com)

; --- PUBLIC FUNCTION --------------------------------------------------------
(defun smart-wire (p1 p2 rad col / doc mSpace tempLine intPoints sortedInts oldSnap)
  
  ; 1. Setup
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mSpace (vla-get-modelspace doc))
  
  ; Force Z=0 for reliability
  (setq p1 (list (car p1) (cadr p1) 0.0))
  (setq p2 (list (car p2) (cadr p2) 0.0))

  ; 2. Find Intersections (Using VLA)
  ; Create invisible temp line
  (setq tempLine (vla-addline mSpace (vlax-3d-point p1) (vlax-3d-point p2)))
  
  ; Scan for crossings
  (setq intPoints (get-flat-intersections tempLine mSpace))
  (vla-delete tempLine) ; Cleanup immediately

  ; 3. Draw
  (setq oldSnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setvar "CECOLOR" (itoa col)) ; Set the requested color

  (if (= (length intPoints) 0)
    ; Case A: No obstacles, simple line
    (command "LINE" p1 p2 "")
    
    ; Case B: Obstacles found, draw bridges
    (progn
      (setq sortedInts (sort-points-by-dist p1 intPoints))
      (draw-bridges-logic p1 p2 sortedInts rad)
    )
  )

  ; Restore
  (setvar "OSMODE" oldSnap)
  (setvar "CECOLOR" "BYLAYER")
)

; --- INTERNAL HELPERS (Do not call directly) --------------------------------

(defun get-flat-intersections (tempLineObj mSpace / ss i obj copyObj intVar intArr ptList)
  (setq ptList '())
  ; Select touching objects (Ignore Text/Hatch)
  (setq ss (ssget "C" 
             (vlax-safearray->list (vlax-variant-value (vla-get-startpoint tempLineObj)))
             (vlax-safearray->list (vlax-variant-value (vla-get-endpoint tempLineObj)))
             '((-4 . "<NOT") (0 . "TEXT,MTEXT,DIMENSION,HATCH") (-4 . "NOT>"))
           ))
  (if ss
    (repeat (setq i (sslength ss))
      (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
      (if (not (equal obj tempLineObj))
        (progn
          (setq copyObj (vla-copy obj))     ; Copy
          (flatten-object copyObj)          ; Squash to 2D
          
          ; --- CRITICAL FIX START ---
          ; Check intersection with error handling
          (setq intVar (vl-catch-all-apply 'vla-intersectwith (list tempLineObj copyObj 0)))

          ; Only process if call succeeded and returned valid variant
          (if (and (not (vl-catch-all-error-p intVar))
                   intVar
                   (= (type intVar) 'VARIANT))
            (progn
              ; Safely extract the variant value
              (setq intArr (vl-catch-all-apply 'vlax-variant-value (list intVar)))

              ; Check if extraction succeeded and we got a valid array
              (if (and (not (vl-catch-all-error-p intArr))
                       intArr
                       (= (type intArr) 'SAFEARRAY)
                       (> (vlax-safearray-get-u-bound intArr 1) -1))
                (setq ptList (append ptList (group-by-3 (vlax-safearray->list intArr))))
              )
            )
          )
          ; --- CRITICAL FIX END ---
          
          (vla-delete copyObj)              ; Delete copy
        )
      )
    )
  )
  ptList
)

(defun flatten-object (obj / start end)
  (cond
    ((= (vla-get-objectname obj) "AcDbLine")
      (setq start (vlax-safearray->list (vlax-variant-value (vla-get-startpoint obj))))
      (setq end (vlax-safearray->list (vlax-variant-value (vla-get-endpoint obj))))
      (vla-put-startpoint obj (vlax-3d-point (list (car start) (cadr start) 0.0)))
      (vla-put-endpoint obj (vlax-3d-point (list (car end) (cadr end) 0.0)))
    )
    ((wcmatch (vla-get-objectname obj) "*Polyline") (vla-put-elevation obj 0.0))
    ((wcmatch (vla-get-objectname obj) "AcDbCircle,AcDbArc")
      (setq start (vlax-safearray->list (vlax-variant-value (vla-get-center obj))))
      (vla-put-center obj (vlax-3d-point (list (car start) (cadr start) 0.0)))
    )
  )
)

(defun draw-bridges-logic (startPt endPt pts rad / currPt ang p1 p2 pMid)
  (setq currPt startPt)
  (setq ang (angle startPt endPt))
  (foreach pt pts
    (if (> (distance currPt pt) 0.1) ; Skip duplicates
      (progn
        (setq p1 (polar pt (+ ang pi) rad))      
        (setq p2 (polar pt ang rad))             
        (setq pMid (polar pt (+ ang (/ pi 2)) rad)) 
        (command "LINE" currPt p1 "")
        (command "ARC" p1 pMid p2)
        (setq currPt p2)
      )
    )
  )
  (command "LINE" currPt endPt "")
)

(defun group-by-3 (lst / res)
  (while lst
    (setq res (cons (list (car lst) (cadr lst) (caddr lst)) res))
    (setq lst (cdddr lst))
  )
  res
)

(defun sort-points-by-dist (p1 ptList)
  (vl-sort ptList
    '(lambda (a b) (< (distance p1 a) (distance p1 b)))
  )
)

(princ "\nSmartJumper Library V1.2 Loaded.")
(princ)
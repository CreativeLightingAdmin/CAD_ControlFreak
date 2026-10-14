; ============================================================================
; AUTO LAYOUT - One paper space layout per frame
; Command: AUTOLAYOUT
;
; For every FRAME_BLOCK in model space (dynamic copies included):
;   - creates a landscape layout (A3 by default, or A2/A1/A0) named from the
;     frame's DRAWING_NUMBER, else TITLE, else "Sheet n"
;   - adds one locked viewport filling the sheet, centred on the frame, at the
;     scale that fits the frame exactly to the paper
;   - print settings: copied from a page setup named CF_A3 (CF_A2...) if the
;     drawing has one, otherwise paper size / landscape / fit are set directly
; Frames that already have a layout can be skipped or replaced.
; Then PUBLISH (or batch plot) gives one PDF page per frame.
; ============================================================================

(vl-load-com)

; --- CONFIGURATION ----------------------------------------------------------
(setq al:*frame-block*  "FRAME_BLOCK")     ; Model space frame block
; Frame outline at scale 1 relative to its base point ((minX minY) (maxX maxY))
(setq al:*frame-outline* '((-210.0 -148.5) (210.0 148.5)))   ; A3, base point at centre
(setq al:*vp-layer*     "Defpoints")       ; Viewport layer (doesn't plot)
(setq al:*setup-prefix* "CF_")             ; Page setup to copy: CF_A3, CF_A2...

; Paper sizes, landscape (width height) in mm, and their PC3 media names
(setq al:*papers*
  '(("A3" 420.0  297.0 "ISO_A3_(297.00_x_420.00_MM)")
    ("A2" 594.0  420.0 "ISO_A2_(420.00_x_594.00_MM)")
    ("A1" 841.0  594.0 "ISO_A1_(594.00_x_841.00_MM)")
    ("A0" 1189.0 841.0 "ISO_A0_(841.00_x_1189.00_MM)")))

; --- HELPERS ----------------------------------------------------------------

(defun al:effective-name (ent / res)
  (setq res (vl-catch-all-apply 'vla-get-effectivename (list (vlax-ename->vla-object ent))))
  (if (vl-catch-all-error-p res) (cdr (assoc 2 (entget ent))) res)
)

; Frames in model space, sorted left to right
(defun al:find-frames ( / ss i ent res)
  (if (setq ss (ssget "X" '((0 . "INSERT") (410 . "Model"))))
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      (if (= (strcase (al:effective-name ent)) (strcase al:*frame-block*))
        (setq res (cons ent res))
      )
    )
  )
  (vl-sort res '(lambda (a b) (< (car (cdr (assoc 10 (entget a))))
                                 (car (cdr (assoc 10 (entget b)))))))
)

; Frame outline in the drawing from its insertion point and scale
; (unrotated frames): ((minX minY) (maxX maxY))
(defun al:frame-box (ent / d ins sx sy)
  (setq d (entget ent))
  (setq ins (cdr (assoc 10 d)))
  (setq sx (abs (cond ((cdr (assoc 41 d))) (1.0))))
  (setq sy (abs (cond ((cdr (assoc 42 d))) (1.0))))
  (list (list (+ (car ins) (* sx (caar al:*frame-outline*)))
              (+ (cadr ins) (* sy (cadar al:*frame-outline*))))
        (list (+ (car ins) (* sx (caadr al:*frame-outline*)))
              (+ (cadr ins) (* sy (cadadr al:*frame-outline*)))))
)

(defun al:attributes (ent / e d res)
  (setq e (entnext ent))
  (while (and e (= "ATTRIB" (cdr (assoc 0 (setq d (entget e))))))
    (setq res (cons (cons (strcase (cdr (assoc 2 d))) (cdr (assoc 1 d))) res))
    (setq e (entnext e))
  )
  res
)

(defun al:att (atts tag / v)
  (setq v (cdr (assoc tag atts)))
  (if (and v (/= (vl-string-trim " " v) "")) (vl-string-trim " " v))
)

; Removes characters layouts can't have
(defun al:sanitize (name / clean i ch)
  (setq clean "" i 1)
  (while (<= i (strlen name))
    (setq ch (substr name i 1))
    (setq clean (strcat clean (if (wcmatch ch "<,>,/,\\,\",:,;,`?,`*,|,`,,=,``") "_" ch)))
    (setq i (1+ i))
  )
  (if (or (= clean "") (= (strcase clean) "MODEL")) (setq clean "Sheet"))
  (if (> (strlen clean) 255) (substr clean 1 255) clean)
)

(defun al:layout-item (doc name / res)
  (setq res (vl-catch-all-apply 'vla-item (list (vla-get-layouts doc) name)))
  (if (not (vl-catch-all-error-p res)) res)
)

(defun al:ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) '(62 . 7) '(6 . "Continuous")))
  )
)

; List from a layout method that returns names (GetPlotDeviceNames etc.)
(defun al:names (lay method / res)
  (setq res (vl-catch-all-apply 'vlax-invoke (list lay method)))
  (cond
    ((vl-catch-all-error-p res) nil)
    ((= (type res) 'VARIANT) (vlax-safearray->list (vlax-variant-value res)))
    ((= (type res) 'SAFEARRAY) (vlax-safearray->list res))
    ((listp res) res)
  )
)

; Best media name for paper (e.g. A3) from the current device, or nil
(defun al:pick-media (lay paper / medias key)
  (setq medias (al:names lay 'GetCanonicalMediaNames))
  (setq key (car paper))
  (cond
    ((member (nth 3 paper) medias) (nth 3 paper))
    ((vl-some '(lambda (m) (if (and (wcmatch (strcase m) (strcat "*ISO*" key "*"))
                                    (not (wcmatch (strcase m) "*EXTRA*,*OVERSIZE*,*EXPAND*")))
                             m))
              medias))
    ((vl-some '(lambda (m) (if (and (wcmatch (strcase m) (strcat "*" key "*"))
                                    (not (wcmatch (strcase m) "*EXTRA*,*OVERSIZE*,*EXPAND*,*+*")))
                             m))
              medias))
  )
)

; Print settings. Uses page setup CF_A3 (etc.) if the drawing has one;
; otherwise picks a PDF printer that offers the paper size and sets
; landscape / fit to paper. Returns a note describing what happened.
(defun al:setup-plot (doc lay paper / setup media devices dev pw ph note)
  (setq setup (vl-catch-all-apply 'vla-item
                (list (vla-get-plotconfigurations doc) (strcat al:*setup-prefix* (car paper)))))
  (if (and (not (vl-catch-all-error-p setup))
           (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-copyfrom (list lay setup)))))
    (strcat "page setup " al:*setup-prefix* (car paper))
    (progn
      (vl-catch-all-apply 'vla-refreshplotdeviceinfo (list lay))
      ; Current printer first; if it has no such paper, try PDF printers, then any
      (if (not (setq media (al:pick-media lay paper)))
        (progn
          (setq devices (al:names lay 'GetPlotDeviceNames))
          (setq devices (append (vl-remove-if-not '(lambda (d) (wcmatch (strcase d) "*PDF*")) devices)
                                (vl-remove-if '(lambda (d) (wcmatch (strcase d) "*PDF*,NONE")) devices)))
          (while (and devices (not media))
            (setq dev (car devices)  devices (cdr devices))
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-configname (list lay dev))))
              (progn
                (vl-catch-all-apply 'vla-refreshplotdeviceinfo (list lay))
                (setq media (al:pick-media lay paper))
              )
            )
          )
        )
      )
      (if media
        (progn
          (vl-catch-all-apply 'vla-put-canonicalmedianame (list lay media))
          (vl-catch-all-apply 'vla-put-paperunits (list lay 1))         ; acMillimeters
          ; Landscape: rotate if the media is defined portrait
          (vl-catch-all-apply 'vla-getpapersize (list lay 'pw 'ph))
          (vl-catch-all-apply 'vla-put-plotrotation
            (list lay (if (and pw ph (< pw ph)) 1 0)))                  ; ac90degrees / ac0degrees
          (vl-catch-all-apply 'vla-put-plottype (list lay 5))           ; acLayout
          (vl-catch-all-apply 'vla-put-standardscale (list lay 0))      ; acScaleToFit
          (vl-catch-all-apply 'vla-put-centerplot (list lay :vlax-true))
          (strcat (vla-get-configname lay) ", " media ", landscape, fit to paper")
        )
        (strcat "NO PRINTER WITH " (car paper) " PAPER FOUND - create a page setup named "
                al:*setup-prefix* (car paper) " and run again with Replace")
      )
    )
  )
)

; Sheet size of the layout as (width height), landscape, in mm - or nil
(defun al:sheet-size (lay / pw ph res)
  (setq res (vl-catch-all-apply 'vla-getpapersize (list lay 'pw 'ph)))
  (if (and (not (vl-catch-all-error-p res)) pw ph (> pw 0) (> ph 0))
    (list (max pw ph) (min pw ph))
  )
)

(defun al:join (strs sep / res)
  (foreach s strs (setq res (if res (strcat res sep s) s)))
  (if res res "")
)

; Points the viewport at the frame: view centre = frame centre, view height
; so the frame fills the paper. Tries the viewport's data first, then ZOOM.
(defun al:aim-viewport (vp centre viewH / ent d id)
  (setq ent (vlax-vla-object->ename vp))
  (setq d (entget ent))
  (setq d (subst (list 12 (car centre) (cadr centre)) (assoc 12 d) d))
  (setq d (subst (cons 45 viewH) (assoc 45 d) d))
  (entmod d)
  (entupd ent)

  ; Check it took; otherwise zoom inside the viewport
  (setq d (entget ent))
  (if (or (> (abs (- (cdr (assoc 45 d)) viewH)) (* 0.001 viewH))
          (> (distance (cdr (assoc 12 d)) (list (car centre) (cadr centre))) (* 0.001 viewH)))
    (progn
      (setq id (cdr (assoc 69 d)))
      (vl-catch-all-apply 'vla-put-mspace (list (vla-get-activedocument (vlax-get-acad-object)) :vlax-true))
      (setvar "CVPORT" id)
      (command "._ZOOM" "_C" (list (car centre) (cadr centre) 0.0) viewH)
      (vl-catch-all-apply 'vla-put-mspace (list (vla-get-activedocument (vlax-get-acad-object)) :vlax-false))
    )
  )
)

; --- MAIN COMMAND -----------------------------------------------------------

(defun c:autolayout ( / *error* doc frames paperKey paper mode oldEcho n made skipped
                        frame atts name lay ss i vpEnt box fw fh centre vp viewH note rot
                        sheet sw sh used k)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (vl-catch-all-apply 'vla-endundomark (list doc))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nAUTOLAYOUT error: " msg)))
    (princ)
  )

  (setq frames (al:find-frames))
  (if (not frames)
    (progn (princ (strcat "\nNo " al:*frame-block* " found in model space.")) (exit))
  )
  (princ (strcat "\nFound " (itoa (length frames)) " frame(s)."))

  ; Paper size
  (initget "A3 A2 A1 A0")
  (setq paperKey (getkword "\nPaper size [A3/A2/A1/A0] <A3>: "))
  (if (not paperKey) (setq paperKey "A3"))
  (setq paper (assoc paperKey al:*papers*))

  ; What to do when a frame already has a layout
  (initget "Skip Replace")
  (setq mode (getkword "\nFrames that already have a layout [Skip/Replace] <Skip>: "))
  (if (not mode) (setq mode "Skip"))

  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (vl-catch-all-apply 'vla-startundomark (list doc))
  (al:ensure-layer al:*vp-layer*)

  (setq n 0 made 0 skipped 0)
  (foreach frame frames
    (setq n (1+ n))
    (setq atts (al:attributes frame))
    (setq name (al:sanitize (cond ((al:att atts "DRAWING_NUMBER"))
                                  ((al:att atts "TITLE"))
                                  (T (strcat "Sheet " (itoa n))))))
    ; Two frames with the same name in this run: "Test", "Test (2)"...
    (if (member (strcase name) used)
      (progn
        (setq k 2)
        (while (member (strcase (strcat name " (" (itoa k) ")")) used) (setq k (1+ k)))
        (princ (strcat "\n  Note: another frame is also named '" name "' - using '"
                       name " (" (itoa k) ")'. Edit DRAWING_NUMBER to give each frame its own name."))
        (setq name (strcat name " (" (itoa k) ")"))
      )
    )
    (setq used (cons (strcase name) used))

    (cond
      ((and (setq lay (al:layout-item doc name)) (= mode "Skip"))
        (princ (strcat "\n  " name ": layout exists - skipped"))
        (setq skipped (1+ skipped)))
      (T
        ; Replace: delete the old layout first
        (if lay
          (progn
            (vla-put-activespace doc 1)   ; acModelSpace, can't delete the active layout
            (vl-catch-all-apply 'vla-delete (list lay))
          )
        )
        (setq lay (vla-add (vla-get-layouts doc) name))
        (vla-put-activelayout doc lay)
        (setq note (al:setup-plot doc lay paper))

        ; Remove any viewports the new layout came with (not the sheet itself, ID 1)
        (if (setq ss (ssget "X" (list '(0 . "VIEWPORT") (cons 410 name))))
          (repeat (setq i (sslength ss))
            (setq vpEnt (ssname ss (setq i (1- i))))
            (if (/= 1 (cdr (assoc 69 (entget vpEnt))))
              (vl-catch-all-apply 'vla-delete (list (vlax-ename->vla-object vpEnt))))
          )
        )

        ; Frame size and centre in the model
        (setq rot (cdr (assoc 50 (entget frame))))
        ; From the frame's known outline and its scale - CAD extents of the
        ; block can include attribute text and be bigger than the frame
        (setq box (al:frame-box frame))
        (setq fw (- (caadr box) (caar box))
              fh (- (cadadr box) (cadar box)))
        (setq centre (list (/ (+ (caar box) (caadr box)) 2.0)
                           (/ (+ (cadar box) (cadadr box)) 2.0)))

        ; Sheet as actually set up (falls back to the nominal paper size)
        (setq sheet (cond ((al:sheet-size lay)) ((list (cadr paper) (caddr paper)))))
        (setq sw (car sheet)  sh (cadr sheet))

        ; One viewport filling the sheet, showing the whole frame
        (setq vp (vla-addpviewport (vla-get-paperspace doc)
                   (vlax-3d-point (list (/ sw 2.0) (/ sh 2.0) 0.0))
                   sw sh))
        (vl-catch-all-apply 'vla-display (list vp :vlax-true))
        (vl-catch-all-apply 'vla-put-viewporton (list vp :vlax-true))
        (vla-put-layer vp al:*vp-layer*)
        (setq viewH (max fh (* fw (/ sh sw))))
        (al:aim-viewport vp centre viewH)
        (vl-catch-all-apply 'vla-put-displaylocked (list vp :vlax-true))

        (princ (strcat "\n  " name ": sheet " (rtos sw 2 0) " x " (rtos sh 2 0)
                       " at 1:" (rtos (/ viewH sh) 2 2)
                       " - " note
                       (if (and rot (> (abs (sin rot)) 1e-6)) "  (WARNING: frame is rotated - not supported)" "")))
        (setq made (1+ made))
      )
    )
  )

  (vla-put-activespace doc 1)   ; back to model space
  (vl-catch-all-apply 'vla-endundomark (list doc))
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\nAUTOLAYOUT: " (itoa made) " layout(s) created"
                 (if (> skipped 0) (strcat ", " (itoa skipped) " skipped") "")
                 ". Use PUBLISH to print them all to PDF."))
  (princ)
)

(princ "\nAuto Layout loaded. Type AUTOLAYOUT to run.")
(princ)

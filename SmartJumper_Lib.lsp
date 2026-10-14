; ============================================================================
; SMART JUMPER LIBRARY (Engine V1.4)
; - Crossing detection no longer depends on what is visible on screen
; - Ignores contacts at a wire's own start/end point
; - Draws with entmake in WCS (independent of the current UCS)
; - Shared helpers: cf:line, cf:arc, draw-junction-dot, cf:att-point, cf:draw-bus,
;   cf:device-routes (routes bottom terminals round the block to the bus)
; ============================================================================

(vl-load-com)

(setq cf:*terminal-gap* 5.0) ; Risers start this far above a terminal point

; --- PUBLIC FUNCTIONS -------------------------------------------------------

; Draws a wire from p1 to p2 (WCS) in colour col, adding bridge arcs of
; radius rad wherever it crosses existing geometry.
(defun smart-wire (p1 p2 rad col / intPoints)
  ; Force Z=0 for reliability
  (setq p1 (cf:flat p1))
  (setq p2 (cf:flat p2))

  (setq intPoints (cf:find-crossings p1 p2 rad))

  (if intPoints
    (draw-bridges-logic p1 p2 intPoints rad col)
    (cf:line p1 p2 col)
  )
  (princ)
)

; Line in WCS on the current layer
(defun cf:line (p1 p2 col)
  (entmake (list '(0 . "LINE")
                 (cons 8 (getvar "CLAYER"))
                 (cons 62 col)
                 (cons 10 (cf:flat p1))
                 (cons 11 (cf:flat p2))))
)

; Counter-clockwise arc in WCS (angles in radians)
(defun cf:arc (cen rad startAng endAng col)
  (entmake (list '(0 . "ARC")
                 (cons 8 (getvar "CLAYER"))
                 (cons 62 col)
                 (cons 10 (cf:flat cen))
                 (cons 40 rad)
                 (cons 50 (cf:norm-angle startAng))
                 (cons 51 (cf:norm-angle endAng))))
)

; Solid junction dot of radius rad (zero-ID donut as an LWPOLYLINE)
(defun draw-junction-dot (cenPt rad col / half)
  (setq half (/ rad 2.0))
  (entmake (list '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 (cons 8 (getvar "CLAYER"))
                 (cons 62 col)
                 '(100 . "AcDbPolyline")
                 '(90 . 2)
                 '(70 . 1)
                 (cons 43 rad)
                 (list 10 (- (car cenPt) half) (cadr cenPt))
                 '(42 . 1.0)
                 (list 10 (+ (car cenPt) half) (cadr cenPt))
                 '(42 . 1.0)))
)

; Terminal point of an attribute (from its entget data).
; Non-left-justified attributes are anchored at DXF 11 - DXF 10 moves when
; the attribute text changes. Aligned/Fit (72 = 3/5) are anchored at DXF 10.
(defun cf:att-point (attData / h v)
  (setq h (cdr (assoc 72 attData)))
  (setq v (cdr (assoc 74 attData)))
  (cf:flat
    (if (and (or (and h (/= h 0)) (and v (/= v 0)))
             (not (member h '(3 5))))
      (cdr (assoc 11 attData))
      (cdr (assoc 10 attData))
    )
  )
)

; Bounding box of an entity: ((minX minY) (maxX maxY)), or nil
(defun cf:bbox (ent / res mn mx)
  (setq res (vl-catch-all-apply 'vla-getboundingbox
              (list (vlax-ename->vla-object ent) 'mn 'mx)))
  (if (not (vl-catch-all-error-p res))
    (progn
      (setq mn (vlax-safearray->list mn))
      (setq mx (vlax-safearray->list mx))
      (list (list (car mn) (cadr mn)) (list (car mx) (cadr mx)))
    )
  )
)

; Works out how each terminal of one block reaches its bus.
;   blockEnt - the device block
;   termPts  - its terminal points, e.g. (plus minus shield)
;   busYs    - the bus height each terminal goes to (same order)
; A terminal below the block's vertical midpoint exits downwards, otherwise
; upwards. If its bus is on that side it is a straight riser; if not, the wire
; goes round the outside of the block to its nearer side. Wires going round
; get separate lanes (inner lane for the wire nearest that side) so they never
; cross each other.
; Returns one route per terminal (same order): a list of points; the last
; point's X is where the wire meets the bus.
(defun cf:device-routes (blockEnt termPts busYs / gap box midY i dirs around
                                                  avgX useLeft lane k pt d p0 base
                                                  exitY sideX routes)
  (setq gap cf:*terminal-gap*)
  (setq box (cf:bbox blockEnt))
  (setq midY (if box (/ (+ (cadar box) (cadadr box)) 2.0)))

  ; Exit direction per terminal (+1 up, -1 down), and which can't go straight
  (setq i 0)
  (foreach pt termPts
    (setq d (if (and midY (< (cadr pt) midY)) -1 1))
    (setq dirs (append dirs (list d)))
    (if (and box
             (if (= d 1)
               (< (nth i busYs) (cadr pt))      ; exits up, bus is below
               (> (nth i busYs) (cadr pt))))    ; exits down, bus is above
      (setq around (cons i around))
    )
    (setq i (1+ i))
  )

  ; Go round the side nearer to those terminals; nearest terminal = inner lane
  (if around
    (progn
      (setq avgX (/ (apply '+ (mapcar '(lambda (j) (car (nth j termPts))) around))
                    (length around)))
      (setq useLeft (< (- avgX (caar box)) (- (caadr box) avgX)))
      (setq around (vl-sort around
                     (if useLeft
                       '(lambda (a b) (< (car (nth a termPts)) (car (nth b termPts))))
                       '(lambda (a b) (> (car (nth a termPts)) (car (nth b termPts)))))))
    )
  )

  (setq i 0)
  (foreach pt termPts
    (setq d (nth i dirs))
    (setq p0 (list (car pt) (+ (cadr pt) (* d gap)) 0.0))
    (if (setq k (vl-position i around))
      (progn
        (setq lane (* gap (1+ k)))
        ; Clear of the block edge (or the start point, whichever is further out)
        (setq base (if (= d 1) (max (cadadr box) (cadr p0)) (min (cadar box) (cadr p0))))
        (setq exitY (+ base (* d lane)))
        (setq sideX (if useLeft (- (caar box) lane) (+ (caadr box) lane)))
        (setq routes (cons (list p0 (list (car pt) exitY 0.0) (list sideX exitY 0.0)) routes))
      )
      (setq routes (cons (list p0) routes))
    )
    (setq i (1+ i))
  )
  (reverse routes)
)

; Draws one bus at busY.
;   paths - one entry per connection, either a terminal point (straight riser
;           from terminal + gap) or a route from cf:device-routes. The last
;           segment always runs vertically to the bus.
;   smart = T uses smart-wire on the wires (bridges over crossings).
; Then a horizontal bus spanning the leftmost to rightmost connection.
; Returns the junction dot centres (connections between the two ends).
(defun cf:draw-bus (paths busY col smart brRad / routes xs minX maxX prev dots)
  ; Plain terminal point -> single-point route starting gap above it
  (setq routes
    (mapcar '(lambda (p)
               (if (listp (car p))
                 p
                 (list (list (car p) (+ (cadr p) cf:*terminal-gap*) 0.0))))
            paths))

  ; A. Wires: route segments, then vertical to the bus
  (foreach route routes
    (setq route (append route (list (list (car (last route)) busY 0.0))))
    (setq prev (car route))
    (foreach pt (cdr route)
      (if smart
        (smart-wire prev pt brRad col)
        (cf:line prev pt col)
      )
      (setq prev pt)
    )
  )

  ; B. Horizontal bus (span from geometry, not selection/click order)
  (setq xs (mapcar '(lambda (r) (car (last r))) routes))
  (setq minX (apply 'min xs))
  (setq maxX (apply 'max xs))
  (cf:line (list minX busY 0.0) (list maxX busY 0.0) col)

  ; C. Junction dot positions: wires that tee into the middle of the bus
  (foreach x xs
    (if (and (> (- x minX) 1e-6) (> (- maxX x) 1e-6))
      (setq dots (cons (list x busY 0.0) dots))
    )
  )
  (reverse dots)
)

; --- INTERNAL HELPERS (Do not call directly) --------------------------------

(defun cf:flat (p) (list (car p) (cadr p) 0.0))

(defun cf:norm-angle (a / twoPi)
  (setq twoPi (* 2.0 pi))
  (while (< a 0.0) (setq a (+ a twoPi)))
  (rem a twoPi)
)

; Name of the space entmake draws into (used to filter ssget "X")
(defun cf:current-space-name ()
  (if (and (= (getvar "TILEMODE") 0) (= (getvar "CVPORT") 1))
    (getvar "CTAB")
    "Model"
  )
)

(defun cf:layer-visible-p (layName / lay)
  (setq lay (tblsearch "LAYER" layName))
  (or (not lay)
      (and (>= (cdr (assoc 62 lay)) 0)             ; Negative colour = layer off
           (= 0 (logand 1 (cdr (assoc 70 lay)))))) ; Bit 1 = frozen
)

; True if obj's bounding box overlaps the box lo-hi (or it can't be measured)
(defun cf:bbox-overlap-p (obj lo hi / res mn mx)
  (setq res (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
  (if (vl-catch-all-error-p res)
    T
    (progn
      (setq mn (vlax-safearray->list mn))
      (setq mx (vlax-safearray->list mx))
      (and (<= (car mn) (car hi)) (>= (car mx) (car lo))
           (<= (cadr mn) (cadr hi)) (>= (cadr mx) (cadr lo)))
    )
  )
)

; Returns the points where segment p1-p2 crosses existing geometry, sorted
; from p1, excluding contacts within rad of either end and crossings too
; close together to bridge separately.
(defun cf:find-crossings (p1 p2 rad / tol lo hi tempEnt tempObj ss i ent obj
                                       copyObj intVar intArr ptList kept)
  (setq tol 0.001)
  (setq lo (list (- (min (car p1) (car p2)) tol) (- (min (cadr p1) (cadr p2)) tol)))
  (setq hi (list (+ (max (car p1) (car p2)) tol) (+ (max (cadr p1) (cadr p2)) tol)))

  ; Temporary line in the current space to test against
  (entmake (list '(0 . "LINE") (cons 10 p1) (cons 11 p2)))
  (setq tempEnt (entlast))
  (setq tempObj (vlax-ename->vla-object tempEnt))

  ; Every candidate in the current space (Ignore Text/Hatch), visible or not
  (setq ss (ssget "X"
             (list '(-4 . "<NOT") '(0 . "TEXT,MTEXT,DIMENSION,HATCH") '(-4 . "NOT>")
                   (cons 410 (cf:current-space-name)))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      (if (not (eq ent tempEnt))
        (progn
          (setq obj (vlax-ename->vla-object ent))
          (if (and (cf:layer-visible-p (vla-get-layer obj))
                   (cf:bbox-overlap-p obj lo hi))
            (progn
              ; Flatten a copy of simple geometry; test blocks etc. directly
              (if (wcmatch (vla-get-objectname obj) "AcDbLine,*Polyline,AcDbCircle,AcDbArc")
                (progn
                  (setq copyObj (vla-copy obj))
                  (flatten-object copyObj)
                )
                (setq copyObj nil)
              )

              (setq intVar (vl-catch-all-apply 'vla-intersectwith
                             (list tempObj (if copyObj copyObj obj) 0))) ; 0 = acExtendNone

              ; Only process if call succeeded and returned valid variant
              (if (and (not (vl-catch-all-error-p intVar))
                       intVar
                       (= (type intVar) 'VARIANT))
                (progn
                  (setq intArr (vl-catch-all-apply 'vlax-variant-value (list intVar)))
                  (if (and (not (vl-catch-all-error-p intArr))
                           intArr
                           (= (type intArr) 'SAFEARRAY)
                           (> (vlax-safearray-get-u-bound intArr 1) -1))
                    (setq ptList (append ptList (group-by-3 (vlax-safearray->list intArr))))
                  )
                )
              )

              (if copyObj (vla-delete copyObj))
            )
          )
        )
      )
    )
  )
  (entdel tempEnt)

  ; Drop contacts at the wire's own ends (terminals, buses it joins)
  (setq ptList (vl-remove-if
                 '(lambda (pt) (or (< (distance p1 pt) rad) (< (distance p2 pt) rad)))
                 (mapcar 'cf:flat ptList)))

  ; Sort from p1 and drop duplicates / overlapping bridges
  (foreach pt (sort-points-by-dist p1 ptList)
    (if (or (not kept) (>= (distance (car kept) pt) (* 2.0 rad)))
      (setq kept (cons pt kept))
    )
  )
  (reverse kept)
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

; pts must be sorted from startPt, clear of both ends and >= 2*rad apart
(defun draw-bridges-logic (startPt endPt pts rad col / currPt ang)
  (setq currPt startPt)
  (setq ang (angle startPt endPt))
  (foreach pt pts
    ; Line up to the bridge, then a half circle hopping over the crossing
    (cf:line currPt (polar pt (+ ang pi) rad) col)
    (cf:arc pt rad ang (+ ang pi) col)
    (setq currPt (polar pt ang rad))
  )
  (cf:line currPt endPt col)
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

(princ "\nSmartJumper Library V1.4 Loaded.")
(princ)

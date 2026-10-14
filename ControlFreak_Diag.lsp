; ============================================================================
; CONTROL FREAK DIAGNOSTICS
; Command: CFDIAG
;
; Checks the Control Freak setup and the product blocks, writes a text report
; and opens it in Notepad.
;   - Select blocks  -> checks those inserts (rotation, scale, missing
;                       attributes) plus their block definitions
;   - Press Enter    -> checks every block definition in the drawing
;
; For each block it lists the terminal attributes the scripts will find
; (PWR_POS/NEG, DAn+/-/G, INn, OUT/GND), where they are relative to the base
; point, whether they're on the top or bottom, and any problems.
; ============================================================================

(vl-load-com)

(setq cfd:*report-name* "ControlFreak_Diagnostics.txt")

; --- REPORT HELPERS ---------------------------------------------------------

(defun cfd:out (str) (setq cfd:*lines* (cons str cfd:*lines*)))

; Record a problem: shown under the current block and in the summary
(defun cfd:warn (context str)
  (cfd:out (strcat "    ! " str))
  (setq cfd:*issues* (cons (strcat context ": " str) cfd:*issues*))
)

(defun cfd:pad (s n)
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s
)

(defun cfd:num (x) (rtos x 2 2))

(defun cfd:yes (flag) (if flag "yes" "NO"))

; "20261014.1530" -> "2026-10-14 15:30"
(defun cfd:timestamp ( / s)
  (setq s (rtos (getvar "CDATE") 2 6))
  (strcat (substr s 1 4) "-" (substr s 5 2) "-" (substr s 7 2)
          " " (substr s 10 2) ":" (substr s 12 2))
)

; --- TERMINAL CLASSIFICATION ------------------------------------------------
; Mirrors the tag matching used by the wiring scripts

(defun cfd:data-line (tag suffix / n found)
  (setq n 1)
  (while (and (not found) (<= n 9))
    (if (wcmatch tag (strcat "*DA" (itoa n) suffix "*")) (setq found n))
    (setq n (1+ n))
  )
  found
)

; Returns (kind number) or nil. kind: POS NEG DA+ DA- DAG IN OUT GND
(defun cfd:class (tag / n)
  (setq tag (strcase tag))
  (cond
    ((wcmatch tag "*PWR_POS*") '("POS" nil))
    ((wcmatch tag "*PWR_NEG*") '("NEG" nil))
    ((setq n (cfd:data-line tag "+")) (list "DA+" n))
    ((setq n (cfd:data-line tag "-")) (list "DA-" n))
    ((setq n (cfd:data-line tag "G")) (list "DAG" n))
    ((wcmatch tag "IN#*") (list "IN" (atoi (substr tag 3))))
    ((wcmatch tag "*OUT*") '("OUT" nil))
    ((wcmatch tag "*GND*") '("GND" nil))
  )
)

(defun cfd:describe (cls)
  (cond
    ((= (car cls) "POS") "Power +")
    ((= (car cls) "NEG") "Power -")
    ((= (car cls) "DA+") (strcat "Data line " (itoa (cadr cls)) " +"))
    ((= (car cls) "DA-") (strcat "Data line " (itoa (cadr cls)) " -"))
    ((= (car cls) "DAG") (strcat "Data line " (itoa (cadr cls)) " shield"))
    ((= (car cls) "IN")  (strcat "Input " (itoa (cadr cls))))
    ((= (car cls) "OUT") "Switch OUT")
    ((= (car cls) "GND") "Switch GND")
  )
)

; --- BLOCK DEFINITION DATA --------------------------------------------------

; ATTDEFs in a block definition: ((tag flags point) ...), point relative to base
(defun cfd:def-attdefs (name / e d res)
  (setq e (tblobjname "BLOCK" name))
  (while (and e (setq e (entnext e)))
    (setq d (entget e))
    (if (= (cdr (assoc 0 d)) "ATTDEF")
      (setq res (cons (list (cdr (assoc 2 d)) (cdr (assoc 70 d)) (cf:att-point d)) res))
    )
  )
  (reverse res)
)

; Inserts a block temporarily at 0,0 to measure it and read its default
; dynamic block properties. Returns (extents dynamic-p propLines):
;   extents   ((minX minY) (maxX maxY)) relative to the base point, or nil
;   dynamic-p T if it's a dynamic block
;   propLines report lines describing the dynamic properties
(defun cfd:probe (name / obj res mn mx ext dyn props ro lines)
  (setq obj (vl-catch-all-apply 'vla-insertblock
              (list (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                    (vlax-3d-point '(0.0 0.0 0.0)) name 1.0 1.0 1.0 0.0)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq res (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
      (if (not (vl-catch-all-error-p res))
        (setq ext (list (vlax-safearray->list mn) (vlax-safearray->list mx))))

      (setq dyn (vl-catch-all-apply 'vla-get-isdynamicblock (list obj)))
      (if (= dyn :vlax-true)
        (progn
          (setq props (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
          (if (or (vl-catch-all-error-p props) (not props))
            (setq lines (list "  Dynamic block: yes, but its properties can't be read"))
            (progn
              (setq lines (list "  Dynamic block properties (defaults):"))
              (foreach p props
                (setq ro (vl-catch-all-apply 'vla-get-readonly (list p)))
                (setq lines (append lines
                              (list (strcat "    " (cfd:pad (vla-get-propertyname p) 22) " = "
                                            (cfd:prop-value p)
                                            (if (= ro :vlax-true) "   (read-only)" "")))))
              )
            )
          )
        )
      )
      (vla-delete obj)
    )
  )
  (list ext (= dyn :vlax-true) lines)
)

(defun cfd:def-extents (name) (car (cfd:probe name)))

; Product-style block definitions: not anonymous, not xrefs, not layouts
(defun cfd:all-block-names ( / rec names name)
  (while (setq rec (tblnext "BLOCK" (not rec)))
    (setq name (cdr (assoc 2 rec)))
    (if (and (not (wcmatch name "`**"))
             (= 0 (logand 4 (cdr (assoc 70 rec)))))
      (setq names (cons name names))
    )
  )
  (acad_strlsort names)
)

; --- CHECK ONE BLOCK DEFINITION ---------------------------------------------

(defun cfd:check-def (name / atts terms ext midY kinds tags seen
                             lines n inputs maxIn gaps cls side pt probe)
  (setq atts (cfd:def-attdefs name))
  (setq terms (vl-remove-if-not '(lambda (a) (cfd:class (car a))) atts))
  (setq probe (cfd:probe name))
  (setq ext (car probe))
  (if ext (setq midY (/ (+ (cadar ext) (cadadr ext)) 2.0)))

  (cfd:out "")
  (cfd:out (strcat "BLOCK  " name))
  (if ext
    (cfd:out (strcat "  Size " (cfd:num (- (caadr ext) (caar ext))) " x "
                     (cfd:num (- (cadadr ext) (cadar ext)))
                     "   extents from base point: X " (cfd:num (caar ext)) " to " (cfd:num (caadr ext))
                     ", Y " (cfd:num (cadar ext)) " to " (cfd:num (cadadr ext))))
    (cfd:warn name "Couldn't measure the block (temporary insert failed)")
  )
  (foreach l (caddr probe) (cfd:out l))

  ; Terminal table
  (if terms
    (progn
      (cfd:out "  Terminals (position relative to base point):")
      (foreach a terms
        (setq cls (cfd:class (car a)))
        (setq pt (caddr a))
        (setq side (cond ((not midY) "")
                         ((< (cadr pt) midY) "bottom")
                         (T "top")))
        (cfd:out (strcat "    " (cfd:pad (car a) 12)
                         (cfd:pad (cfd:describe cls) 22)
                         (cfd:pad (strcat "(" (cfd:num (car pt)) ", " (cfd:num (cadr pt)) ")") 22)
                         side
                         (if (= 2 (logand 2 (cadr a))) "   CONSTANT" "")))
        (setq kinds (cons cls kinds))
      )
    )
    (cfd:out "  No wiring terminals - the wiring commands will skip this block.")
  )
  (if (vl-remove-if '(lambda (a) (cfd:class (car a))) atts)
    (cfd:out (strcat "  Other attributes: "
                     (cf:join-str (mapcar 'car (vl-remove-if '(lambda (a) (cfd:class (car a))) atts))
                                  ", ")))
  )

  ; Summary line for this block
  (if terms
    (progn
      (setq lines nil)
      (foreach k kinds
        (if (and (member (car k) '("DA+" "DA-")) (not (member (cadr k) lines)))
          (setq lines (cons (cadr k) lines))
        )
      )
      (setq lines (vl-sort lines '<))
      (setq inputs (vl-sort (mapcar 'cadr (vl-remove-if-not '(lambda (k) (= (car k) "IN")) kinds)) '<))
      (cfd:out (strcat "  Summary: power " (if (assoc "POS" kinds) "yes" "no")
                       " | data lines "
                       (if lines
                         (apply 'strcat (mapcar '(lambda (l)
                                                   (strcat (itoa l)
                                                           (if (member (list "DAG" l) kinds) "(DMX)" "")
                                                           " "))
                                                lines))
                         "none ")
                       "| inputs " (itoa (length inputs))
                       (if (or (assoc "OUT" kinds) (assoc "GND" kinds)) " | switch" "")))
    )
  )

  ; --- Problems ---
  ; Constant attributes aren't stored on inserts, so scripts can't see them
  (foreach a terms
    (if (= 2 (logand 2 (cadr a)))
      (cfd:warn name (strcat (car a) " is a Constant attribute - scripts can't read it. Untick Constant in the block editor."))
    )
  )

  ; Duplicate terminal tags
  (foreach a terms
    (if (member (strcase (car a)) seen)
      (cfd:warn name (strcat "Tag " (car a) " appears more than once - only one will be used"))
      (setq seen (cons (strcase (car a)) seen))
    )
  )

  ; Pairs that must come together
  (if (and (assoc "POS" kinds) (not (assoc "NEG" kinds)))
    (cfd:warn name "Has PWR_POS but no PWR_NEG - AUTOPOWERBUS will skip it"))
  (if (and (assoc "NEG" kinds) (not (assoc "POS" kinds)))
    (cfd:warn name "Has PWR_NEG but no PWR_POS - AUTOPOWERBUS will skip it"))
  (foreach l lines
    (if (not (member (list "DA+" l) kinds))
      (cfd:warn name (strcat "Data line " (itoa l) " has DA" (itoa l) "- but no DA" (itoa l) "+")))
    (if (not (member (list "DA-" l) kinds))
      (cfd:warn name (strcat "Data line " (itoa l) " has DA" (itoa l) "+ but no DA" (itoa l) "-")))
  )
  ; Switch-style blocks (no power/data/inputs) need both OUT and GND
  (if (not (vl-some '(lambda (k) (member (car k) '("POS" "NEG" "DA+" "DA-" "DAG" "IN"))) kinds))
    (progn
      (if (and (assoc "OUT" kinds) (not (assoc "GND" kinds)))
        (cfd:warn name "Has OUT but no GND - switch won't get a ground connection"))
      (if (and (assoc "GND" kinds) (not (assoc "OUT" kinds)))
        (cfd:warn name "Has GND but no OUT"))
    )
  )

  ; Inputs should run 1..n without gaps
  (if inputs
    (progn
      (setq maxIn (apply 'max inputs))
      (setq n 1)
      (while (<= n maxIn)
        (if (not (member n inputs)) (setq gaps (cons (itoa n) gaps)))
        (setq n (1+ n))
      )
      (if gaps
        (cfd:warn name (strcat "Missing input(s): IN" (cf:join-str (reverse gaps) ", IN"))))
    )
  )

  ; Tags that look like terminals but won't be recognised
  (foreach a atts
    (if (and (not (cfd:class (car a)))
             (wcmatch (strcase (car a)) "*PWR*,DA#*"))
      (cfd:warn name (strcat "Tag " (car a) " looks like a terminal but isn't recognised - check spelling"))
    )
  )
  (length terms)
)

(defun cf:join-str (strs sep / res)
  (foreach s strs (setq res (if res (strcat res sep s) s)))
  (if res res "")
)

; --- CHECK ONE INSERT -------------------------------------------------------

(defun cfd:effective-name (ent / res)
  (setq res (vl-catch-all-apply 'vla-get-effectivename (list (vlax-ename->vla-object ent))))
  (if (vl-catch-all-error-p res) (cdr (assoc 2 (entget ent))) res)
)

; Value of a dynamic block property as text
(defun cfd:prop-value (prop / v)
  (setq v (vl-catch-all-apply 'vla-get-value (list prop)))
  (if (= (type v) 'VARIANT) (setq v (vlax-variant-value v)))
  (if (= (type v) 'SAFEARRAY) (setq v (vlax-safearray->list v)))
  (cond
    ((vl-catch-all-error-p v) "?")
    ((numberp v) (cfd:num v))
    ((= (type v) 'STR) v)
    ((listp v) (cf:join-str (mapcar '(lambda (x) (if (numberp x) (cfd:num x) "?")) v) ", "))
    (T "?")
  )
)

; Extents in the drawing, plus dynamic block properties where supported
(defun cfd:report-insert-geometry (ent / box obj dyn props ro)
  (if (setq box (cf:bbox ent))
    (cfd:out (strcat "  Extents: X " (cfd:num (caar box)) " to " (cfd:num (caadr box))
                     ", Y " (cfd:num (cadar box)) " to " (cfd:num (cadadr box))))
  )
  (setq obj (vlax-ename->vla-object ent))
  (setq dyn (vl-catch-all-apply 'vla-get-isdynamicblock (list obj)))
  (cond
    ((vl-catch-all-error-p dyn)
      (cfd:out "  Dynamic block: can't tell (not supported by this CAD's automation)"))
    ((= dyn :vlax-true)
      (setq props (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
      (if (or (vl-catch-all-error-p props) (not props))
        (cfd:out "  Dynamic block: yes, but its properties can't be read")
        (progn
          (cfd:out "  Dynamic block properties:")
          (foreach p props
            (setq ro (vl-catch-all-apply 'vla-get-readonly (list p)))
            (cfd:out (strcat "    " (cfd:pad (vla-get-propertyname p) 22) " = " (cfd:prop-value p)
                             (if (= ro :vlax-true) "   (read-only)" "")))
          )
        )
      )
    )
  )
)

; Returns the definition name
(defun cfd:check-insert (ent / d name ctx rot sx sy e att instTags defTags)
  (setq d (entget ent))
  (setq name (cfd:effective-name ent))
  (setq ctx (strcat name " insert at " (cfd:num (car (cdr (assoc 10 d)))) ","
                    (cfd:num (cadr (cdr (assoc 10 d))))))
  (setq rot (cond ((cdr (assoc 50 d))) (0.0)))
  (setq sx (cond ((cdr (assoc 41 d))) (1.0)))
  (setq sy (cond ((cdr (assoc 42 d))) (1.0)))

  (cfd:out "")
  (cfd:out (strcat "INSERT " ctx))
  (cfd:out (strcat "  Rotation " (cfd:num (* rot (/ 180.0 pi))) " deg, scale "
                   (cfd:num sx) " x " (cfd:num sy)))

  (cfd:report-insert-geometry ent)

  (if (> (abs (sin rot)) 1e-6)
    (cfd:warn ctx "Block is rotated - wiring assumes unrotated blocks (top/bottom terminals)"))
  (if (or (< sx 0) (< sy 0))
    (cfd:warn ctx "Block is mirrored - wiring may route to the wrong side"))

  ; Attributes the definition has but this insert doesn't (needs ATTSYNC)
  (setq e (entnext ent))
  (while (and e (= "ATTRIB" (cdr (assoc 0 (setq att (entget e))))))
    (setq instTags (cons (strcase (cdr (assoc 2 att))) instTags))
    (setq e (entnext e))
  )
  (foreach a (cfd:def-attdefs name)
    (if (and (cfd:class (car a))
             (= 0 (logand 2 (cadr a)))
             (not (member (strcase (car a)) instTags)))
      (setq defTags (cons (car a) defTags))
    )
  )
  (if defTags
    (cfd:warn ctx (strcat "Missing on this insert: " (cf:join-str (reverse defTags) ", ")
                          " - run ATTSYNC on " name)))
  name
)

; --- ENVIRONMENT ------------------------------------------------------------

(defun cfd:check-environment ( / app res sup trusted lib)
  (setq app (vlax-get-acad-object))
  (cfd:out "ENVIRONMENT")
  (setq res (vl-catch-all-apply '(lambda () (strcat (vla-get-name app) " " (vla-get-version app)))))
  (cfd:out (strcat "  CAD:            " (if (vl-catch-all-error-p res) "?" res)
                   "  (ACADVER " (getvar "ACADVER") ")"))
  (cfd:out (strcat "  Drawing:        " (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (cfd:out (strcat "  Units:          INSUNITS " (itoa (getvar "INSUNITS"))
                   (if (= (getvar "INSUNITS") 4) " (mm)" "")))
  (cfd:out (strcat "  UCS:            " (if (= (getvar "WORLDUCS") 1) "World" "custom (fine - scripts draw in WCS)")))

  (cfd:out (strcat "  Tools folder:   " (if cf:*root* cf:*root* "NOT SET")))
  (if (not cf:*root*)
    (cfd:warn "Setup" "Tools folder unknown - load ControlFreak.lsp rather than individual scripts"))

  (if (and cf:*root* cf:path-in-list-p cf:get-support-path)
    (progn
      (setq sup (cf:get-support-path))
      (if (not sup)
        (cfd:out "  Support path:   couldn't read (folder found via saved location instead)")
        (progn
          ; Not an error: the loader also finds the folder via its saved location
          (cfd:out (strcat "  Support path:   "
                           (if (cf:path-in-list-p cf:*root* sup)
                             "includes tools folder"
                             "doesn't include tools folder (OK - found via saved location; add it in Options > Files if you like)")))
        )
      )
      (setq trusted (vl-catch-all-apply 'getvar (list "TRUSTEDPATHS")))
      (cfd:out (strcat "  Trusted paths:  "
                       (cond ((/= (type trusted) 'STR) "n/a")
                             ((cf:path-in-list-p cf:*root* trusted) "includes tools folder")
                             (T "doesn't include tools folder (you may get security prompts)"))))
    )
  )

  (setq lib (if cf:block-library-path (cf:block-library-path)))
  (cfd:out (strcat "  Block library:  " (if lib lib "NOT FOUND")))
  (if (not lib)
    (cfd:warn "Setup" "Product block library not found - CFBLOCKS won't work"))

  (cfd:out (strcat "  Wiring library: "
                   (cond (cf:*lib-version* (strcat "V" cf:*lib-version*))
                         (cf:line "loaded, older version - reload ControlFreak.lsp")
                         (T "NOT LOADED"))))
  (if (not cf:line)
    (cfd:warn "Setup" "SmartJumper_Lib.lsp isn't loaded - wiring commands won't run"))
  (if (and cf:line (not cf:*lib-version*))
    (cfd:warn "Setup" "An older copy of the wiring library is loaded - reload ControlFreak.lsp"))

  (cfd:out "  Commands:")
  (foreach c '(("AUTOPOWERBUS" c:autopowerbus) ("DATALINE" c:dataline)
               ("AUTOINPUT" c:autoinput) ("SMARTMLINE" c:smartmline)
               ("AUTOLAYOUT" c:autolayout) ("CFBLOCKS" c:cfblocks)
               ("CFBUILD" c:cfbuild))
    (cfd:out (strcat "    " (cfd:pad (car c) 14) (if (eval (cadr c)) "loaded" "NOT LOADED")))
    (if (not (eval (cadr c)))
      (cfd:warn "Setup" (strcat (car c) " isn't loaded")))
  )

  (if BLOCK_MOMENTARY
    (foreach b (list BLOCK_MOMENTARY BLOCK_LATCHING)
      (cfd:out (strcat "  Switch block " (cfd:pad b 10) (if (tblsearch "BLOCK" b) "in drawing" "not in drawing (AUTOINPUT imports it)")))
    )
  )
)

; --- MAIN COMMAND -----------------------------------------------------------

(defun c:cfdiag ( / *error* ss i ent name defNames noTerms path f issues)
  (defun *error* (msg)
    (if f (close f))
    (princ (strcat "\nCFDIAG stopped: " msg))
    (princ)
  )

  (setq cfd:*lines* nil)
  (setq cfd:*issues* nil)

  (princ "\nSelect blocks to check <Enter = every block definition in the drawing>: ")
  (setq ss (ssget '((0 . "INSERT"))))

  (cfd:out (strcat "CONTROL FREAK DIAGNOSTICS   " (cfd:timestamp)))
  (cfd:out "==================================================================")
  (cfd:check-environment)

  (cfd:out "")
  (cfd:out "==================================================================")
  (if (not cf:att-point)
    (cfd:out "BLOCKS: skipped - wiring library not loaded")
    (progn
      (if ss
        (progn
          (cfd:out (strcat "SELECTED BLOCKS (" (itoa (sslength ss)) ")"))
          (repeat (setq i (sslength ss))
            (setq ent (ssname ss (setq i (1- i))))
            (setq name (cfd:check-insert ent))
            (if (not (member name defNames)) (setq defNames (cons name defNames)))
          )
          (setq defNames (acad_strlsort defNames))
          (cfd:out "")
          (cfd:out "------------------------------------------------------------------")
          (cfd:out "BLOCK DEFINITIONS")
        )
        (progn
          (setq defNames (cfd:all-block-names))
          (cfd:out (strcat "BLOCK DEFINITIONS (" (itoa (length defNames)) ")"))
        )
      )

      (foreach n defNames
        ; In "all" mode, blocks with no attributes that aren't dynamic are listed once at the end
        (if (or ss (cfd:def-attdefs n) (cadr (cfd:probe n)))
          (cfd:check-def n)
          (setq noTerms (cons n noTerms))
        )
      )
      (if noTerms
        (progn
          (cfd:out "")
          (cfd:out (strcat "Blocks with no attributes (ignored by wiring): "
                           (cf:join-str (reverse noTerms) ", ")))
        )
      )
    )
  )

  ; Summary
  (setq issues (reverse cfd:*issues*))
  (cfd:out "")
  (cfd:out "==================================================================")
  (cfd:out (strcat "SUMMARY: " (if issues (strcat (itoa (length issues)) " issue(s)") "no issues found")))
  (foreach s issues (cfd:out (strcat "  - " s)))

  ; Write report next to the drawing (or to the temp folder if unsaved)
  (setq path (strcat (if (= 1 (getvar "DWGTITLED")) (getvar "DWGPREFIX") (getvar "TEMPPREFIX"))
                     cfd:*report-name*))
  (if (setq f (open path "w"))
    (progn
      (foreach l (reverse cfd:*lines*) (write-line l f))
      (close f)
      (setq f nil)
    )
    (setq path nil)
  )

  ; Command line summary
  (princ (strcat "\n\nCFDIAG: " (if issues (strcat (itoa (length issues)) " issue(s) found:") "no issues found.")))
  (foreach s issues (princ (strcat "\n  - " s)))
  (if path
    (progn
      (princ (strcat "\nFull report: " path))
      (vl-catch-all-apply 'startapp (list "notepad" (strcat "\"" path "\"")))
    )
    (princ "\nCouldn't write the report file.")
  )
  (princ)
)

(princ "\nControl Freak diagnostics loaded. Type CFDIAG to run.")
(princ)

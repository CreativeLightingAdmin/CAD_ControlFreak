; ============================================================================
; CONTROL FREAK LOADER
; Load this ONE file (APPLOAD / drag-and-drop / Startup Suite). It finds the
; Control Freak folder, loads every script in the right order and adds:
;   CFBLOCKS - bring all product block definitions into the current drawing
;
; First run: if the folder isn't known yet you'll be asked to pick this file.
; The folder is then added to the Support File Search Path (and Trusted
; Locations on AutoCAD) so it is found automatically from then on.
; ============================================================================

(vl-load-com)

; --- CONFIGURATION ----------------------------------------------------------
(setq cf:*block-library* "Control Freak Product Blocks.dwg")
; Older file names still accepted if the library hasn't been renamed
(setq cf:*block-library-old* '("Control Freak Product Blocks 2025.dwg"))

; Full path of the block library, or nil
(defun cf:block-library-path ()
  (if cf:*root*
    (vl-some '(lambda (n) (findfile (strcat cf:*root* "\\" n)))
             (cons cf:*block-library* cf:*block-library-old*))
  )
)

; Load order matters: the library must come first
(setq cf:*scripts* '("SmartJumper_Lib.lsp"
                     "AutoPowerBus.lsp"
                     "DataLineBus.lsp"
                     "AutoInputDevice.lsp"
                     "SmartMultiJumper.lsp"
                     "AutoLayout.lsp"
                     "ControlFreak_Diag.lsp"
                     "ControlFreak_Build.lsp"))

(setq cf:*reg-key* "HKEY_CURRENT_USER\\Software\\ControlFreak")

; --- PATH HELPERS -----------------------------------------------------------

(defun cf:split (str delim / pos res)
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res))
    (setq str (substr str (+ pos 1 (strlen delim))))
  )
  (reverse (cons str res))
)

(defun cf:path-in-list-p (dir pathList)
  (setq dir (strcase (vl-string-right-trim "\\/" dir)))
  (vl-some '(lambda (p) (= dir (strcase (vl-string-right-trim "\\/" (vl-string-trim " " p)))))
           (cf:split pathList ";"))
)

; True if dir holds the Control Freak scripts
(defun cf:valid-root-p (dir)
  (and dir (findfile (strcat dir "\\SmartJumper_Lib.lsp")))
)

; Find the Control Freak folder: support path, then saved setting, then ask
(defun cf:find-root ( / f dir)
  (cond
    ((and (setq f (findfile "ControlFreak.lsp"))
          (cf:valid-root-p (setq dir (vl-filename-directory f))))
      dir)
    ((cf:valid-root-p (setq dir (vl-registry-read cf:*reg-key* "Path")))
      dir)
    ((and (setq f (getfiled "Locate ControlFreak.lsp" "ControlFreak.lsp" "lsp" 0))
          (cf:valid-root-p (setq dir (vl-filename-directory f))))
      dir)
  )
)

; Support File Search Path as a string, or nil if it can't be read.
; AutoCAD exposes it through Preferences > Files; progeCAD/IntelliCAD
; doesn't, but keeps it in the SRCHPATH system variable instead.
(defun cf:get-support-path ( / res)
  (setq res (vl-catch-all-apply
              '(lambda () (vla-get-supportpath
                            (vla-get-files (vla-get-preferences (vlax-get-acad-object)))))))
  (if (= (type res) 'STR)
    res
    (progn
      (setq res (vl-catch-all-apply 'getvar (list "SRCHPATH")))
      (if (= (type res) 'STR) res)
    )
  )
)

; Sets the support path using whichever method this CAD supports
(defun cf:set-support-path (newPath / res)
  (setq res (vl-catch-all-apply
              '(lambda () (vla-put-supportpath
                            (vla-get-files (vla-get-preferences (vlax-get-acad-object)))
                            newPath))))
  (if (vl-catch-all-error-p res)
    (vl-catch-all-apply 'setvar (list "SRCHPATH" newPath))
  )
)

; Remember the folder so other drawings/sessions find it without asking
(defun cf:register-root (dir / cur trusted)
  (vl-registry-write cf:*reg-key* "Path" dir)

  ; Support File Search Path (persists in the current profile)
  (setq cur (cf:get-support-path))
  (if (and cur (not (cf:path-in-list-p dir cur)))
    (if (progn
          (cf:set-support-path (if (= cur "") dir (strcat cur ";" dir)))
          (cf:path-in-list-p dir (cf:get-support-path)))   ; re-read to confirm
      (princ (strcat "\nControl Freak: added to support path: " dir))
      (princ "\nControl Freak: couldn't add the folder to the support path (using saved location instead).")
    )
  )

  ; Trusted Locations (AutoCAD 2014+; stops the SECURELOAD warning)
  (setq trusted (vl-catch-all-apply 'getvar (list "TRUSTEDPATHS")))
  (if (and (= (type trusted) 'STR) (not (cf:path-in-list-p dir trusted)))
    (vl-catch-all-apply 'setvar
      (list "TRUSTEDPATHS" (if (= trusted "") dir (strcat trusted ";" dir))))
  )
)

; --- BLOCK IMPORT -----------------------------------------------------------

(defun cf:block-names ( / rec names)
  (while (setq rec (tblnext "BLOCK" (not rec)))
    (setq names (cons (cdr (assoc 2 rec)) names))
  )
  names
)

; Brings every block definition from the product library into this drawing
; without placing anything. Blocks already in the drawing are left as they are.
(defun c:cfblocks ( / *error* lib wrapper oldEcho oldAttReq before added doc
                      blocks ref lastEnt res)
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldAttReq (setvar "ATTREQ" oldAttReq))
    (princ (strcat "\nCFBLOCKS stopped: " msg))
    (princ)
  )

  (setq lib (cf:block-library-path))
  (if (not lib)
    (progn
      (alert (strcat "Block library not found:\n" (if cf:*root* cf:*root* "?") "\\" cf:*block-library*))
      (exit)
    )
  )
  (princ (strcat "\nControl Freak: importing blocks from " lib " ..."))

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq blocks (vla-get-blocks doc))
  (setq wrapper (vl-filename-base lib))

  ; A leftover wrapper definition from an earlier import would stop the
  ; library being read again - remove it if it isn't used
  (if (tblsearch "BLOCK" wrapper)
    (vl-catch-all-apply 'vla-delete (list (vla-item blocks wrapper)))
  )

  (setq before (cf:block-names))

  ; The library is inserted as a block at 0,0 and the reference is deleted
  ; straight away - its block definitions stay in this drawing.
  ; Method 1: COM InsertBlock with the file path (no command prompts)
  (setq ref (vl-catch-all-apply 'vla-insertblock
              (list (vla-get-modelspace doc) (vlax-3d-point '(0.0 0.0 0.0)) lib 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p ref)
    (progn
      (princ (strcat "\n  InsertBlock failed (" (vl-catch-all-error-message ref)
                     "), trying -INSERT command..."))
      ; Method 2: -INSERT command, answering every prompt (no cancel)
      (setq oldEcho (getvar "CMDECHO"))
      (setq oldAttReq (getvar "ATTREQ"))
      (setvar "CMDECHO" 0)
      (setvar "ATTREQ" 0)
      (setq lastEnt (entlast))
      (command "_.-INSERT" lib "0,0" "1" "1" "0")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (setvar "ATTREQ" oldAttReq)
      (setvar "CMDECHO" oldEcho)
      (if (not (eq lastEnt (entlast)))
        (entdel (entlast))
      )
    )
    (vla-delete ref)
  )

  ; Remove the now-unused wrapper definition named after the library file
  (if (tblsearch "BLOCK" wrapper)
    (progn
      (setq res (vl-catch-all-apply 'vla-delete (list (vla-item blocks wrapper))))
      (if (vl-catch-all-error-p res)
        (princ (strcat "\n  Note: couldn't remove the unused block '" wrapper
                       "' - PURGE will clear it."))
      )
    )
  )

  (setq added (vl-remove-if '(lambda (n) (or (member n before)
                                             (= (strcase n) (strcase wrapper))
                                             (wcmatch n "`**")))
                            (cf:block-names)))
  (if added
    (progn
      (princ (strcat "\nControl Freak: " (itoa (length added)) " block(s) added:"))
      (foreach n (acad_strlsort added) (princ (strcat "\n  " n)))
      (princ "\nUse INSERT (or the Blocks palette) to place them.")
    )
    (princ (strcat "\nControl Freak: no new blocks added - the drawing already has them all,"
                   "\nor the import failed (see any messages above)."))
  )
  (princ)
)

; --- LOAD EVERYTHING --------------------------------------------------------

(defun cf:load-all ( / root res failed)
  (setq root (cf:find-root))
  (if (not root)
    (princ "\nControl Freak: folder not found - scripts not loaded.")
    (progn
      (setq cf:*root* root)
      (cf:register-root root)
      (foreach f cf:*scripts*
        (setq res (vl-catch-all-apply 'load (list (strcat root "\\" f))))
        (if (vl-catch-all-error-p res)
          (setq failed (cons (strcat f " (" (vl-catch-all-error-message res) ")") failed))
        )
      )
      (princ "\n\n--- Control Freak tools loaded ---")
      (princ "\n  AUTOPOWERBUS  DATALINE  AUTOINPUT  SMARTMLINE  AUTOLAYOUT  CFBLOCKS  CFDIAG  CFBUILD")
      (foreach f (reverse failed) (princ (strcat "\n  FAILED: " f)))
    )
  )
  (princ)
)

(cf:load-all)
(princ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ihs-def.lisp: 
; Author  Jun Sawada, University of Texas at Austin
;
; This file loads the IHS library and adjust the ACL2 theory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ACL2")

(deflabel before-loading-ihs)
(include-book "ihs/ihs-definitions" :dir :system)
(include-book "ihs/ihs-lemmas" :dir :system)
(deflabel after-loading-ihs)

(deftheory default-IHS-incompatibles
    '(oddp evenp floor mod rem truncate expt
      loghead logtail logior lognot logand logand
      logeqv logorc1 logorc2 logandc1 logandc2 logcount
      lognand lognor logxor))

(in-theory
 (set-difference-theories (current-theory :here)
	  (set-difference-theories  (universal-theory 'after-loading-IHS)
				    (universal-theory 'before-loading-IHS))))

(in-theory (disable default-ihs-incompatibles))
(in-theory
 (enable
  ihs-math				; From "math-lemmas"
  quotient-remainder-rules		; From "quotient-remainder-lemmas"
  logops-lemmas-theory))		; From "logops-lemmas"

(in-theory (disable (force)))

; Matt K. mod: Comment out runes that no longer exist.
(in-theory (disable
	    ;; (:generalize MOD-X-Y-=-X+Y)
	    ;; (:generalize MOD-X-Y-=-X)
	    (:generalize MOD-=-0)
	    (:generalize FLOOR-TYPE-4)
	    (:generalize FLOOR-TYPE-3)
	    (:generalize FLOOR-TYPE-2)
	    (:generalize FLOOR-TYPE-1)
	    ;; (:generalize FLOOR-BOUNDS)
	    (:generalize FLOOR-=-X/Y)))


(defun defword-tuple-typecheck-thms (tuple)
  (let ((tuple-thm-name
	 (pack-intern (first tuple) "DEFWORD-TUPLE-" (first tuple))))
    `(local (defthm ,tuple-thm-name
		(and
		 (integerp ,(second tuple))
		 (> ,(second tuple) 0)
		 (integerp ,(third tuple))
		 (>= ,(third tuple) 0)
		 (implies ,(fourth tuple) (stringp ,(fourth tuple))))
	      :rule-classes nil))))

   
(defun defword-struct-typecheck-thms (struct)
  (if (endp struct) nil
      (cons (defword-tuple-typecheck-thms (car struct))
	    (defword-struct-typecheck-thms (cdr struct)))))

; Matt K. mod: The :doc argument is no longer allowed for defthm, so I'm
; eliminating it.  Perhaps this would be a good place for defrule, but I'll
; leave that to someone else if they choose to take that on.
(defun type-check-thms
  (name struct conc-name set-conc-name keyword-updater
        ;; doc
        ) 
  (append     
   (list
    `(local (defthm defword-symbolp-name (symbolp ',name)
	      :rule-classes nil
	      ;; :doc "Defword name should be a symbol."
              ))
    `(local (defthm defword-symbolp-conc-name (symbolp ',conc-name)
	      :rule-classes nil
	      ;; :doc "Defword conc-name should be a symbol."
              ))
    `(local (defthm defword-symbolp-set-conc-name (symbolp ',set-conc-name)
	      :rule-classes nil
	      ;; :doc "Defword set-conc-name should be a symbol."
              ))
    `(local
      (defthm defword-symbolp-keyword-updater (symbolp ',keyword-updater)
	:rule-classes nil
	;; :doc "Defword keyword-updater should be a symbol."
        ))
; Matt K. mod, elimination of doc (see above);
#|
    `(local (defthm defword-stringp-doc
	      (implies ',doc (stringp ',doc))
	      :rule-classes nil
	      :doc "Defword doc should be a string."
              ))
|#
    )
   (defword-struct-typecheck-thms struct)))

(defun weak-defword-tuple-p (tuple)
  (or (and (true-listp tuple)
           (or (equal (length tuple) 3)
               (equal (length tuple) 4))
           (symbolp (first tuple)))
      (er hard 'defword
          "A field designator for DEFWORD must be a list, the first ~
             element of which is a symbol, the second a positive integer, ~
             and the third a non-negative integer.  If a fouth element is ~
             provided it must be a string.  This object violates these ~
             constraints: ~p0" tuple)))
                                            
(defun weak-defword-tuple-p-listp (struct)
  (cond
   ((null struct) t)
   (t (and (weak-defword-tuple-p (car struct))
           (weak-defword-tuple-p-listp (cdr struct))))))                      

(defun weak-defword-struct-p (struct)
  (cond
   ((true-listp struct) (weak-defword-tuple-p-listp struct))
   (t (er hard 'defword
          "The second argument of DEFWORD must be a true list. ~
          This object is not a true list: ~p0" struct))))                      

(defun weak-defword-guards (struct)
  (weak-defword-struct-p struct))                                               

;; This version of defword postpones the type checking of arguments until
;; macro expansion is completed.  Because the original defword checks
;; types of arguments before expanding macro, some defword expressions
;; are rejected even if it can be considered as a legitimate defword
;; expression. For instance, 
;; (defword new-word ((field1 *field1-pos* *field1-width*)
;;                    (field2 *field2-pos* *field2-width*)))
;; is rejected because *field1-pos* and other constants are not
;; integers. 
(defmacro defword* (name struct &key conc-name set-conc-name keyword-updater
; Matt K. mod: Removing :doc keyword, since neither defthm nor deflabel now
; accept it.
			 ;; doc
                         ) 
  (cond
   ((not (weak-defword-guards struct)))
   (t
    (let*
      ((conc-name (if conc-name
                      conc-name
                    (pack-intern name name "-")))
       (set-conc-name (if set-conc-name
                          set-conc-name
                        (pack-intern name "SET-" name "-")))
       (keyword-updater (if keyword-updater
			    keyword-updater
			  (pack-intern name "UPDATE-" name)))
       (type-check-thms
	(type-check-thms name struct conc-name set-conc-name
			 keyword-updater
                         ;; doc ; Matt K. mod (see above)
                         ))
       (accessor-definitions
        (defword-accessor-definitions 'RDB name conc-name struct))
       (updater-definitions
        (defword-updater-definitions 'WRB name set-conc-name struct))
       (field-names (strip-cars struct)))

      `(ENCAPSULATE ()                  ;Only to make macro expansion pretty.
         (DEFLABEL ,name
           ;; ,@(if doc `(:DOC ,doc) nil) ; Matt K. mod (see above)
           )
	 ,@type-check-thms
         ,@accessor-definitions
         ,@updater-definitions
         ,(defword-keyword-updater
	    name keyword-updater set-conc-name field-names))))))

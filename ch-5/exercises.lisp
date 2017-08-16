;---------- Chapter 5 exercises ----------

(defun simple-equal (x y)
  "Is x equal to y? Don't check in strings."
  (if (or (atom x) (atom y))
    (eql x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  (if (variable-p pattern)
      t
      (if (or (atom pattern) (atom input))
          (eql pattern input)
          (and (pat-match (first pattern) (first input))
               (pat-match (rest pattern) (rest input))))))

(defun variable-p (x)
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

; 1. Would it be a good idea to replace the complex and form in pat-match with
;    the simpler (every '#pat-match pattern input)?
;
; No, because every iterates through lists and returns when the shortest list
; runs out. Therefore, it would not catch failure scenarios caused by differing
; number of elements.

; 2. Experiment with this version of ELIZA. Show some exchanges where it
;    performs well, and some where it fails. Try to characterize the
;    difference. Which failures could be fixed by changing the rule set, which
;    by changing the pat-match function (and the pattern language it defines),
;    and which require a change to the eliza program itself?

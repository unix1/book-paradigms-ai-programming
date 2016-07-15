;---------- Chapter 2 exercises ----------

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> a the)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
   "Simple grammar rules for trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "Grammar used to generate sentences.")

(defun mappend (fn lst)
  "Apply fn to each element of the list and append the results."
  (apply #'append (mapcar fn lst)))

(defun random-elt (choices)
  "Pick a random element from choices list"
  (elt choices (random (length choices))))

(defun rule-lhs (rule)
  (car rule))

(defun rule-rhs (rule)
  (cdr (cdr rule)))

(defun rewrites (category grammar)
  (rule-rhs (assoc category grammar)))

(defun generate (phrase)
  (cond ((listp phrase) (mappend #'generate phrase))
        ((rewrites phrase *grammar*) (generate (random-elt (rewrites phrase *grammar*))))
        (t (list phrase))))

; 1. Write a version of generate which uses cond but avoids calling rewrites
;    twice.

(defun generate1 (phrase)
  (let ((rewrite (rewrites phrase *grammar*)))
    (cond ((listp phrase) (mappend #'generate1 phrase))
          (rewrite (generate1 (random-elt rewrite)))
          (t (list phrase)))))

; 2. Write a version of generate that explicitly differentiates between terminal
;    symbols (those with no rewrite rules) and nonterminal symbols.

(defun generate2 (phrase)
  (cond ((listp phrase) (mappend #'generate2 phrase))
        ((member phrase (mapcar #'car *grammar*)) (generate2 (random-elt (rewrites phrase *grammar*))))
        (t (list phrase))))

; 3.

; 4. One way of describing combine-all is that it calculates the cross-product
;    of the function append on the argument lists. Write the higher order
;    function cross-product, and define combine-all in terms of it.
;    The moral is to make your code as general as possible, because you never
;    know what you may want to do with it next.

(defun cross-product (xlist ylist fn)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (apply fn (list x y))) xlist))
           ylist))

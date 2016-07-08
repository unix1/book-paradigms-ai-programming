;---------- Chapter 1 exercises ----------

; 1. Define a version of last-name that handles "Rex Morgan MD", "Morton Downey
;    Jr" and whatever other cases you can think of.

(defparameter *suffixes* '(Jr Sr MD Esq))

(defun last-name (name &optional (n (- (length name) 1)))
  (if (eql n 0)
      nil
      (if (member (nth n name) *suffixes*)
          (last-name name (- n 1))
          (nth n name))))

; 2. Write a function to exponentiate, or raise a number to integer power. For
;    example: (power 3 2) = 3^2 = 9.

(defun power (n exp)
  (cond ((< exp 0) nil)
        ((eql exp 0) 1)
        ((eql exp 1) n)
        (t (* n (power n (- exp 1))))))

; 3. Write a function that counts the number of atoms in an expression. For
;    example: (count-atoms '(a (b) c)) = 3. Notice that there is something of
;    an ambiguity in this: should (a nil c) count as three atoms, or as two,
;    because it is equivalent to (a () c)?

(defun count-atoms (lst &optional (acc 0))
  (cond ((and (null (car lst)) (null (cdr lst))) acc)
        ((listp (car lst)) (count-atoms (cdr lst) (+ acc (count-atoms (car lst)))))
        (t (count-atoms (cdr lst) (+ acc 1)))))

; 4. Write a function that counts the number of times an expression occurs
;    anywhere within another expression. Example:
;    (count-anywhere 'a '(a ((a) b) a)) = 3.

(defun count-anywhere (needle haystack &optional (acc 0))
  (cond ((and (null (car haystack)) (null (cdr haystack))) acc)
        ((eql (car haystack) needle) (count-anywhere needle (cdr haystack) (+ 1 acc)))
        ((listp (car haystack))
                (count-anywhere needle
                                (cdr haystack)
                                (+ acc (count-anywhere needle (car haystack)))))
        (t (count-anywhere needle (cdr haystack) acc))))

; 5. Write a function to compute the dot product of two sequences of numbers,
;    represented as lists. The dot product is computed by multiplying
;    corresponding elements and then adding up the resulting products. Example:
;    (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110

(defun dot-product (nums1 nums2)
  (reduce #'+
          (mapcar #'*
                  nums1
                  nums2)))

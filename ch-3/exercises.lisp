;---------- Chapter 3 exercises ----------

(let ((x 40)
      (y (+ 1 1)))
  (+ x y))

((lambda (x y)
   (+ x y))
  40 (+ 1 1))

; 1. Show a lambda expression that is equivalent to the above let* expression.
;    You may need more than one lambda.

(let* ((x 6)
       (y (* x x)))
  (+ x y))

((lambda (x)
   (+ x ((lambda (x) (* x x)) x)))
  6)

; 2. The function cons can be seen as a special case of one of the other
;    functions listed previously. Which one?

; 3. Write a function that will print an expression in dotted pair notation.
;    Use the built-in function princ to print each component of the expression.

; 4. Write a function that, like the regular print function, will print an
;    expression in dotted pair notation when necessary but will use normal list
;    notation when possible.

; 5. Exercise in altering structure. Write a program that will play the role of
;    the guesser in the game of Twenty Questions. The user of the program will
;    have in mind any type of thing. The program will ask questions of the
;    user, which must be answered yes or no, or "it" when the program has
;    guessed it. If the program runs out of guesses, it gives up and asks the
;    user what "it" was. At first the program will not play well, but each time
;    it plays, it will remember the user's replies and use them for subsequent
;    guesses.

; 6. Given the following initialization for the lexical variable a and the
;    special variable *b* what will be the value of the let form?

(setf a 'global-a)
(defvar *b* 'global-b)

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

; 7. [Call to a function may contain more than one keywords with the same
;    name.] This is not an error; Common Lisp declares that the leftmost value
;    is the one that counts. Why do you think the leftmost of two keys is the
;    one that counts, rather than the rightmost?

; 8. Some versions of Kyoto Common Lisp (KCL) have a bug wherein they use the
;    rightmost value when more than one keyword/value pair is specified for the
;    same keyword. Change the definition of find-all so that it works in KCL.

; 9. Write a version of length using the function reduce.

(defun length-using-reduce (lst)
  (reduce #'(lambda (acc val) (+ acc 1))
          lst
          :initial-value 0))

; 10. Use a reference manual or describe to figure out what hte function lcm
;     and nreconc do.

; - lcm computes the least common multiple of given integers.
; - nreconc takes 2 lists, reverses the first one in place and concatenates,
;   again, in place (using nconc), the second on the first; it returns the
;   result of that nconc, which is the final value of the first list.

; 11. There is a built-in Common Lisp function that, given a key, a value, and
;     an association list, returns a new association list that is extended to
;     include the key/value pair. What is the name of this function?

; 12. Write a single expression using format that will take a list of words and
;     print them as a sentence, with the first word capitalized and a period
;     after the last word. You will have to consult a reference to learn new
;     format directives.

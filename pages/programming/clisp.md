# Learn Common Lisp

```clojure
;; An example for understanding the concept of clojure
;; The following function returns a function
(defun addn (n)
  #'(lambda (x)
    (+ x n)))
```

```clojure
;; Lisp Operations
(cons 'a '(b c d)) ; Short for "Construct"
(car '(a b c)) ; Short for "Contents of the Address part of Register number"
(cdr '(a b c)) ; Short for "Contents of the Decrement part of Register number"

(nth 1 '(a b c))
(nthcdr 1 '(a b c))
(first '(a b c))
(last '(a b c))

(mapcar #'list '(1 2 3) '(10 11 12 13)) ; based on the next `car`
(maplist #'list '(1 2 3)) ; based on the next `cdr`

(member 'a '(a b c a)) ; Check whether there is some member
(remove 'a '(a b c a)) ; This is non-destructive

(adjoin 'b '(a b c))
(adjoin 'z '(a b c))

(union '(a b c) '(c b s))
(intersection '(a b c) '(b b c))
(set-difference '(a b c d e) '(b e))

(length '(1 2 3)) ; Return the length of the list
(subseq '(a b c d) 1 3)
(subseq '(a b c d) 1)
(reverse '(a b c d))
(sort '(4 2 3 1) #'>) ; Use it with caution because it is destructive

(every #'oddp '(1 3 5))
(some #'evenp '(1 3 4))
(every #'> '(1 3 5) '(0 2 4))

(setf x '(a b))
(push 'c x) ; This operation is destructive.
(pop x) ; Also destructive.

;; Predicate
(zerop (- 2 2))
(listp '(a b c))
(numberp 12)
(integerp 13)
(floatp 12.3)
(stringp "hello")
(typep 12 'integer)
(oddp 13)
(evenp 12)

;; Input and Output
(format t "~d plus ~d equals ~d. ~%" 2 3 (+ 2 3))
(format t "You just typed ~a~%" (read))

;; Variables
; The difference between `defparameter` and `defvar`
(defparameter *glob* 99)
(defparameter *glob* 1)
(defvar *glob* 2)
(defvar *glob* 4) ; defvar sets variable only once
(defconstant limit (+ *global* 1)) ; set a constant

(boundp '*global*) ; check if it is a global variable

(setf *glob* 78) ; Assignment

(typep 12 'integer) ; Check the type
(typep '(a b c) 'list)

(eql 12 12)
(eq 12 12)
(equal 12 12)

;; Functions
(funcall #'+ 1 2 3 4 5)
(apply #'+ '(1 2 3))

;; Array
(setf arr (make-array '(2 3) :initial-element nil)) ; Array dimensions can be 7 at most
(aref arr 0 1) ; array reference?
(setf (aref arr 0 1) 'b) ; set the element value
#2a((nil b nil) (nil nil nil)) ; literal array (2 is the number of dimensions)
;; one-dimension array is a vector
(make-array 3 :initial-element 1) ; equals the above
(vector 1 1 1)
(svref (vector 'a 'b 3) 1) ; simple vector reference
#(a b 3) ; literal vector
;; string is a vecotr that consists of characters
#\b ; literal character
(sort "acb" #'char<)
(aref "acb" 1)
(svref "acb" 1)
(char "acb" 1)
(elt "acb" 1)
(elt '(a b c) 1) ; elt means element. It is general-purpose for both list and arrays

(position #\a "fantasy")
(position #\a "fantasy" :start 3 :end 5)
(position #\a "fantasy" :from-end t)
(position 'a '((c d) (a b)) :key #'car)
(position 3 '(1 0 7 5) :test #'<)
(position-if #'oddp '(2 3 4 5))
(remove-if #'oddp '(1 2 3 4))
(remove-duplicates "abracadabra")

(reduce #'+ '(1 2 3)) ; famous reduce operation, equals to the below one
( + (+ 1 2) 3)


(equal "fred" "Fred")
(string-equal "fred" "Fred")
(format nil "~A or ~A" "truth" "dare") ; create a string via format
(concatenate 'string "not" "to worry")
(concatenate 'list '(a b c) '(1 2 3))
(concatenate 'vector #(a b c) #(1 2 3))

```

## References
* [Learn X in Y minutes](http://learnxinyminutes.com/docs/common-lisp/)
* [Lisp Quickstart](http://cs.gmu.edu/~sean/lisp/LispTutorial.html)

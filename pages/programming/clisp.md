# Learn Common Lisp
```lisp
;; An example for understanding the concept of closure
;; The following function returns a function
(defun addn (n)
  #'(lambda (x)
    (+ x n)))
```

```lisp
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
```

## References
* [Learn X in Y minutes](http://learnxinyminutes.com/docs/common-lisp/)
* [Lisp Quickstart](http://cs.gmu.edu/~sean/lisp/LispTutorial.html)

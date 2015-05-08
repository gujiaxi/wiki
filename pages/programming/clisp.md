# Learn Common Lisp

## Clojure
Below is an example for understanding the concept of clojure in Lisp. It returns a function.

```
(defun addn (n)
  #'(lambda (x)
    (+ x n)))
```

## Operations

```
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

(format t "~d plus ~d equals ~d. ~%" 2 3 (+ 2 3))
(format t "You just typed ~a~%" (read))

(funcall #'+ 1 2 3 4 5)
(apply #'+ '(1 2 3))
```

## Predicates

```
(zerop (- 2 2))
(listp '(a b c))
(numberp 12)
(integerp 13)
(floatp 12.3)
(stringp "hello")
(typep 12 'integer)
(oddp 13)
(evenp 12)
(fboundp '+) ; check whether there is a function bound with the symol
```

## Variables

```
;; The difference between `defparameter` and `defvar`
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

;; `let` creates a new lexical context and all the variable in it is local
(let ((x 7) (y 8)) (format t "Number ~d" (+ x y)) (format t "~%"))
(let ((x 1) (y (+ x 1))) (format t "~d" y)) ; It's wrong
(let* ((x 1) (y (+ x 1)) (format t "~d" y)) ; It's right
```

## Arrays

```
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
(+ (+ 1 2) 3)
```

## Structures

```
(defstruct point x y)
(setf p (make-point :x 1 :y 2))
(setf (point-x p) 3)
(setf (point-y p) 4)
```

## Hash Table

```
(setf ht (make-hash-table))
(setf ht (make-hash-table :size 10))
(setf (gethash 'color ht) 'red)
(gethash 'color ht)
(setf (gethash 'shape ht) 'square)
(setf (gethash 'gender ht) 'male)
(remhash 'gender ht)
(maphash #'(lambda (k v) (format t "~A = ~A~%" k v)) ht)


(equal "fred" "Fred")
(string-equal "fred" "Fred")
(format nil "~A or ~A" "truth" "dare") ; create a string via format
(concatenate 'string "not" "to worry")
(concatenate 'list '(a b c) '(1 2 3))
(concatenate 'vector #(a b c) #(1 2 3))
```

## Blocks
Three basic function: `progn`, `block`, `tagbody`.

```
(progn
  (format t "A")
  (format t "B")
  (+ 11 12))

(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this."))

(tagbody
  (setf x 0)
  top
    (setf x (+ x 1))
	(format t "~A " x)
	(if (< x 10) (go top)))
```

## Conditions
`if`, `when`. `unless` is opposite to `when`. All their parent body form is `cond`.

```
(if (oddp 13) (format t "Is odd.") (format t "Is even"))
(if (oddp that) (progn (format t "Hmm, that's odd.") (+ that 1)))
(when (oddp that) (format t "Hmm, that's odd.") (+ that 1)) ; equals to the above one

(cond ((oddp 13) (format t "Is odd")) ((oddp 15) (format t "Is odd")))

(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
	((apr jun sept nov) 30)
	(feb (if (leap-year) 29 28))
	(otherwise "unknow month)))
```

## Loop
The essence of Loop is **Iteration** and the basic operation of iteration is `do`. We can use `return`, `return-from` and `go` in the do block.

```
;; compare the following result with the `do*` version
(do ((i 0 (+ i 1)) (j 10 (- j i)))
  ((> i 5) 'done)
  (format t "~a ~a~%" i j))

(dolist (x '(a b c d) 'done) (format t "~a " x))
(dotimes (x 4 'done) (format t "~a " x))

(mapc #'(lambda (x y) (format t "~a ~a " x y)) '(a b c) '(x y z)) ;	`mapc` is similar to `mapcar` but it is non-destructive. It always returns the second parameter.
```

## Multiple Values
As a functional programming language, Lisp can return multiple values. In order to get the multiple valus, we need to `multiple-value-bind`.

```
(get-decoded-time) ; return the current time
(values 'a nil (+ 2 4))
(multiple-value-bind (x y z) (values 1 2) (format t "~a ~a ~a~%" x y z))
(multiple-value-bind (s m h) (get-decoded-time) (format t "~a:~a:~a~%" h m s))
(multiple-value-call #'+ (values 1 2 3))
(multiple-value-list (values 'a 'b 'c))
```

## Throw and Catch
Lisp uses `throw` and `catch` to deal with the errors.

```
(defun super ()
  (catch 'abort
    (sub)
	(format t "We'll never see this.")))
(defun sub ()
  (throw 'abort 99))
```

## Functions
Global functions and local functions.

```
(setf (symol-function 'add2) #'(lambda (x) (+ x 2)))
(defun add2 (x) (+ x @)) ; same to the above

(labels ((add2 (x) (+ x 2)) (consa (x) (cons 'a x))) (consa (add2 3))) ; `labels` creates local functions. similar to `let`

;; Something about optional parameters: `rest`, `optional`, `key`
(defun our-funcall (fn &rest args) (apply fn args)) ; `rest` is a list of the rest parameters
(defun philosoph (thing &optional (property 'fun)) (list thing 'is property))
(defun keylist (a &key x y z) (list a x y z))
(keylist 1 :y 3 :x 2)
```

## Input and Output
First set the pathname and then do read and write.

```
(setf path (make-pathname :name "myfilename"))
(setf str (open path :direction :output :if-exist :supersede))
(fornat str "something~%")
(close str)
(setf str (open path :direction :input))
(read-line str)
(close str)

(with-open-file (str path :direction :output :if-exist :supersede) (format str "something~%")) ; `with-open-file` will close it automtically

(read-from-string "a b c")

;; `prin1`, `princ`, `terpri`
(prin1 "Hello") ; prin1 is for machine
(princ "Hello") ; princ is for human
(terpri)
```

## References
* [Learn X in Y minutes](http://learnxinyminutes.com/docs/common-lisp/)
* [Lisp Quickstart](http://cs.gmu.edu/~sean/lisp/LispTutorial.html)

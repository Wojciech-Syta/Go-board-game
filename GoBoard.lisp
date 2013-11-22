;doesnt get full count when propg changes
;doesnt change union after adding
; lots of cleaning to do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Board Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter size 9)

;union count
(defparameter strng 0)

;two dim array with elements (Color Linerties Union#)
(defvar *board* (make-array (list size size) :element-type 'list :initial-element '(N 999 0)))

(defun filled (x y)
	   (aref *board* x y))

(defun setC (x y val lib union)
	   (setf (aref *board* x y) (list val lib union)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Placement Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;make return value of checksurr (list lib union)
;cond strng to check if new union
(defun evalchoice (x y wORb)
	   (let ((val (filled x y)))
	     (cond ((not (equal 'N (car val)))
		    nil)
		   (t
		    (let ((lib (checkSurr x y wORb)))
		      (cond ((zerop lib)
			     nil)
			    (t
			     (setC x y wORb lib 0)
			     (checkSurr x y wORb 3 lib union))))))))

;considinf destroying by union
(defun destroy (lst wORb)
	   (let ((x (car lst))
		 (y (car (cdr lst))))
	     (setC x y 'N 999 0)
	     (checkSurr x y wORb 2)))

;clean this up
(defun genSurr (x y)
	   (list
	    (cond ((> x 0)
		   (let* (
			  (a (filled (- x 1) y))
			  (leftColor (car a))
			  (leftLibs (second a))
			  (leftunion (third a)))
		     (list leftColor leftLibs leftunion)))
		  (t
		   '(NIL NIL)))
	    (cond ((< x (- size 1))
		   (let* (				 
			  (b (filled (+ x 1) y)) 
			  (rightColor (car b))
			  (rightLibs (second b))
			  (rightunion (third b)))
		     (list rightColor rightLibs rightunion)))
		  (t
		   '(NIL NIL)))
	    (cond ((> y 0)
		   (let* (
			  (d (filled x (- y 1)))
			  (downColor (car d))
			  (downLibs (second d))
			  (downunion (third d)))
		     (list downColor downLibs downunion)))
		  (t
		   '(NIL NIL)))
	    (cond ((< y (- size 1))				    
		   (let* (
			  (c (filled x (+ y 1))) 
			  (upColor (car c))
			  (upLibs (second c))
			  (upunion (third c)))
		       (list upColor upLibs upunion)))
		  (t
		   '(NIL NIL NIL)))))

;use strng to check union creation
(defun checkUnion (lst &optional(acc 0))
	   (cond ((equal (car lst) 'nil)
		  acc)
		 (t
		  (loop for i in (cdr lst) do
		       (cond ((not (equal (third (car lst)) (third i)))
			      (setf acc (+ acc (second i)))
			      (incf strng))))		    		    
		  (checkUnion (cdr lst) acc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;In progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
(defun checkSurr (x y wORb &optional (checking 1)(lib 0)(union 0))
	   (let*
	    ((count 0)
	     (lt (genSurr x y))
	     (left (list (- x 1) y))
	     (right (list (+ x 1) y))
	     (down (list x (- y 1)))
	     (up (list x (+ y 1)))
	     (dirc (list left right down up))
	     (temp '()))
					;Modes checking or setting or destroying
					;2 == Destroying
	     (cond ((= checking 2)
		    (loop for i in lt do
			 (cond ((and (not (equal i '(nil nil)))
				     (equal (car i) wORb))		       
				(destroy (car  dirc) wORb)))
			 (setf dirc (cdr dirc))))
		   
					;propogating Liberty changes		   
		   ((= checking 3)
		    
		    (loop for i in lt do
			 (cond ((and (not (equal i '(nil nil)))
				     (equal (car i) wORb)
				     (not (equal (second  i) lib)))
				(setf temp (car dirc))
				(setC (car temp)(second temp) wORb lib union)
				(checkSurr (car temp)(second temp) wORb 3 lib)))
			 (setf dirc (cdr dirc))))
		    		    		      	   
					;calculating Liberties
		   ((= checking 1)		   
		    (incf strng)
					;if any adjacent are empty
		    (loop for i in lt do
			 (cond ((and (not (equal i '(nil nil)))
				     (= (second i) 999))
				(incf count))))
			    		    
			 
					;if any adjacent are same color
		    (loop for i in lt do
			 (cond ((and (not (equal i '(nil nil)))
				     (equal (car i) wORb))
				(setf temp (list temp i)))))
		    
		    (cond (( equal (car temp) nil)
			   (break))
			  ((equal (cdr temp) nil)
			   (setf count (+ count (+ (car temp) 1))))
			  (t
			   (setf count (+ count (checkUnion temp)))))

					;if any opposite color, with only 1 Liberty
		    (loop for i in lt do 
			 (cond ((and (not (equal i '(nil nil)))
				     (not (equal (car i) wORb))
				     (= 1 (second i)))
				(incf count)
				(destroy (car dirc) wORb)))
			 (setf dirc (cdr dirc)))
		    count
		    ))))

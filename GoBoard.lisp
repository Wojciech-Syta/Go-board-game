;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Board Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *board* (make-array '(9 9) :element-type 'list :initial-element '(N 999)))

(defun filled (x y)
	   (aref *board* x y))

(defun setC (x y val lib)
	   (setf (aref *board* x y) (list val lib)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Placement Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evalchoice (x y wORb)
	   (let ((val (filled x y)))
	     (cond ((not (equal 'N (car val)))
		    nil)
		   (t
		    (let ((lib (checkSurr x y wORb)))
		      (cond ((zerop lib)
			     nil)
			    (t
			     (setC x y wORb lib)
			     (checkSurr x y wORb 3 lib))))))))

(defun checkSurr (x y wORb &optional (checking 1)(lib 0))
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
				     (equal (car dirc) wORb)
				     (not (equal (second (car dirc)) lib)))
				(checkSurr (car i)(second i) wORb 3 lib)))
			 (setf dirc (cdr dirc))))
		    		    		      	   
					;calculating Liberties
		   ((= checking 1)
		    (setf temp dirc)
					;if any adjacent are empty
		    (loop for i in lt do
			 (cond ((and (not (equal i '(nil nil)))
				     (= (second i) 999))
				(incf count)))
			 (setf temp (cdr temp)))

		    (setf temp dirc)
					;if any adjacent are same color
		    (loop for i in lt do
			 (cond ((and (not (equal i '(nil nil)))
				     (equal (car i) wORb))
				(setf count (+ count (second i)))))
			 (setf temp (cdr temp)))

					;if any opposite color, with only 1 Liberty
		    (loop for i in lt do 
			 (cond ((and (not (equal i '(nil nil)))
				     (not (equal (car i) wORb))
				     (= 1 (second i)))
				(incf count)
				(destroy (car dirc) wORb)))
			 (setf dirc (cdr dirc)))
		    count))))
(defun destroy (lst wORb)
	   (let ((x (car lst))
		 (y (car (cdr lst))))
	     (setC x y 'N 999)
	     (checkSurr x y wORb 2)))

(defun genSurr (x y)
	   (list
	    (cond ((> x 0)
		   (let* (
			  (a (filled (- x 1) y))
			  (leftColor (car a))
			  (leftLibs (car (cdr a))))
		     (list leftColor leftLibs)))
		  (t
		   '(NIL NIL)))
	    (cond ((< x 8)
		   (let* (				 
			  (b (filled (+ x 1) y)) 
			  (rightColor (car b))
			  (rightLibs (car (cdr b))))
		     (list rightColor rightLibs)))
		  (t
		   '(NIL NIL)))
	    (cond ((> y 0)
		   (let* (
			  (d (filled x (- y 1)))
			  (downColor (car d))
			  (downLibs (car (cdr d))))
		     (list downColor downLibs)))
		  (t
		   '(NIL NIL)))
	    (cond ((< y 8)				    
		   (let* (
			  (c (filled x (+ y 1))) 
			  (upColor (car c))
			  (upLibs (car (cdr c))))
		       (list upColor upLibs)))
		  (t
		   '(NIL NIL)))))

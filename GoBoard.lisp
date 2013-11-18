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
			     (propogateLibChanges x x wORb lib))))))))


(defun checkSurr (x y wORb &optional (checking 1))
	   (let*
	    ((count 0)
	     (lt (genSurr x y))
	     (dirc '(left right down up))
	     (up (list x (+ y 1)))
	     (down (list x (- y 1)))
	     (left (list (- x 1) y))
	     (right (list (+ x 1) y)))
					;Modes checking or setting or destroying
					;2 == Destroying
	     (cond ((= checking 2)
		    (loop for i in lt do
			 (cond ((equal (car i) wORb)
				(destroy (car (car dirc)) wORb)))
			 (setq dirc (cdr dirc)))))
	     
	     
					;1 == checking
		   ((= checking 1)
					;if any adjacent are empty
		    (setq temp dirc)
		    (loop for i in lt do
			 (cond ((= (last i) 999)
				(incf count)))
			 (setq temp (cdr temp)))
					;(cond ((= downLibs 999)
					;	   (incf count)))
		   
		   
					;if any adjacent are same color
		    (setq temp dirc)
		    (loop for i in lt do
			 (cond ((equal (car i) wORb)
				(setf count (+ count (last i)))))
			 (setq temp (cdr temp)))
					;(cond ((equal downColor wORb)
					;     (setf count (+ count (- downLibs 1)))
					;     (checkSurr x (- y 1) wORb 6)))
			
		    
					;if any opposite color, with only 1 Liberty
		    (loop for i in lt do
			 (cond ((and (not (= (car i) wORb))
				     (= 1 (last i)))
				(incf count)
				(destroy (car (car dirc)))))
			 (setq dirc (cdr dirc)))
		   
		   
					; (cond ((and (not (equal rightColor wORb))
					;		(= 1 rightLibs))
					;	   (incf count)
					;	   (destroy (+ x 1) y wORb)))
		    count)))

(defun destroy (x y wORb)
	(setC x y 'N 999)
	(checkSurr x y wORb 2))

(defun propogateLibChanges (x y wORb lib)
	   (let*(
		(a (filled (- x 1) y)) 
		(b (filled (+ x 1) y)) 
		(c (filled x (+ y 1))) 
		(d (filled x (- y 1))) 
		(leftColor (car a))
		(leftLibs (car (cdr a)))
		(rightColor (car b))
		(rightLibs (car (cdr b)))
		(upColor (car c))
		(upLibs (car (cdr c)))
		(downColor (car d))
		(downLibs (car (cdr d))))
	     (cond ((and (not (equal rightLibs lib))
			 (equal rightColor wORb))
		    (setC (+ x 1) y wORb lib)
		    (propogateLibChanges (+ x 1) y wORb lib)))
	     (cond ((and (not (equal leftLibs lib))
			 (equal leftColor wORb))
		    (setC (- x 1) y wORb lib)
		    (propogateLibChanges (- x 1) y wORb lib)))
	     (cond ((and (not (equal upLibs lib))
			 (equal upColor wORb))
		    (setC x (+ y 1) wORb lib)
		    (propogateLibChanges x (+ y 1) wORb lib)))		   
	     (cond ((and (not (equal downLibs lib))
			 (equal downColor wORb))
		    (setC x (- y 1) wORb lib)
		    (propogateLibChanges x (- y 1) wORb lib)))))

(defun genSurr (x y)
	   (list
	    (cond ((> x 1)
		   (let* (
			  (a (filled (- x 1) y))
			  (leftColor (car a))
			  (leftLibs (car (cdr a))))
		     (list leftColor leftLibs)))
		  (t
		   '(NIL NIL)))
	    (cond ((< x 19)
		   (let* (				 
			  (b (filled (+ x 1) y)) 
			  (rightColor (car b))
			  (rightLibs (car (cdr b))))
		     (list rightColor rightLibs)))
		  (t
		   '(NIL NIL)))
	    (cond ((> y 1)
		   (let* (
			  (d (filled x (- y 1)))
			  (downColor (car d))
			  (downLibs (car (cdr d))))
		     (list downColor downLibs)))
		  (t
		   '(NIL NIL)))
	    (cond ((< y 19)				    
		   (let* (
			  (c (filled x (+ y 1))) 
			  (upColor (car c))
			  (upLibs (car (cdr c))))
		       (list upColor upLibs)))
		  (t
		   '(NIL NIL)))))
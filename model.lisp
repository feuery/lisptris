(ql:quickload :fset)
 (ql:quickload :bordeaux-threads)

(defpackage :libtris.model
  (:use :common-lisp)
  (:export :*expected-world-w*
	   :rotate
	   :get-in
           :*expected-world-h*
	   :*world*
           :*current-block*
	   :*current-block-rotation*
	   :*block-x*
	   :merge-block
	   :range
           :*block-y*)
  (:shadowing-import-from :fset :map :map? :seq?))

(in-package :libtris.model)

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(setf *readtable* (fset:fset-setup-readtable *readtable*))

(defun repeat (n val)
  (labels ((do-repeat (val n acc)
	     
	     (if (= n 0)
		 acc
		 (do-repeat val (1- n) (fset:with-first acc val)))))
    (do-repeat val n (fset:empty-seq))))

(defun gen-world (w h)
  (repeat w (repeat h nil)))

(defun gen-block ()
   #[#[nil t]
     #[t t]
   #[nil t]]

  ;; #[#[nil t nil]
  ;;   #[t t t]]
  )

(defun assoc-in (obj ks val)
  ;;(declare (optimize (speed 0) (space 0) (debug 3)))
  (let ((key (car ks))
	(ks (cdr ks)))
    (assert (< key (fset:size obj)))
    (let ((new-obj (if ks
		       (fset:with obj key (assoc-in (fset:@ obj key) ks val))
		       (fset:with obj key val))))
      (assert (= (fset:size obj)
		 (fset:size new-obj)))
      new-obj)))

(defun get-in (o ks)
  (declare (optimize (speed 0) (space 0) (debug 3)))
  (let ((x (car ks))
	(y (cadr ks)))
    (fset:@
     (fset:@ o x)
     y)))

(defun not-collides? (world block block-x block-y)
  ;; (declare (optimize (speed 0) (space 0) (debug 3)))
  ;; (let* ((all-coordinates (apply #'concatenate 'list
  ;; 				 (loop for x from 0 to (1- (fset:size world))
  ;; 				       collect 
  ;; 				       (loop for y from 0 to (1- (fset:size (fset:first world)))
  ;; 					     collect (list x y)))))
  ;; 	 (blocked-block-coordinates (apply 'concatenate 'list
  ;; 					   (loop for x from 0 to (1- (fset:size block))
  ;; 						 collect (loop for y from 0 to (1- (fset:size (fset:first block)))
  ;; 							       if (get-in block (list x y))
  ;; 								 collect (list x y)))))
  ;; 	 (interesting-coordinates

  ;; 	   (remove-if-not (lambda (coordinate)
  ;; 			    (let ((x (first coordinate))
  ;; 				  (y (second coordinate)))
  ;; 			      (member-if (lambda (block-coord)
  ;; 					   (let ((inner-block-x (+ block-x (first block-coord)))
  ;; 						 (inner-block-y (+ block-y (second block-coord))))
  ;; 					     (or (and (= (1- inner-block-x) x)
  ;; 						      (= inner-block-y y))
  ;; 						 (and (= inner-block-x x)
  ;; 						      (= (1- inner-block-y) y)))))
  ;; 					 blocked-block-coordinates)))
  ;; 			  all-coordinates)))

  ;;   result

    t)
    

    

(defun merge-block (world block at-x at-y) ;;at-x, at-y is the coordinate in world space
;;  (declare (optimize (speed 0) (space 0) (debug 3)))
  (let* ((world-w (fset:size world))
	 (world-h (fset:size (fset:first world)))
	 (block-w (fset:size block))
	 (block-h (fset:size (fset:first block)))
	 (max-x (+ at-x block-w))
	 (max-y (+ at-y block-h)))
;;    (format t "block size ~a~%" (list block-w block-h))
    (labels ((make-merge (world x y)  ;; x,y is again in world-space
	       
	       (let ((block-x (- x at-x))
		     (block-y (- y at-y)))
		 (if (= y max-y)
		     world
		     (make-merge (if (and (< x world-w)
					  (< y world-h)
					  (get-in block (list block-x block-y))
					  (not-collides? world block block-x block-y))
				     (assoc-in world (list x y)
					       (get-in block (list block-x block-y)))
				     world)
				 (if (>= (1+ x) max-x)
				     at-x
				     (1+ x))
				 (if (< (1+ x) max-x) 
				     y
				     (1+ y)))))))
      (let ((new-world 
	      (make-merge world at-x at-y)))
	(assert (= (fset:size
		    (fset:convert 'fset:wb-set (fset:image #'fset:size new-world)))
		   1))
	new-world))))

(defun rotate (list-of-lists rotation)
  (let ((transpose-or-mirror (cond ((= rotation 1) 'transpose)
				   ((= rotation 2) 'mirror)
				   ((= rotation 3) 'mirror-and-transpose))))
    (labels ((transpose (list-of-lists)
	       
	       (labels ((convert (type l-of-ls)
			  (fset:convert type (fset:image
					      (lambda (seq)
						(fset:convert type seq))
					      l-of-ls))))
		 (let ((list-of-lists (convert 'list list-of-lists)))
		   (convert 'fset:seq (apply #'mapcar #'list list-of-lists)))))
	     (mirror (list-of-lists)
	       (fset:image #'fset:reverse list-of-lists)))

       (cond ((equalp transpose-or-mirror 'transpose)
	     (transpose list-of-lists))
	    ((equalp transpose-or-mirror 'mirror)
	     (mirror list-of-lists))
	    ((equalp transpose-or-mirror 'mirror-and-transpose)
	     (transpose (mirror list-of-lists)))
	    (t list-of-lists)))))

(defvar *expected-world-w* 10)
(defvar *expected-world-h* 10)

(defvar *world* (gen-world *expected-world-w* *expected-world-h*))
(defvar *current-block* (gen-block))
;; fucking hack, can't be arsed to make block a hashmap with these two fields
(defvar *current-block-rotation* 0)

(defvar *block-x* 0)
(defvar *block-y* 0)

(defun update-world ()
;;    (declare (optimize (speed 0) (space 0) (debug 3)))
  (loop
    (let ((block-height (fset:size (fset:first *current-block*))))
      (if (<= (+ (1+ *block-y*) block-height)
	      (fset:size (fset:first *world*)))
	  (incf *block-y*)
	  (progn
	    (setf *world* (merge-block *world* (rotate *current-block* *current-block-rotation*) *block-x* *block-y*)
		  *block-x* 0
		  *block-y* 0
		  *current-block-rotation 0
		  *current-block* (gen-block))))
      (sleep 1))))

(defun world-updater ()
  ;;bordeaux-thread doesn't seem to get updated defuns
  (update-world))

(defvar *world-updater* (bordeaux-threads:make-thread #'world-updater))

(defun reset ()
  (handler-case (bordeaux-threads:destroy-thread *world-updater*)
    (error (c)))

  (setf *current-block-rotation* 0)
  (setf *world-updater* (bordeaux-threads:make-thread #'world-updater))
  (setf *world* (gen-world *expected-world-w* *expected-world-h*))
  (setf *block-x* 0)
  (setf *block-y* 0))

;;(reset)

(ql:quickload :fset)
 (ql:quickload :bordeaux-threads)

(defpackage :libtris.model
  (:use :common-lisp)
  (:export :*expected-world-w*
	   :get-in
           :*expected-world-h*
	   :*world*
           :*current-block*
	   :*block-x*
	   :merge-block
           :*block-y*)
  (:shadowing-import-from :fset :map :map? :seq?))

(in-package :libtris.model)

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
    #[nil t]])

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
  (let ((x (car ks))
	(y (cadr ks)))
    (fset:@
     (fset:@ o x)
     y)))

(defun merge-block (world block at-x at-y) ;;at-x, at-y is the coordinate in world space
;;  (declare (optimize (speed 0) (space 0) (debug 3)))
  (let* ((world-w (fset:size world))
	 (world-h (fset:size (fset:first world)))
	 (block-w (fset:size block))
	 (block-h (fset:size (fset:first block)))
	 (max-x (+ at-x block-w))
	 (max-y (+ at-y block-h)))
    (labels ((make-merge (world x y)  ;; x,y is again in world-space
	       
	       (let ((block-x (- x at-x))
		     (block-y (- y at-y)))
		 (if (= y max-y)
		     world
		     (make-merge (if (and (< x world-w)
					  (< y world-h))
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


 ;; (merge-block (gen-world 5 10)
;; 	     (gen-block) 1 1)


(defvar *expected-world-w* 10)
(defvar *expected-world-h* 10)

(defvar *world* (gen-world *expected-world-w* *expected-world-h*))
(defvar *current-block* (gen-block))

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
	    (setf *world* (merge-block *world* *current-block* *block-x* *block-y*)
		  *block-x* 0
		  *block-y* 0
		  *current-block* (gen-block))))
      (sleep 1))))

(defun world-updater ()
  ;;bordeaux-thread doesn't seem to get updated defuns
  (update-world))

(defvar *world-updater* (bordeaux-threads:make-thread #'world-updater))

(defun reset ()
  (handler-case (bordeaux-threads:destroy-thread *world-updater*)
    (error (c)))
      
  (setf *world-updater* (bordeaux-threads:make-thread #'world-updater))
  (setf *world* (gen-world *expected-world-w* *expected-world-h*))
  (setf *block-x* 0)
  (setf *block-y* 0))

;;(reset)

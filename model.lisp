;;(ql:quickload :fset)

(defpackage :libtris.model
  (:use :common-lisp)
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
    (if ks
	(fset:with obj key (assoc-in (fset:@ obj key) ks val))
	(fset:with obj key val))))

(defun get-in (o ks)
  (let ((x (car ks))
	(y (cadr ks)))
    (fset:@
     (fset:@ o x)
     y)))

(defun merge-block (world block at-x at-y) ;;at-x, at-y is the coordinate in world space
  (let* ((block-w (fset:size block))
	 (block-h (fset:size (fset:first block)))
	 (max-x (+ at-x block-w))
	 (max-y (+ at-y block-h)))
    (labels ((make-merge (world x y)  ;; x,y is again in world-space
	       (let ((block-x (- x at-x))
		     (block-y (- y at-y)))
		 (if (= y max-y)
		     world
		     (make-merge (assoc-in world (list x y)
					   (get-in block (list block-x block-y)))
				 (if (>= (1+ x) max-x)
				     at-x
				     (1+ x))
				 (if (< (1+ x) max-x) 
				     y
				     (1+ y)))))))
    (make-merge world at-x at-y))))


 ;; (merge-block (gen-world 5 10)
 ;; 	     (gen-block) 1 1)

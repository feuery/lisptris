;; (require 'quicklisp)
 (ql:quickload :sdl2)
 (ql:quickload :fset)

(defpackage :libtris.main 
  (:use :common-lisp
   :sdl2)
  (:import-from :libtris.model :*expected-world-w*
		:rotate
           :*expected-world-h*
	   :get-in
	   :*world*
	   :range
	   :merge-block
           :*current-block*
           :*current-block-rotation*
	   :*block-x*
           :*block-y*))

(in-package :libtris.main)

(defvar *renderer*)
(defvar *window*)
(defvar ctrl-down? nil)
(defvar alt-down? nil)
(defvar shift-down? nil)

(defun handle-kbd (sym)
  (let ((scancode (sdl2:scancode-value sym)))
    (cond ((sdl2:scancode= scancode :scancode-left) (if (> *block-x* 0)
							(decf *block-x*)))
	  ((sdl2:scancode= scancode :scancode-right) (if (< *block-x* (fset:size *world*))
							 (incf *block-x*)))
	  ((sdl2:scancode= scancode :scancode-up) (setf *current-block-rotation* (mod (1+ *current-block-rotation*) 4))))))
	   
	  



(defun handle-windowevent ()
  (let ((flags (sdl2:get-window-flags *window*)))
    ;; haaaack
    (if (or (position :mouse-focus flags)
	    (position :input-focus flags))
	(progn
	  ;;got focus
	  (format t "We have a focus!~%"))
	(progn
	  ;;lost focus
	  (setf ctrl-down? nil
		alt-down? nil
		shift-down? nil)))))

(defun idle (renderer draw-queue)
  (let ((world (merge-block *world* (rotate *current-block* *current-block-rotation*) *block-x* *block-y*)))
    (sdl2:render-clear renderer)

    (let ((blocks-width (fset:size *world*))
	  (blocks-height (fset:size (fset:first *world*))))
      (dolist (x (range blocks-width))
	(dolist (y (range blocks-height))
	  (let ((block (get-in world (list x y))))
	    (if block
		(sdl2:set-render-draw-color renderer 0 0 255 120)
		(sdl2:set-render-draw-color renderer 0 0 255 255))
	    (sdl2:render-fill-rect renderer (sdl2:make-rect (* x 50)
							    (* y 50)
							    (if block
								50
								48)
							    (if block
								50
								48)))))))
    (sdl2:set-render-draw-color renderer 255 0 0 255)
    
    
    (sdl2:render-present renderer)
    (sleep 0.002)))

(defvar +left-mouse-button+ 1)
(defvar +right-mouse-button+ 3)


(defun event-loop (renderer)
   (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
	      (handle-kbd keysym))

    (:keyup (:keysym keysym)
	    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	      (sdl2:push-event :quit)))

    ;; (:mousebuttondown ()
    ;; 		      (start-drag *document*))
    ;; (:mousebuttonup ()
    ;; 		    (stop-drag))

    (:windowevent () 
		  (handle-windowevent))

     (:mousemotion (:x x :y y)
		   (handler-case 
		       (if (or (sdl2:mouse-state-p +left-mouse-button+)
			       (sdl2:mouse-state-p +right-mouse-button+))
			   (format t "dragging, kait ~%"))
		     (error (c)
		       ;;(format t "tool-error: ~a~%" c)
		       )))
    (:idle ()
	   (idle renderer '()))

    (:quit () t)))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Tetris" :flags '(:shown :resizable))
      (let ((renderer (sdl2:create-renderer win)))
	(setf *renderer* renderer)
	(setf *window* win)
	(sdl2:set-render-draw-color renderer 255 0 0 255)
	(sdl2:set-render-draw-blend-mode renderer sdl2-ffi:+SDL-BLENDMODE-BLEND+)

	(event-loop renderer)))))
;;(main)

(loop for x from 0 to 7
      if (oddp x) collect x)



       (apply #'concatenate 'list
	      (loop for x from 0 to 10
		    collect 
		    (loop for y from 0 to 10
			  collect (list x y)))))


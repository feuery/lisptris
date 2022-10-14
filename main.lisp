;; (require 'quicklisp)
;; (ql:quickload :sdl2)
;; (ql:quickload :fset)

(defpackage :libtris.main 
  (:use :common-lisp
   :sdl2))

(in-package :libtris.main)

(defvar *renderer*)
(defvar *window*)
(defvar ctrl-down? nil)
(defvar alt-down? nil)
(defvar shift-down? nil)

(defun handle-kbd (sym) nil)

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
  (sdl2:render-clear renderer)

  ;;(render-scene renderer *document*)
  
  (sdl2:render-present renderer)
  (sleep 0.002)

  ;; (when (equalp app-state :engine)
  ;;   (handle-kbd-event (queues:qpop qmapper.keyboard_loop:kbd-queue)))

  ;; pass lambdas to sdl thread?
  ;; (dolist (cmd *sdl-single-command-queue*)
  ;;   (funcall cmd))
  ;; (setf *sdl-single-command-queue* nil)
  )


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
		       ;;(format t "tool-error: ~a~%" c))))
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

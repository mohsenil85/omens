(in-package :cl-user)
(ql:quickload "cl-charms")
(defpackage omens
  (:use :cl
        :cl-charms)
  (:export :write-at-point
           :with-color
           :clear-screen
           :quit-screen
           :with-init
           :+white+
           :+red+
           :+blue+
           :+green+
           :+cyan+
           :+yellow+
           :+magenta+
           :run-screen
           :defscreen
           :*screen-width*
           :*screen-height*
           :*screens*
           :*running*
           :*width*
           :*height*)) 

(in-package :omens)

(defun color-init ()
  (defconstant +white+ 0)
  (defconstant +red+ 1)
  (defconstant +blue+ 2)
  (defconstant +green+ 3)
  (defconstant +cyan+ 4)
  (defconstant +yellow+ 5)
  (defconstant +magenta+ 6)
  (cl-charms/low-level:start-color)
  (cl-charms/low-level:init-pair +white+ cl-charms/low-level:color_white charms/ll:color_black )
  (cl-charms/low-level:init-pair +red+ cl-charms/low-level:color_red charms/ll:color_black )
  (cl-charms/low-level:init-pair +blue+ cl-charms/low-level:color_blue charms/ll:color_black )
  (cl-charms/low-level:init-pair +green+ cl-charms/low-level:color_green charms/ll:color_black )
  (cl-charms/low-level:init-pair +cyan+ cl-charms/low-level:color_cyan charms/ll:color_black )
  (cl-charms/low-level:init-pair +yellow+ cl-charms/low-level:color_yellow charms/ll:color_black )
  (cl-charms/low-level:init-pair +magenta+ cl-charms/low-level:color_magenta charms/ll:color_black ))

(defparameter *screen-width* 120)
(defparameter *screen-height* 30)

(defparameter *interval* .001)
(defparameter *running* nil)
(defparameter *screens* (make-hash-table))

(defparameter *width*  nil)
(defparameter *height* nil)

(defstruct screen input output next boxed before)

(defmacro with-color (color &body body)
  `(progn
     (cl-charms/low-level:attron (cl-charms/low-level:color-pair ,color))  
     ,@body
     (cl-charms/low-level:attroff (cl-charms/low-level:color-pair ,color))))

(defun write-at-point (s x y &optional (color +white+))
  "write sting or char at point with a color"
  (with-color color
    (with-restored-cursor *standard-window*
      (multiple-value-bind (w h)
        (window-dimensions *standard-window*)
        (write-string-at-point *standard-window* 
                               (format nil "~A" s) 
                               (norm x w)
                               (norm y h ))))))


(defun norm (num limit)
  (mod  num (- limit 1)))

(defmacro grid-each (&body body)
  `(loop :for i :below *width* :do
         (loop :for j :below *height* :do
               ,@body)))

(defun clear-screen ()
  (grid-each 
    (write-at-point #\Space i j)))

(defun quit-screen ()
  (setf *running* nil))


(defun set-width-and-height ()
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
    (setf *width* (- width 2))
    (setf *height* (- height 2))))


(defmacro with-init (&body body)
  `(with-curses ()
     (disable-echoing)
     (cl-charms/low-level:curs-set 0)
     (enable-raw-input :interpret-control-characters t)
     (enable-non-blocking-mode *standard-window*)
     (set-width-and-height)
     (color-init)
     (ensure-screen-size)
     ,@body))

(defmacro defscreen (screen-name &key input output next boxed before)
  `(setf (gethash ',screen-name *screens*) 
         (make-screen 
           :before '(progn ,@before)
           :input 
           '(let  ((c (get-char *standard-window* :ignore-error t)))
              (case c ,@input))
           :output '(progn ,@output)
           :next '(gethash ,next *screens*)
           :boxed ,boxed)))

(defun run-screen (screen)
  (if screen
    (progn (clear-screen)
           (setf *running* t)
           (eval (screen-before screen))
           (loop :while *running*
                 :do
                 (refresh-window *standard-window*)
                 (sleep *interval*  )
                 (if (screen-boxed screen) (draw-box))
                 (eval (screen-input screen ))
                 (eval (screen-output screen )))
           (run-screen (eval (screen-next screen))))
    (sb-sys:os-exit 0)))

(defun draw-box ()
  (loop for i from 1 below *screen-width* do
        (write-at-point #\u2500 i *screen-height*)
        (write-at-point #\u2500 i 0 ))
  (loop for i from 1 below *screen-height* do
        (write-at-point #\u2502 0 i ) 
        (write-at-point #\u2502 *screen-width* i ))
  (write-at-point #\u2514 0 *screen-height* )
  (write-at-point #\u250c 0 0 )
  (write-at-point #\u2510 *screen-width* 0 )
  (write-at-point #\u2518 *screen-width* *screen-height* ))

(defun err-screen ()
  (defscreen err
             :input (((nil) nil)
                     (t (sb-sys:os-exit 2)))
             :output ((write-at-point "Screen to small. Press any key to exit."
                                      0
                                      0 
                                      +red+ ))
             :boxed nil)
  (run-screen (gethash 'err *screens*)))

(defun ensure-screen-size ()
  (multiple-value-bind (width height)
    (window-dimensions *standard-window* )
    (if (or 
          (>= *screen-width* width )
          (>= *screen-height* height ))
      (err-screen))))

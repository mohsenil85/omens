(in-package :cl-user)
(ql:quickload "cl-charms")
(defpackage omens
  (:use :cl
        :cl-charms)
  (:export :write-at-point
           :with-color
           :with-init
           :+red+
           :+blue+
           :+green+
           :+cyan+
           :+yellow+
           :+magenta+ ))

(in-package :omens)


(defconstant +white+ 0)
(defconstant +red+ 1)
(defconstant +blue+ 2)
(defconstant +green+ 3)
(defconstant +cyan+ 4)
(defconstant +yellow+ 5)
(defconstant +magenta+ 6)

(defparameter *screen-width* 120)
(defparameter *screen-height* 30)

(defparameter *width*  nil)
(defparameter *height* nil)

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
      (if (characterp s)
       (write-char-at-point *standard-window* 
                            s
                            (norm x w)
                            (norm y h))
       (write-string-at-point *standard-window* 
                              (format nil "~A" s) 
                              (norm x w)
                              (norm y h) ))))))


(defun norm (num limit)
   (mod  num (- limit 1)))



(defun ensure-screen-size ()
  (multiple-value-bind (width height)
    (window-dimensions *standard-window* )
    (assert 
      (and 
        (<= *screen-width* width )
        (<= *screen-height* height )))))

(defun set-width-and-height ()
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
    (setf *width* (- width 2))
    (setf *height* (- height 2))))


(defmacro with-init (&body body)
  
    `(with-curses ()
;       (ensure-screen-size)
       (disable-echoing)
       (cl-charms/low-level:curs-set 0)
       (enable-raw-input :interpret-control-characters t)
       (enable-non-blocking-mode *standard-window*)
       (set-width-and-height)
       (cl-charms/low-level:start-color)
       (cl-charms/low-level:init-pair +white+ cl-charms/low-level:color_white charms/ll:color_black )
       (cl-charms/low-level:init-pair +red+ cl-charms/low-level:color_green charms/ll:color_black )
       (cl-charms/low-level:init-pair +blue+ cl-charms/low-level:color_green charms/ll:color_black )
       (cl-charms/low-level:init-pair +green+ cl-charms/low-level:color_green charms/ll:color_black )
       (cl-charms/low-level:init-pair +cyan+ cl-charms/low-level:color_green charms/ll:color_black )
       (cl-charms/low-level:init-pair +yellow+ cl-charms/low-level:color_green charms/ll:color_black )
       (cl-charms/low-level:init-pair +magenta+ cl-charms/low-level:color_green charms/ll:color_black )
       ,@body)
  
  )

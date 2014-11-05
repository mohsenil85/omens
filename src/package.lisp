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
           :*height*
           :write-at-center
           )) 

(in-package :omens)

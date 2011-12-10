;;; a little test: import a bunch of svgs,
;;; render them and some text to various formats.

(defpackage #:cl-rsvg-cairo-test
  (:nicknames #:rsvg-cairo-test)
  (:use #:cl #:cairo #:cl-rsvg2)
  (:export #:test-rsvg))

(in-package #:cl-rsvg-cairo-test)

(defparameter *rsvg-directory*
  (namestring (asdf:system-source-directory :cl-rsvg2)))

(defun make-test-filepath (filename)
  (concatenate 'string *rsvg-directory* "tests/" filename))

(defun create-type-context (type out-file width height)
  (case type
    (:pdf (create-pdf-context out-file width height))
    (:svg (create-svg-context out-file width height))))

(defun draw-with-at (filename scale x y)
  (translate x y)
  (scale scale scale)
  (multiple-value-bind (width height)
      (draw-svg-file (make-test-filepath filename))
    (format t "~A size: ~Ax~A~%" filename width height))
  (scale (/ 1 scale) (/ 1 scale))
  (translate (- 0 x) (- 0 y)))

(defun write-at (text size x y)
  (select-font-face "Helvetica" :normal :bold)
  (set-font-size size)
  (set-source-rgb 0d0 0d0 0d0)
  (move-to x y)
  (show-text text))

(defun draw (width height)
  ;; fill bg
  (rectangle 0d0 0d0 width height)
  (set-source-rgb 1d0 1d0 1d0)
  (format t "filling background: ~A~%" 
          (multiple-value-list (fill-extents)))
  (fill-path)

  ;; SVGs
  (draw-with-at "lion.svg" 0.5d0 55d0 65d0)
  (draw-with-at "tux.svg" 0.06d0 350d0 155d0)
  (draw-with-at "tiger.svg" 0.3d0 515d0 55d0)
  (draw-with-at "panel.svg" 1d0 0d0 30d0)
  (draw-with-at "panel.svg" 1d0 250d0 30d0)
  (draw-with-at "panel.svg" 1d0 500d0 30d0)
  
  ;; text
  (write-at "happy animals!" 25d0 10d0 30d0)
  (write-at "i'm a little worried, actually." 12d0 300d0 160d0))

(defun test-rsvg (out-filebase type)
  (let* ((out-file (make-test-filepath out-filebase))
	 (width 750d0)
	 (height 300d0))
    
    (case type
      (:png
       (with-png-file (out-file :argb32 (round width) (round height))
         (draw width height)))
      ((:svg :pdf)
       (with-context ((create-type-context type out-file width height))
         (draw width height)
         (destroy *context*)))
      (otherwise
       (error "~S is not valid type for TEST-RSVG." type)))))

;; (test-rsvg "out.svg" :svg)
;; (test-rsvg "out.png" :png)
;; (test-rsvg "out.pdf" :pdf)

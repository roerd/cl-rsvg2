;;; a little test: import a bunch of svgs,
;;; render them and some text to various formats.

(in-package :cl-rsvg)

(use-package :cairo)


;; NOTE: change this to your source directory
(defparameter *rsvg-directory* "/home/alx/cl/cl-rsvg")

(defun make-test-filepath (filename)
  (concatenate 'string *rsvg-directory* "/tests/" filename))

(defun display-gerror (err)
  (with-foreign-slots ((code message) err gerror)
    (format t "Couldn't load svg: (~A) ~A~%" code message)))

(defun draw-svg-file (filename cairo)
  "Draw a SVG file on a Cairo surface."
  (with-foreign-objects ((err 'gerror)
			 (svg 'handle)
			 (dims 'dimension-data))
    (init)
    (setf svg (handle-new-from-file filename err))
    (cond ((not (null-pointer-p svg))
	   (handle-get-dimensions svg dims)
	   (with-foreign-slots ((width height) dims dimension-data)
	     (format t "~A size: ~Ax~A~%" filename width height)
	     (handle-render-cairo svg cairo)
	     (term)))
	  (t (display-gerror (mem-ref err :pointer))))))

(defun create-surface (type out-file width height)
  (cond ((eq type :pdf)
	 (pdf-surface-create out-file width height))
	((eq type :svg)
	 (svg-surface-create out-file width height))
	((eq type :png)
	 (image-surface-create :argb32 (round width) (round height)))))

(defun draw-with-at (filename scale x y +cr+)
  (translate x y +cr+)
  (scale scale scale +cr+)
  (draw-svg-file (make-test-filepath filename) +cr+)
  (scale (/ 1 scale) (/ 1 scale) +cr+)
  (translate (- 0 x) (- 0 y) +cr+))

(defun write-at (text size x y)
  (select-font-face "Helvetica" :normal :bold)
  (set-font-size size)
  (set-source-rgb 0d0 0d0 0d0)
  (move-to x y)
  (show-text text))

(defun test-rsvg (out-filebase type)
  (let* ((out-file (make-test-filepath out-filebase))
	 (width 750d0)
	 (height 300d0)
	 (surface (create-surface type out-file width height))
	 (+cr+ (create surface)))

    ;; fill bg
    (rectangle 0d0 0d0 width height)
    (set-source-rgb 1d0 1d0 1d0)
    (format t "filling background: ~A~%" 
	    (multiple-value-list (fill-extents)))
    (fill)

    ;; SVGs
    (draw-with-at "lion.svg" 0.5d0 55d0 65d0 +cr+)
    (draw-with-at "tux.svg" 0.06d0 350d0 155d0 +cr+)
    (draw-with-at "tiger.svg" 0.3d0 515d0 55d0 +cr+)
    (draw-with-at "panel.svg" 1d0 0d0 30d0 +cr+)
    (draw-with-at "panel.svg" 1d0 250d0 30d0 +cr+)
    (draw-with-at "panel.svg" 1d0 500d0 30d0 +cr+)
    
    ;; text
    (write-at "happy animals!" 25d0 10d0 30d0)
    (write-at "i'm a little worried, actually." 12d0 300d0 160d0)
    
    ;; save
    (cond ((eq type :pdf) (show-page +cr+))
	  ((eq type :svg) (show-page))
	  ((eq type :png) (surface-write-to-png surface out-file)))
    (destroy)
    (surface-destroy surface)))

;; (test-rsvg "out.svg" :svg)
;; (test-rsvg "out.png" :png)
;; (test-rsvg "out.pdf" :pdf)

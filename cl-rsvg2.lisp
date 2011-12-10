;; This software is Copyright (c) 2007
;; Sasha Kovar <sasha-lisp@arcocene.org>, and
;; 2011 Rörd Hinrichsen <roerdhh@gmail.com>.
;; The copyright holders grant you the rights to
;; distribute and use this software as governed by
;; the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-rsvg2)

;;; the interface: high-level functions and macros building upon the
;;; C functions.

(defmacro with-handle ((var handle) &body body)
  `(with-foreign-object (,var 'handle)
     (%g-type-init)
     (setf ,var ,handle)
     (unless  (null-pointer-p ,var)
       (unwind-protect
            (progn ,@body)
         (g-object-unref ,var)))))

(defun handle-write-data (handle buf count)
  (with-g-error (err)
   (handle-write handle buf count err)))

(defun handle-close* (handle)
  (with-g-error (err)
    (handle-close handle err)))

(defun handle-get-dimension-values (handle)
  (with-foreign-object (dims 'dimension-data)
    (handle-get-dimensions handle dims)
    (with-foreign-slots ((width height em ex) dims dimension-data)
      (values width height em ex))))

(defun handle-get-sub-dimension-values (handle id)
  (with-foreign-object (dims 'dimension-data)
    (handle-get-dimensions-sub handle dims id)
    (with-foreign-slots ((width height em ex) dims dimension-data)
      (values width height em ex))))

(defun handle-get-sub-position-values (handle id)
  (with-foreign-object (pos 'position-data)
    (handle-get-position-sub handle pos id)
    (with-foreign-slots ((x y) pos position-data)
      (values x y))))

(defmacro with-handle-from-data ((handle data data-len) &body body)
  (let ((err (gensym)))
    `(with-handle (,handle (with-g-error (,err)
                             (handle-new-from-data ,data ,data-len ,err)))
       (handle-close* ,handle)
       ,@body)))

(defmacro with-handle-from-file ((handle filespec) &body body)
  (let ((err (gensym)))
    `(with-handle (,handle (with-g-error (,err)
                             (handle-new-from-file (namestring ,filespec)
                                                   ,err)))
       (handle-close* ,handle)
       ,@body)))

(defun draw-svg (svg &optional (context *context*))
  (multiple-value-bind (width height)
      (handle-get-dimension-values svg)
    (handle-render-cairo svg (get-pointer context))
    (values width height)))

(defun draw-svg-sub (svg id &optional (context *context*))
  (multiple-value-bind (width height)
      (handle-get-dimension-values svg)
    (handle-render-cairo-sub svg (get-pointer context) id)
    (values width height)))

(defun draw-svg-data (data data-len &optional (context *context*))
  "Draw SVG data on a Cairo surface. DATA needs to be an uint8 C array,
   and DATA-LEN its length, in bytes. Return the SVGs width and height."
  (with-handle-from-data (svg data data-len)
    (draw-svg svg context)))

(defun draw-svg-file (filespec &optional (context *context*))
  "Draw a SVG file on a Cairo surface. Return its width and height."
  (with-handle-from-file (svg filespec)
    (draw-svg svg context)))

;;; a gray-stream class for writing to a RsvgHandle

(defclass handle-data-stream
    (fundamental-binary-output-stream trivial-gray-stream-mixin)
  ((handle :reader handle :initarg :handle)))

(defmethod stream-element-type ((stream handle-data-stream))
  '(unsigned-byte 8))

(defmethod close ((stream handle-data-stream) &key abort)
  (declare (ignore abort))
  (handle-close* (handle stream)))

(defmethod stream-write-sequence ((stream handle-data-stream)
                                  sequence start end &key)
  (cffi-sys:with-pointer-to-vector-data
      (buf (coerce (subseq sequence start end)
                   '(vector (unsigned-byte 8))))
    (handle-write-data (handle stream) buf (- end start))))

(defmethod stream-write-byte ((stream handle-data-stream) integer)
  (with-foreign-object (buf :uint8)
    (setf (mem-ref buf :uint8) integer)
    (handle-write-data (handle stream) buf 1)))

(defmacro with-handle-data-stream ((stream) &body body)
  (let ((handle (gensym)))
    `(with-handle (,handle (handle-new))
       (let ((,stream (make-instance 'handle-data-stream
                                     :handle ,handle)))
         ,@body))))

;; This software is Copyright (c) 2007
;; Sasha Kovar <sasha-lisp@arcocene.org>, and
;; 2011 Rörd Hinrichsen <roerdhh@gmail.com>.
;; The copyright holders grant you the rights to
;; distribute and use this software as governed by
;; the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-rsvg2)

(define-foreign-library librsvg-2
  (:unix (:or "librsvg-2.so.2" "librsvg-2.so"))
  (t (:default "librsvg-2")))

(use-foreign-library librsvg-2)

;;; RsvgHandle

(defctype handle :pointer)

(defctype gsize :uint)

(defcstruct dimension-data
  (width :int)
  (height :int)
  (em :double)
  (ex :double))

(defcstruct position-data
  (x :int)
  (y :int))

(defcfun ("rsvg_set_default_dpi" set-default-dpi) :void
  (dpi :double))

(defcfun ("rsvg_set_default_dpi_x_y" set-default-dpi-x-y) :void
  (dpi-x :double)
  (dpi-y :double))

(defcfun ("rsvg_handle_set_dpi" handle-set-dpi) :void
  (handle handle)
  (dpi :double))

(defcfun ("rsvg_handle_set_dpi_x_y" handle-set-dpi-x-y) :void
  (handle handle)
  (dpi-x :double)
  (dpi-y :double))

(defcfun ("rsvg_handle_new" handle-new) handle)

(defcfun ("rsvg_handle_write" handle-write) :boolean
  (handle handle)
  (buf (:pointer :uchar))
  (count gsize)
  (error (:pointer :pointer)))

(defcfun ("rsvg_handle_close" handle-close) :boolean
  (handle handle)
  (error (:pointer :pointer)))

(defcfun ("rsvg_handle_get_base_uri" handle-get-base-uri) :string
  (handle handle))

(defcfun ("rsvg_handle_set_base_uri" handle-set-base-uri) :void
  (handle handle)
  (base-uri :string))

(defcfun ("rsvg_handle_get_dimensions" handle-get-dimensions) :void
  (handle handle)
  (dimension-data dimension-data))

(defcfun ("rsvg_handle_get_dimensions_sub" handle-get-dimensions-sub) :void
  (handle handle)
  (dimension-data dimension-data)
  (id :string))

(defcfun ("rsvg_handle_get_position_sub" handle-get-position-sub) :void
  (handle handle)
  (position-data position-data)
  (id :string))

(defcfun ("rsvg_handle_has_sub" handle-has-sub) :boolean
  (handle handle)
  (id :string))

(defcfun ("rsvg_handle_get_title" handle-get-title) :string
  (handle handle))

(defcfun ("rsvg_handle_get_desc" handle-get-desc) :string
  (handle handle))

(defcfun ("rsvg_handle_get_metadata" handle-get-metadata) :string
  (handle handle))

(defcfun ("rsvg_handle_new_from_data" handle-new-from-data) handle
  (data (:pointer :uint8))
  (data_len gsize)
  (error (:pointer :pointer)))

(defcfun ("rsvg_handle_new_from_file" handle-new-from-file) handle
  (file_name :string)
  (error (:pointer :pointer)))

;;; Using RSVG with cairo

(defcfun ("rsvg_handle_render_cairo" handle-render-cairo) :void
  (handle handle)
  (cr :pointer))

(defcfun ("rsvg_handle_render_cairo_sub" handle-render-cairo-sub) :void
  (handle handle)
  (cr :pointer)
  (id :string))

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
       (with-g-error (,err)
         (handle-close ,handle ,err))
       ,@body)))

(defmacro with-handle-from-file ((handle filename) &body body)
  (let ((err (gensym)))
    `(with-handle (,handle (with-g-error (,err)
                             (handle-new-from-file ,filename ,err)))
       (with-g-error (,err)
         (handle-close ,handle ,err))
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

(defun draw-svg-file (filename &optional (context *context*))
  "Draw a SVG file on a Cairo surface. Return its width and height."
  (with-handle-from-file (svg filename)
    (draw-svg svg context)))

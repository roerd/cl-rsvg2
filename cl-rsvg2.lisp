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

;; should the impossible happen...
;; (defcenum rsvg-error
;;   :RSVG_ERROR_FAILED)

;; (defcstruct RsvgHandleClass
;;      (parent :pointer)
;;      (_abi_padding :pointer))

;; could maybe just use typedef :pointer
(defcstruct handle
  (parent %g-object)
  (priv :pointer)
  (_abi_padding :pointer))

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
  (handle :pointer)
  (dpi :double))

(defcfun ("rsvg_handle_set_dpi_x_y" handle-set-dpi-x-y) :void
  (handle :pointer)
  (dpi-x :double)
  (dpi-y :double))

(defcfun ("rsvg_handle_new" handle-new) :pointer)

(defcfun ("rsvg_handle_close" handle-close) :boolean
  (handle :pointer)
  (error :pointer))

(defcfun ("rsvg_handle_get_base_uri" handle-get-base-uri) :string
  (handle :pointer))

(defcfun ("rsvg_handle_set_base_uri" handle-set-base-uri) :void
  (handle :pointer)
  (base-uri :string))

(defcfun ("rsvg_handle_get_dimensions" handle-get-dimensions) :void
  (handle :pointer)
  (dimension-data :pointer))

(defcfun ("rsvg_handle_get_dimensions_sub" handle-get-dimensions-sub) :void
  (handle :pointer)
  (dimension-data :pointer)
  (id :string))

(defcfun ("rsvg_handle_get_position_sub" handle-get-position-sub) :void
  (handle :pointer)
  (position-data :pointer)
  (id :string))

(defcfun ("rsvg_handle_has_sub" handle-has-sub) :boolean
  (handle :pointer)
  (id :string))

(defcfun ("rsvg_handle_get_title" handle-get-title) :string
  (handle :pointer))

(defcfun ("rsvg_handle_get_desc" handle-get-desc) :string
  (handle :pointer))

(defcfun ("rsvg_handle_get_metadata" handle-get-metadata) :string
  (handle :pointer))

(defcfun ("rsvg_handle_new_from_file" handle-new-from-file) :pointer
  (file_name :string)
  (error :pointer))

;;; Using RSVG with cairo

(defcfun ("rsvg_handle_render_cairo" handle-render-cairo) :void
  (handle :pointer)
  (cr :pointer))

(defcfun ("rsvg_handle_render_cairo_sub" handle-render-cairo-sub) :void
  (handle :pointer)
  (cr :pointer)
  (id :string))

;;; the interface: moved here from cairo-test.lisp

(defmacro with-handle ((var handle) &body body)
  `(with-foreign-object (,var 'handle)
     (%g-type-init)
     (setf ,var ,handle)
     (unless  (null-pointer-p ,var)
       (unwind-protect
            (progn
              (with-g-error (err)
                (handle-close ,var err))
              ,@body)
         (g-object-unref ,var)))))

(defun make-handle-from-file (filename)
  (handler-case
      (with-g-error (err)
        (handle-new-from-file filename err))
    (g-error-condition (e) (warn e))))

(defun draw-svg-file (filename &optional (context *context*))
  "Draw a SVG file on a Cairo surface."
  (with-foreign-object (dims 'dimension-data)
    (with-handle (svg (make-handle-from-file filename))
      (handle-get-dimensions svg dims)
      (with-foreign-slots ((width height) dims dimension-data)
        (format t "~A size: ~Ax~A~%" filename width height)
        (handle-render-cairo svg (get-pointer context))))))

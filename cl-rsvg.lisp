;; This software is Copyright (c) 2007
;; Sasha Kovar <sasha-lisp@arcocene.org>.
;; Sasha grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-rsvg)


(define-foreign-library librsvg-2
  (:unix (:or "librsvg-2.so.2" "librsvg-2.so"))
  (t (:default "librsvg-2")))
   
(use-foreign-library librsvg-2)


;; some things from gnome
(defctype gquark :uint32)

(defcstruct gerror
  (domain gquark)
  (code :int)
  (message :string))

(defcstruct gtypeinstance
  (g_class :pointer))

(defcstruct gobject
  (gtypeinstance gtypeinstance)
  ;; private below
  (ref_count :uint)
  (qdata :pointer))

;; should the impossible happen...
;; (defcenum rsvg-error
;;   :RSVG_ERROR_FAILED)

;; (defcstruct RsvgHandleClass
;; 	(parent :pointer)
;; 	(_abi_padding :pointer))

;; could maybe just use typedef :pointer
(defcstruct handle
  (parent gobject)
  (priv :pointer)
  (_abi_padding :pointer))

(defcstruct dimension-data
  (width :int)
  (height :int)
  (em :double)
  (ex :double))

(defcfun ("rsvg_init" init) :void)

(defcfun ("rsvg_term" term) :void)

;; (defcfun ("rsvg_set_default_dpi" set-default-dpi) :void
;;   (dpi :double))

;; (defcfun ("rsvg_set_default_dpi_x_y" set-default-dpi-x-y) :void
;;   (dpi-x :double)
;;   (dpi-y :double))

;; (defcfun ("rsvg_handle_set_dpi" handle-set-dpi) :void
;;   (handle :pointer)
;;   (dpi :double))

;; (defcfun ("rsvg_handle_set_dpi_x_y" handle-set-dpi-x-y) :void
;;   (handle :pointer)
;;   (dpi-x :double)
;;   (dpi-y :double))

(defcfun ("rsvg_handle_get_dimensions" handle-get-dimensions) :void
  (handle :pointer)
  (dimension-data :pointer))

(defcfun ("rsvg_handle_new_from_file" handle-new-from-file) :pointer
  (file_name :string)
  (error :pointer))

(defcfun ("rsvg_handle_close" handle-close) :boolean
  (handle :pointer)
  (error :pointer))

(defcfun ("rsvg_handle_render_cairo" handle-render-cairo) :void
  (handle :pointer)
  (cr :pointer))

(defcfun ("rsvg_handle_render_cairo_sub" handle-render-cairo-sub) :void
  (handle :pointer)
  (cr :pointer)
  (id :string))


;; TODO g_object_unref() for closing a handle

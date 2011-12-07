;; This software is Copyright (c) 2011
;; Rörd Hinrichsen <roerdhh@gmail.com>.
;; Rörd grants you the rights to distribute
;; and use this software as governed by the
;; terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-rsvg2)

;;; Using RSVG with GdkPixbuf

(defcfun ("rsvg_handle_get_pixbuf" handle-get-pixbuf)
    (gobject:g-object pixbuf)
  (handle :pointer))

(defcfun ("rsvg_handle_get_pixbuf_sub" handle-get-pixbuf-sub)
    (gobject:g-object pixbuf)
  (handle :pointer)
  (id :string))

;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;
;; This software is Copyright (c) Rörd Hinrichsen, 2011.
;; Rörd grants you the rights to distribute and use
;; this software as governed by the terms of the
;; Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

(asdf:defsystem cl-rsvg2-pixbuf
  :description "Using the RSVG Library with GdkPixbuf."
  :version "0.2"
  :author "Rörd Hinrichsen <roerdhh@gmail.com>"
  :licence "LLGPL"
  :serial t
  :components ((:file "rsvg-pixbuf"))
  :depends-on (#:cl-rsvg2 #:cl-gtk2-gdk))

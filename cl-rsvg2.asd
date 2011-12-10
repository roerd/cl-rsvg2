;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;
;; This software is Copyright (c) Sasha Kovar, 2007,
;; and Rörd Hinrichsen, 2011.
;; The copyright holders grant you the rights to
;; distribute and use this software as governed by
;; the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

(asdf:defsystem cl-rsvg2
  :description "Bindings for RSVG Library."
  :version "0.4.1"
  :author "Sasha Kovar <sasha-rsvg@arcocene.org>"
  :maintainer "Rörd Hinrichsen <roerdhh@gmail.com>"
  :licence "LLGPL"
  :serial t
  :components ((:file "package")
               (:file "cl-rsvg2-ffi")
	       (:file "cl-rsvg2"))
  :depends-on (#:cffi #:cl-cairo2 #:cl-gtk2-glib #:trivial-gray-streams))

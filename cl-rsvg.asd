;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;
;; This software is Copyright (c) Sasha Kovar, 2007.
;; Sasha grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

(asdf:defsystem cl-rsvg
  :description "Bindings for librsvg."
  :version "0.9.1"
  :author "Sasha Kovar <sasha-rsvg@arcocene.org>"
  :licence "LLGPL"
  :serial t
  :components ((:file "package")
	       (:file "cl-rsvg"))
  :depends-on (#:cffi #:cl-cairo))

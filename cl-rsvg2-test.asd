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

(asdf:defsystem cl-rsvg2-test
  :description "Eos tests for cl-rsvg2."
  :version "0.4.1"
  :author "Rörd Hinrichsen <roerdhh@gmail.com>"
  :licence "LLGPL"
  :serial t
  :components ((:static-file "tests/tux.svg")
               (:file "tests/rsvg-test"))
  :depends-on (#:asdf #:cffi #:cl-rsvg2 #:eos))

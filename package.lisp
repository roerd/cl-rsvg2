;; Sasha Kovar <sasha-lisp@arcocene.org>, and
;; 2011 Rörd Hinrichsen <roerdhh@gmail.com>.
;; The copyright holders grant you the rights to
;; distribute and use this software as governed by
;; the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.


(defpackage #:cl-rsvg2
  (:nicknames #:rsvg2)
  (:use #:cl #:cffi #:gobject.ffi #:glib)
  (:import-from #:cl-cairo2 #:*context* #:get-pointer)
  (:export #:draw-svg-file))

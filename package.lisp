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
  (:export #:set-default-dpi       #:set-default-dpi-x-y
           #:handle-set-dpi        #:handle-set-dpi-x-y
           #:handle-new            #:handle-close
           #:handle-get-base-uri   #:handle-set-base-uri
           #:handle-has-sub        #:handle-get-title
           #:handle-get-desc       #:handle-get-metadata
           #:with-handle           #:handle-write-data
           #:handle-get-dimension-values
           #:handle-get-sub-dimension-values
           #:handle-get-sub-position-values
           #:with-handle-from-data #:with-handle-from-file
           #:draw-svg-data         #:draw-svg-file))

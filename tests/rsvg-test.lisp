(defpackage #:cl-rsvg2-test
  (:nicknames #:rsvg2-test)
  (:use #:cl #:cffi #:rsvg2 #:eos)
  (:export #:run!))

(in-package #:cl-rsvg2-test)

(def-suite cl-rsvg2)

(in-suite cl-rsvg2)

(defparameter *tux-file*
  (asdf:component-pathname
   (asdf:find-component :cl-rsvg2-test "tests/tux.svg")))

(test handle-has-sub
  (is-true (with-handle-from-file (svg *tux-file*)
             (handle-has-sub svg "#g3")))
  (is-false (with-handle-from-file (svg *tux-file*)
              (handle-has-sub svg "#foo"))))

(test handle-get-dimension-values
  (is (equal '(744 1052 744d0 1052d0)
             (multiple-value-list
              (with-handle-from-file (svg *tux-file*)
                (handle-get-dimension-values svg))))))

(test handle-get-sub-dimension-values
  (is (equal '(744 1052 744d0 1052d0)
             (multiple-value-list
              (with-handle-from-file (svg *tux-file*)
                (handle-get-sub-dimension-values svg (null-pointer))))))
  (is (equal '(593 704 593d0 704d0)
             (multiple-value-list
              (with-handle-from-file (svg *tux-file*)
                (handle-get-sub-dimension-values svg "#g3"))))))

(test handle-get-sub-position-values
  (is (equal '(0 0)
             (multiple-value-list
              (with-handle-from-file (svg *tux-file*)
                (handle-get-sub-position-values svg (null-pointer))))))
  (is (equal '(75 173)
             (multiple-value-list
              (with-handle-from-file (svg *tux-file*)
                (handle-get-sub-position-values svg "#g3"))))))

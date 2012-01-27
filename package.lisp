;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-90
  (:use)
  (:export :make-table))

(defpackage :srfi-90.internal
  (:use :srfi-90 :cl :fiveam :srfi-89)
  (:shadow :lambda))


;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-90"
  (:use)
  (:export make-table)
  (:size 1))


(defpackage "https://github.com/g000001/srfi-90#internals"
  (:use
   "https://github.com/g000001/srfi-90"
   "https://github.com/g000001/srfi-89"
   "https://github.com/g000001/srfi-69"
   cl 
   fiveam)
  (:shadow lambda)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-69"
   make-hash-table
   hash-table-size)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-69#internals"
   *default-table-size*
   hash-table-entries))

;;; *EOF*

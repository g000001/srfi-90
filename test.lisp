(cl:in-package :srfi-90.internal)

(def-suite srfi-90)

(in-suite srfi-90)

(test make-table
  (is (eq #'srfi-69:hash
          (srfi-69:hash-table-hash-function (make-table))))
  (is (eq #'sxhash
          (srfi-69:hash-table-hash-function (make-table :hash #'sxhash))))
  (is (eq #'equal?
          (srfi-69:hash-table-equivalence-function (make-table))))
  (is (eq #'=
          (srfi-69:hash-table-equivalence-function (make-table :test #'=))))
  (is (eq #'=
          (srfi-69:hash-table-equivalence-function (make-table :hash #'values :test #'=))))
  (is (= 0
         (srfi-69:hash-table-size (make-table :hash #'values :test #'=))))
  ;; :size /= default size?
  (is (= 100
         (let ((tab (make-table :hash #'values :test #'= :size 100)))
           (length (srfi-69.internal::hash-table-entries tab))))))

;;; eof
(cl:in-package "https://github.com/g000001/srfi-90#internals")


(def-suite* srfi-90)


(test make-table
  (is (eq #'hash
          (hash-table-hash-function (make-table))))
  (is (eq #'sxhash
          (hash-table-hash-function (make-table :hash #'sxhash))))
  (is (eq #'equal?
          (hash-table-equivalence-function (make-table))))
  (is (eq #'=
          (hash-table-equivalence-function (make-table :test #'=))))
  (is (eq #'=
          (hash-table-equivalence-function (make-table :hash #'values :test #'=))))
  (is (= 0
         (hash-table-size (make-table :hash #'values :test #'=))))
  ;; :size /= default size?
  (is (= 100
         (let ((tab (make-table :hash #'values :test #'= :size 100)))
           (length (hash-table-entries tab))))))


;;; *EOF*

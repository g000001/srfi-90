;;;; srfi-90.lisp
(cl:in-package "https://github.com/g000001/srfi-90#internals")


(define-function make-table
  (lambda* ((:test        test        #'equal?)
            (:hash        hash        #'hash)
            (:size        size        *default-table-size*)
            (:min-load    min-load    0)
            (:max-load    max-load    1)
            (:weak-keys   weak-keys   nil)
            (:weak-values weak-values nil))
    (declare (ignore min-load max-load weak-keys weak-values))
    (make-hash-table test hash size)))


;;; *EOF*

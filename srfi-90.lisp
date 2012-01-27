;;;; srfi-90.lisp
(cl:in-package :srfi-90.internal)

(define-function make-table
  (lambda* ((:test        test        #'equal?)
            (:hash        hash        #'srfi-69:hash)
            (:size        size        0)
            (:min-load    min-load    0)
            (:max-load    max-load    1)
            (:weak-keys   weak-keys   nil)
            (:weak-values weak-values nil))
    (declare (ignore min-load max-load weak-keys weak-values))
    (srfi-69:make-hash-table test hash size)))

;;; eof

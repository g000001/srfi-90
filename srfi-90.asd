;;;; srfi-90.asd -*- lisp -*-

(cl:in-package :asdf)

(defsystem :srfi-90
  :serial t
  :depends-on (:fiveam :srfi-69 :srfi-89)
  :components ((:file "package")
	       (:file "util")
               (:file "srfi-90")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-90))))
  (load-system :srfi-90)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-90.internal :srfi-90))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))


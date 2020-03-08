;;;; srfi-90.asd -*- lisp -*-

(cl:in-package :asdf)


(defsystem :srfi-90
  :version "20200309"
  :description "SRFI 90 for CL: Extensible hash table constructor"
  :long-description "SRFI 90 for CL: Extensible hash table constructor
https://srfi.schemers.org/srfi-90"
  :author "Marc Feeley"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam :srfi-69 :srfi-89)
  :components ((:file "package")
	       (:file "util")
               (:file "srfi-90")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-90))))
  (let ((name "https://github.com/g000001/srfi-90")
        (nickname :srfi-90))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-90))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-90#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-90)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*


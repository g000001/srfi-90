(cl:in-package "https://github.com/g000001/srfi-90#internals")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (setf (fdefinition 'eq?) #'eq)
    (setf (fdefinition 'integer?) #'integerp)
    (setf (fdefinition 'list?) #'listp)
    (setf (fdefinition 'negative?) #'minusp)
    (setf (fdefinition 'null?) #'null)
    (setf (fdefinition 'pair?) #'consp)
    (setf (fdefinition 'positive?) #'plusp)
    (setf (fdefinition 'zero?) #'zerop)
    (setf (fdefinition 'vector-length) #'length)
    (setf (fdefinition 'vector?) #'vectorp)
    (setf (fdefinition 'procedure?) #'functionp)
    (setf (fdefinition 'exact?) #'rationalp)
    (setf (fdefinition 'even?) #'evenp)
    (setf (fdefinition 'real?) #'realp)
    (setf (fdefinition 'newline) #'terpri)
    (setf (fdefinition 'display) #'princ)
    (setf (fdefinition 'remainder)  #'rem)
    (setf (fdefinition 'string-length)  #'length)
    (setf (fdefinition 'char->integer)  #'char-code)
    (setf (fdefinition 'string-ref) #'char)
    (setf (fdefinition 'symbol->string) #'string)
    (setf (fdefinition 'string?) #'stringp)
    (setf (fdefinition 'symbol?) #'symbolp)
    (setf (fdefinition 'number?) #'numberp)
    (setf (fdefinition 'char?) #'characterp)
    (setf (fdefinition 'real-part) #'realpart)
    (setf (fdefinition 'imag-part) #'imagpart)
    (setf (fdefinition 'string=?) #'string=)
    (setf (fdefinition 'string-ci=?) #'string-equal)
    ))


(defmacro set! (var val)
  `(setq ,var ,val))


(declaim (inline list-tail vector-set! list-ref vector->list list->vector
                 quotient set-car! set-cdr! eqv? equal?
                 assq assv #|assoc|# for-each memq))


(defun memq (item list)
  (member item list :test #'eq?))


(defun for-each (fn &rest lists)
  (apply #'mapc fn lists)
  nil)


(defun assq (item alist)
  (cl:assoc item alist :test #'eq?))


(defun assv (item alist)
  (cl:assoc item alist :test #'eqv?))


#|(defun assoc (item alist)
  (cl:assoc item alist :test #'equal?))|#


(defun equal? (x y)
  (equal x y))


(defun set-car! (list obj)
  (rplaca list obj))


(defun set-cdr! (cons x)
  (rplacd cons x))


(defun quotient (x y)
  (values (truncate x y)))


(defun list-tail (list k)
  (nthcdr k list))


(defun list-ref (list k)
  (nth k list))


(defun vector-set! (vec index val)
  (setf (aref vec index) val))


(defun vector->list (vec)
  (coerce vec 'list))


(defun list->vector (list)
  (coerce list 'vector))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (typecase list
      (list (if (tailp () list)
                list
              (cl:let ((last (last list)))
                `(,@(butlast list)
                  ,(car last)
                  cl:&rest
                  ,(cdr last)))))
      (symbol `(cl:&rest ,list)))))


(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))


(defmacro letrec ((&rest binds) &body body)
  `(let (,@(mapcar (cl:lambda (x)
                     `(,(car x) #'values))
             binds))
     (declare (optimize (space 3)))
     (labels (,@(remove nil
                  (mapcar (cl:lambda (x &aux (name (car x)))
                            `(,name
                               (&rest args)
                               (apply ,name args)))
                          binds)))
       (declare (optimize (debug 0) (space 3)))
       (psetq ,@(apply #'append binds))
       ,@body)))


(defmacro define-function (name-args &body body)
  (if (consp name-args)
      (destructuring-bind (name . args)
                          name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (setf (fdefinition ',name-args)
               ,(car body)))))


(declaim (inline vector-ref))


(defun vector-ref (vec k)
  (svref vec k))


(declaim (inline modulo))


(defun modulo (x y)
  (mod x y))


(defmacro begin (&body body)
  `(progn ,@body))


(declaim (inline make-vector))


(defun make-vector (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))


(declaim (inline string-append))


(defun string-append (&rest strings)
  (format nil "~{~A~}" strings))


(declaim (inline number->string))


(defun number->string (num)
  (write-to-string num))


(defmacro dolex ((&rest varlist) endlist &body body)
  (let* ((vars (mapcar (lambda (v)
                         (if (consp v) (car v) v) )
                       varlist ))
         (binds (mapcar (lambda (b)
                          (if (consp b)
                              (destructuring-bind (var &optional init next)
                                                  b
                                (if next
                                    `(,var ,init
                                           (let (,@(mapcar (lambda (x)
                                                             (list x x) )
                                                     vars ))
                                             (declare (ignorable ,@vars))
                                             ,next ))
                                    `(,var ,init)))
                              (list b nil) ))
                        varlist )))
    `(cl:do ,binds ,endlist ,@body) ))


(defmacro with-local-define-function (&body defines-body)
  (or (member :in defines-body) (error "no body"))
  (let* ((body-pos (position :in defines-body))
         (defines  (subseq defines-body 0 body-pos))
         (body     (subseq defines-body (1+ body-pos))))
  (cl:loop
     :for (nil name-arg . bo) :in defines
     :collect (let ((name-arg (to-proper-lambda-list name-arg)))
                `(,(car name-arg) ,(cdr name-arg) ,@bo))
     :into defs
     :finally (return
                `(labels (,@defs)
                   ,@body)))))


(defmacro with-local-define-variable (&body defines-body)
  (or (member :in defines-body) (error "no body"))
  (let* ((body-pos (position :in defines-body))
         (defines  (subseq defines-body 0 body-pos))
         (body     (subseq defines-body (1+ body-pos))))
  (cl:loop
     :for (nil v bo) :in defines
     :collect v :into vars
     :collect v :into setqs
     :collect bo :into setqs
     :finally (return
                `(cl:let (,@vars)
                   (psetq ,@setqs)
                   ,@body)))))


;;; *EOF*

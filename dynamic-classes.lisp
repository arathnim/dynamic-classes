(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))

(ql:quickload '(alexandria iterate anaphora destructuring-match) :silent t)

(defpackage dynamic-classes
 (:shadow defclass make-instance)
 (:use cl alexandria iterate anaphora destructuring-match)
 (:export defclass make-instance))

(in-package dynamic-classes)

(defun as-keyword (symbol)
   "Returns a keyword with the same name as symbol"
   (intern (symbol-name symbol) :keyword))

(defun expand-slot (name value &rest more-keys)
   `(,name :accessor ,name :initarg ,(as-keyword name) :initform ,value ,@more-keys))

(defun expand-method (name args &rest body)
   `(cl:defmethod ,name ,args ,@body))

(defmacro defclass (name superclass &rest attributes)
   (let ((slots nil) (methods nil))
      (iter (for x in attributes)
         (when (listp x)
            (if (or (eql (length x) 2) (and (eql (length x) 3) (keywordp (nth 3 x))))
                (push (apply #'expand-slot x) slots)
                (push (apply #'expand-method x) methods))))
      `(progn 
         (cl:defclass ,name ,superclass (,@slots))
         ,@methods)))

(defvar *gensym* (make-hash-table :test #'equalp))
(defun gen-sym (&optional (prefix "gs"))
   (setf (gethash prefix *gensym*) (+ 1 (gethash prefix *gensym* 0)))
   (intern (string-upcase (format nil "~a~a" prefix (gethash prefix *gensym*)))))

;; guess I'll just do this manually
;; TODO: symbol/list parser combinators
;; ahahaha, generator magic
(defmacro make-instance (class &rest args)
   (let ((keyargs nil) (defs nil) (gs (gen-sym "dynamic-class"))) 
      (iter (generating x in args)
            (if (keywordp x)
                (progn (push (list x (next x)) keyargs) (next x))
                (progn (when x (push x defs)) (next x))))
      (if defs
         `(progn (defclass ,gs (,class) ,@defs) (cl:make-instance ',gs ,@(flatten keyargs)))
         `(cl:make-instance ',class ,@(flatten keyargs)))))

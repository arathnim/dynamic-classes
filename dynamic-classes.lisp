(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))

;; TODO

(ql:quickload '(alexandria iterate) :silent t)

(defpackage dynamic-classes
 (:shadow defclass make-instance)
 (:use cl alexandria iterate)
 (:export defclass make-instance))

(in-package dynamic-classes)

(defvar *class-name* nil)

(defun symbol-eq (x y)
   "Compares the names of symbols, to prevent package issues"
   (and (symbolp x) (symbolp y) (string= (string x) (string y))))

(defun as-keyword (symbol)
   "Returns a keyword with the same name as symbol"
   (intern (symbol-name symbol) :keyword))

(defun transform-arguments (args)
   (iter (for x in args)
         (collect
            (if (symbol-eq x '*this*)
               `(,(intern "THIS" *package*) ,*class-name*)
                x))))

(defun expand-slot (name value &rest more-keys)
   `(,name :accessor ,name :initarg ,(as-keyword name) :initform ,value ,@more-keys))

(defun expand-method (name args &rest body)
   `(cl:defmethod ,name ,(transform-arguments args) ,@body))

(defun expand-special-method (special name args &rest body)
   `(cl:defmethod ,name ,special ,(transform-arguments args) ,@body))

(defmacro defclass (name superclass &rest attributes)
   (let ((slots nil) (methods nil) (*class-name* name))
      (iter (for x in attributes)
         (when (listp x)
            (cond ((or (eql (length x) 2) (and (eql (length x) 3) (keywordp (nth 3 x))))
                     (push (apply #'expand-slot x) slots))
                  ((keywordp (first x))
                     (push (apply #'expand-special-method x) methods))
                  ((> (length x) 2)
                     (push (apply #'expand-method x) methods))
                  (t (error "whoops!")))))
      `(progn
         (cl:defclass ,name ,superclass (,@slots))
         ,@methods)))

(defvar *gensym* (make-hash-table :test #'equalp))
(defun gen-sym (&optional (prefix "gs"))
   (setf (gethash prefix *gensym*) (+ 1 (gethash prefix *gensym* 0)))
   (intern (string-upcase (format nil "~a~a" prefix (gethash prefix *gensym*)))))

(defun extract-keyargs (args)
   (let ((keyargs nil) (other-things nil))
      (iter (generating x in args)
            (if (keywordp x)
                (progn (push (list x (next x)) keyargs) (next x))
                (progn (when x (push x other-things)) (next x))))
      (list keyargs other-things)))

(defun unpair (list)
   (iter (for x in list) (collect (first x)) (collect (second x))))

;; guess I'll just do this manually
;; TODO: symbol/list parser combinators
;; ahahaha, generator magic
(defmacro make-instance (class &rest args)
   (let* ((pargs (extract-keyargs args))
          (keyargs (first pargs)) (defs (second pargs)) 
          (gs (gen-sym "dynamic-class")))
      (if defs
         `(progn (defclass ,gs (,class) ,@defs) (cl:make-instance ',gs ,@(unpair keyargs)))
         `(cl:make-instance ',class ,@(unpair keyargs)))))

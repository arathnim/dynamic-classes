(asdf:defsystem "dynamic-classes"
   :serial t
   :version "0.1"
   :author "Dylan Ball <arathnim@gmail.com>"
   :maintainer "Dylan Ball <arathnim@gmail.com>"
   :description "better defclass and make-instance"
   :depends-on (alexandria iterate)
   :components ((:file "dynamic-classes")))

(in-package #:cl-user)

(asdf:defsystem #:stleem
  :description "stleem is a Lisp version of the 'streem' language ( https://github.com/matz/streem )."
  :license "MIT License"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:bordeaux-threads #:lparallel)
  :components ((:file "package")
	       (:file "pipe")
	       (:file "main-loop")
	       (:file "drivers")
	       (:file "example")))

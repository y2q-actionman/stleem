(in-package #:cl-user)

(asdf:defsystem #:stleem
  :description "stleem is a Lisp version of the 'streem' language ( https://github.com/matz/streem )."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:bordeaux-threads #:lparallel)
  :components ((:file "package")
	       (:file "pipe" :depends-on ("package"))
	       (:file "main-loop" :depends-on ("pipe"))
	       (:file "drivers" :depends-on ("main-loop"))
	       (:file "example" :depends-on ("main-loop"))))

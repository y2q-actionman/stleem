(in-package #:cl-user)

(defpackage #:stleem
  (:use #:cl #:bordeaux-threads #:lparallel.queue)
  (:export #:stleem
	   #:seq #:stdin #:stdout))

(defpackage #:stleem-example
  (:use #:cl #:stleem)
  (:export #:cat #:copy-with-cat #:fizzbuzz))
  

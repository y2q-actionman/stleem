(in-package :stleem)

;;; Drivers
(define-filter stdout (obj)
  (format t "~&~A~%" obj)
  obj)

(define-filter stdin (_)
  (declare (ignore _))
  (read-line *standard-input* nil nil))

#+ignore
(defun seq (n-max)
  (let ((n 0))
    #'(lambda (_)
	(declare (ignore _))
	(if (< n n-max)
	    (incf n)
	    nil))))

(defun seq (n-max)
  (loop for n from 0 to n-max collect n))

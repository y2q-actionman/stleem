(in-package :stleem)

;;; Drivers
(define-filter (stdout &optional (out *standard-output*)) (obj)
  (format out "~&~A~%" obj)
  obj)

(define-filter (stdin &optional (in *standard-input*)) (_)
  (declare (ignore _))
  (read-line in nil nil))

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

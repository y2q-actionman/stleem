(in-package :stleem)

;;; Drivers
(defun stleem-write-driver (stream)
  #'(lambda (obj)
      (format stream "~&~A~%" obj)
      obj))

(defun stleem-read-line-driver (stream)
  #'(lambda (_)
      (declare (ignore _))
      (read-line stream nil nil)))

(defun seq (n-max)
  (let ((n 0))
    #'(lambda (_)
	(declare (ignore _))
	(if (< n n-max)
	    (incf n)
	    nil))))

(define-symbol-macro stdin
    (stleem-read-line-driver *standard-input*))

(define-symbol-macro stdout
    (stleem-write-driver *standard-output*))

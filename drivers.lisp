(in-package :stleem)

;;; Drivers
(define-filter (stdout &optional (out *standard-output*)) (obj)
  (format out "~&~A~%" obj)
  obj)

(define-generator (stdin &optional (in *standard-input*)) ()
  (let ((l (read-line in nil :eof)))
    (when (eq l :eof)
      (return))
    l))
	
(define-generator (seq n-max &aux (n 0)) ()
  (if (< n n-max)
      (incf n)
      (return)))

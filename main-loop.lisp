(in-package :stleem)

(defun make-stleem-thread-function (pipeline-func from-pipe to-pipe end-symbol)
  #'(lambda ()
      (unwind-protect
	   (loop for in = (pipe-pop from-pipe end-symbol)
	      until (eq in end-symbol)
	      as out = (funcall pipeline-func in)
	      until (eq out end-symbol)
	      do (pipe-push to-pipe out))
	(pipe-close to-pipe))))

;;; Main loop
(defun run-stleem (pipeline-start pipeline-funcs
		   &key (start-symbol t) (end-symbol nil) (extract-values t))
  (let ((threads nil)
	(first-pipe
	 (etypecase pipeline-start
	   (sequence
	    (make-instance 'sequence-pipe :sequence pipeline-start))
	   (function
	    (push pipeline-start pipeline-funcs)
	    (make-instance 'constant-pipe :value start-symbol))))
	(last-pipe nil))
    (unwind-protect
	 (loop for pipeline-func in pipeline-funcs
	    as from-pipe = first-pipe then to-pipe
	    as to-pipe = (make-instance 'queue-pipe)
	    do (push (make-thread
		      (make-stleem-thread-function
		       pipeline-func from-pipe to-pipe end-symbol)
		      :name "streem worker thread")
		     threads)
	    finally
	      (setf last-pipe to-pipe)
	      (mapc #'join-thread threads))
      (mapc #'destroy-thread threads))
    (cond ((null last-pipe)
	   (values nil nil))
	  (extract-values
	   (values (pipe-extract last-pipe) t))
	  (t
	   (values last-pipe t)))))

;;; Entry point
(defmacro stleem ((&key (start-symbol t) (end-symbol nil) (extract-values t))
		  &body pipelines)
  `(run-stleem
    ,(first pipelines)
    (list ,@(rest pipelines))
    :start-symbol ,start-symbol
    :end-symbol ,end-symbol
    :extract-values ,extract-values))

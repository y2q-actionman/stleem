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
(defun run-stleem (pipeline-funcs
		   &key (start-symbol t) (end-symbol nil) (extract-values t))
  (loop for pipeline-func in pipeline-funcs
     as from-pipe = (make-instance 'constant-pipe :value start-symbol)
     then to-pipe
     as to-pipe = (make-instance 'tekitou-pipe)
     collect (make-thread
	      (make-stleem-thread-function
	       pipeline-func from-pipe to-pipe end-symbol)
	      :name "streem worker thread")
     into threads
     finally
       (mapc #'join-thread threads)
       (return
	 (if extract-values
	     (pipe-extract to-pipe)
	     to-pipe))))

;;; Entry point
(defmacro stleem ((&key (start-symbol t) (end-symbol nil) (extract-values t))
		  &body pipelines)
  `(run-stleem
    (list ,@pipelines)
    :start-symbol ,start-symbol
    :end-symbol ,end-symbol
    :extract-values ,extract-values))

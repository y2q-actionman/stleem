(in-package :stleem)

#+ignore
(defun make-stleem-thread-function (pipeline-func from-pipe to-pipe end-symbol)
  #'(lambda ()
      (unwind-protect
	   (loop until (pipe-closed-p from-pipe)
	      for in = (pipe-pop from-pipe end-symbol)
	      as out = (funcall pipeline-func in)
	      until (eq out end-symbol)
	      do (pipe-push to-pipe out))
	(pipe-close to-pipe))))

(defmacro filter-lambda-function ((&rest args) (filter-arg) &body body)
  (let ((from-pipe (gensym "from-pipe"))
	(to-pipe (gensym "to-pipe"))
	(pipe-loop-next (gensym "pipe-loop-next")))
    `#'(lambda (,@args)
	 #'(lambda (,from-pipe ,to-pipe)
	     #'(lambda ()
		 (unwind-protect
		      (prog ()
			 ,pipe-loop-next
			 (flet ((skip ()
				  (go ,pipe-loop-next))
				(emit (x)
				  (pipe-push ,to-pipe x)))
			   (declare (ignorable (function skip)
					       (function emit)))
			   (when (pipe-closed-p ,from-pipe)
			     (return))
			   (emit ((lambda (,filter-arg) ,@body)
				  (pipe-pop ,from-pipe))))
			 (go ,pipe-loop-next))
		   (pipe-close ,to-pipe)))))))

(defmacro define-filter (func-spec (filter-arg) &body body)
  (etypecase func-spec
    (list
     (let ((name (first func-spec))
	   (args (rest func-spec)))
       `(setf (fdefinition ',name)
	      (filter-lambda-function (,@args) (,filter-arg) ,@body))))
    (symbol
     `(setf (fdefinition ',func-spec)
	    (filter-lambda-function nil (,filter-arg) ,@body)))))

;;; Main loop
(defun run-stleem (pipeline-start pipeline-funcs
		   &key (start-symbol t) (extract-values t))
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
		      (funcall pipeline-func
			       from-pipe to-pipe)
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
(defmacro stleem ((&key (start-symbol t) (extract-values t))
		  &body pipelines)
  (flet ((trans-pipe-filter (filter) 	; should be rewrited//
	   (cond ((symbolp filter)
		  `(,filter))
		 ((and (listp filter)
		       (eq (first filter) 'lambda))
		  `(funcall (filter-lambda-function nil ,@(rest filter))))
		 (t
		  filter))))
    `(run-stleem
      ,(trans-pipe-filter (first pipelines))
      (list ,@(mapcar #'trans-pipe-filter (rest pipelines)))
    :start-symbol ,start-symbol
    :extract-values ,extract-values)))

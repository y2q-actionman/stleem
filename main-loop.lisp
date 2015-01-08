(in-package :stleem)

(defvar *from-pipe*)
(defvar *to-pipe*)

(defmacro filter-lambda-function ((&rest extra-parameters) (filter-arg)
				  &body body)
  (let ((pipe-loop-next (gensym "pipe-loop-next")))
    `#'(lambda (,@extra-parameters)
	 #'(lambda ()
	     (unwind-protect
		  (prog ()
		     ,pipe-loop-next
		     (flet ((skip ()
			      (go ,pipe-loop-next))
			    (emit (x)
			      (pipe-push *to-pipe* x)))
		       (declare (ignorable (function skip)
					   (function emit)))
		       (when (pipe-closed-p *from-pipe*)
			 (return))
		       (emit ((lambda (,filter-arg) ,@body)
			      (pipe-pop *from-pipe*))))
		     (go ,pipe-loop-next))
	       (pipe-close *to-pipe*))))))

(defmacro define-filter* ((func-name &rest extra-parameters)
			  (filter-arg) &body body)
  `(setf (fdefinition ',func-name)
	 (filter-lambda-function (,@extra-parameters) (,filter-arg) ,@body)))

(defmacro define-filter (func-spec (filter-arg) &body body)
  (etypecase func-spec
    (list
     (let ((name (first func-spec))
	   (args (rest func-spec)))
       `(define-filter* (,name ,@args) (,filter-arg) ,@body)))
    (symbol
     `(define-filter* (,func-spec) (,filter-arg) ,@body))))

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
		      (funcall pipeline-func)
		      :name "streem worker thread"
		      :initial-bindings
		      `((*from-pipe* (lambda () ,from-pipe))
			(*to-pipe* (lambda () ,to-pipe))))
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
		  `#',filter)
		 ((and (listp filter)
		       (eq (first filter) 'lambda))
		  `(filter-lambda-function () ,@(rest filter)))
		 (t
		  filter))))
    `(run-stleem
      ,(trans-pipe-filter (first pipelines))
      (list ,@(mapcar #'trans-pipe-filter (rest pipelines)))
    :start-symbol ,start-symbol
    :extract-values ,extract-values)))

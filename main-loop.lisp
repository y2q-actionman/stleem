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
     `(define-filter* (,@func-spec) (,filter-arg) ,@body))
    (symbol
     `(define-filter* (,func-spec) (,filter-arg) ,@body))))

(defmacro generator-lambda-function ((&rest extra-parameters) ()
				     &body body)
  ;; TODO: merge with 'filter-lambda-function'
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
		       (emit ((lambda () ,@body))))
		     (go ,pipe-loop-next))
	       (pipe-close *to-pipe*))))))

(defmacro define-generator* ((func-name &rest extra-parameters)
			     () &body body)
  `(setf (fdefinition ',func-name)
	 (generator-lambda-function (,@extra-parameters) () ,@body)))

(defmacro define-generator (func-spec () &body body)
  (etypecase func-spec
    (list
     `(define-generator* (,@func-spec) () ,@body))
    (symbol
     `(define-generator* (,func-spec) () ,@body))))

;;; Main loop
(defun run-pipeline-threads (first-pipe pipeline-funcs)
  (let ((threads nil))
    (unwind-protect
	 (loop for pipeline-func in pipeline-funcs
	    as from-pipe = first-pipe then to-pipe
	    as to-pipe = (make-instance 'queue-pipe)
	    do (push (make-thread pipeline-func
				  :name "streem worker thread"
				  :initial-bindings
				  `((*from-pipe* (lambda () ,from-pipe))
				    (*to-pipe* (lambda () ,to-pipe))))
		     threads)
	    finally
	      (mapc #'join-thread threads)
	      (return (or to-pipe first-pipe)))
      (mapc #'destroy-thread threads))))

(defun return-from-pipe (last-pipe extract-values)
  (cond ((null last-pipe)
	 (values nil nil))
	(extract-values
	 (values (pipe-extract last-pipe) t))
	(t
	 (values last-pipe t))))

(defgeneric run-stleem
    (pipeline-start pipeline-funcs &key (extract-values t)))

(defmethod run-stleem (pipeline-start pipeline-funcs
		       &key (extract-values t))
  (let* ((first-pipe (make-instance 'constant-pipe :value pipeline-start))
	 (last-pipe (run-pipeline-threads first-pipe pipeline-funcs)))
    (return-from-pipe last-pipe extract-values)))
  
(defmethod run-stleem ((pipeline-start sequence) pipeline-funcs
		       &key (extract-values t))
  (let* ((first-pipe (make-instance 'queue-pipe :sequence pipeline-start))
	 (last-pipe (run-pipeline-threads first-pipe pipeline-funcs)))
    (return-from-pipe last-pipe extract-values)))
  
(defmethod run-stleem ((pipeline-start function) pipeline-funcs
		       &key (extract-values t))
  ;; now, only for 'not forking' case..
  (let ((generator-output-pipe (make-instance 'queue-pipe))
	(generator-thread nil)
	(last-pipe nil))
    (unwind-protect
	 (progn
	   (setf generator-thread
		 (make-thread pipeline-start
			      :name "stleem generator thread"
			      :initial-bindings
			      `((*to-pipe* (lambda () ,generator-output-pipe)))))
	   (setf last-pipe
		 (run-pipeline-threads generator-output-pipe
				       pipeline-funcs))
	   (join-thread generator-thread))
      (destroy-thread generator-thread))
    (return-from-pipe last-pipe extract-values)))

;;; Entry point
(defmacro stleem ((&key (extract-values t))
		  pipeline-start
		  &body pipeline-filters)
  `(run-stleem
    ,(cond ((symbolp pipeline-start)
	    `(,pipeline-start))
	   (t
	    pipeline-start))
    (list ,@(mapcar #'(lambda (filter)
			(cond ((symbolp filter)
			       `(,filter))
			      ((and (listp filter)
				    (eq (first filter) 'lambda))
			       `(funcall
				 (filter-lambda-function () ,@(rest filter))))
			      (t
			       filter)))
		    pipeline-filters))
    :extract-values ,extract-values))

(in-package :stleem)

;;; Pipe
(defclass pipe ()
  ())

(defgeneric pipe-close (pipe))

(defgeneric pipe-closed-p (pipe))

(defgeneric pipe-push (pipe item))

(defgeneric pipe-pop (pipe &optional eof-value))

(defgeneric pipe-extract (pipe)
  (:method (pipe)
    (loop until (pipe-closed-p pipe)
       collect (pipe-pop pipe))))
    
;;; a pipe made by a queue.
(defclass queue-pipe (pipe)
  ((queue :initarg :queue :initform (make-queue) :accessor queue-pipe-queue)))

(defmethod pipe-close ((pipe queue-pipe))
  (push-queue :queue-pipe-closed
	      (queue-pipe-queue pipe)))

(defmethod pipe-closed-p ((pipe queue-pipe))
  (eq (peek-queue (queue-pipe-queue pipe))
      :queue-pipe-closed))

(defmethod pipe-push ((pipe queue-pipe) item)
  (unless (pipe-closed-p pipe)
    (push-queue item (queue-pipe-queue pipe))))

(defmethod pipe-pop ((pipe queue-pipe) &optional (eof-value nil))
  (if (pipe-closed-p pipe)
      eof-value
      (pop-queue (queue-pipe-queue pipe))))

;; for mapping sequence to queue-pipe
(defmethod initialize-instance ((instance queue-pipe)
				&rest args
				&key (sequence nil)
				&allow-other-keys)
  (if sequence
      (apply #'call-next-method
	     instance
	     :queue (make-queue :initial-contents sequence)
	     args)
      (call-next-method)))

(defmethod initialize-instance :after ((instance queue-pipe)
				       &rest args
				       &key (allow-append nil)
				       &allow-other-keys)
  (unless allow-append
    (pipe-close instance)))

;;; Constant-pipe (used for initial stream)
(defclass constant-pipe (pipe)
  ((value :initarg :value :initform t :accessor constant-pipe-value)
   (close-marker :initform nil :accessor constant-pipe-close-marker)))

(defmethod pipe-close ((cpipe constant-pipe))
    (setf (constant-pipe-close-marker cpipe) t))

(defmethod pipe-closed-p ((cpipe constant-pipe))
  (constant-pipe-close-marker cpipe))

(defmethod pipe-push ((cpipe constant-pipe) item)
  (declare (ignore item))
  nil)

(defmethod pipe-pop ((cpipe constant-pipe) &optional eof-value)
  (declare (ignore eof-value))
  (constant-pipe-value cpipe))

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
(defclass tekitou-pipe (pipe)
  ((queue :initform (make-queue) :accessor tekitou-pipe-queue)))

(defmethod pipe-close ((pipe tekitou-pipe))
  (push-queue :tekitou-pipe-closed
	      (tekitou-pipe-queue pipe)))

(defmethod pipe-closed-p ((pipe tekitou-pipe))
  (eq (peek-queue (tekitou-pipe-queue pipe))
      :tekitou-pipe-closed))

(defmethod pipe-push ((pipe tekitou-pipe) item)
  (unless (pipe-closed-p pipe)
    (push-queue item (tekitou-pipe-queue pipe))))

(defmethod pipe-pop ((pipe tekitou-pipe) &optional (eof-value nil))
  (if (pipe-closed-p pipe)
      eof-value
      (pop-queue (tekitou-pipe-queue pipe))))

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

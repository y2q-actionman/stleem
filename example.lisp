(in-package :stleem-example)

;;; Examples
(defun cat ()
  (stleem ()
    stdin
    stdout))

(defun copy-with-cat (from to)
  (with-open-file (istream from :direction :input)
    (with-open-file (ostream to :direction :output
			     :if-exists :supersede)
      (let ((*standard-input* istream)
	    (*standard-output* ostream))
	(cat)))))

(defun hello ()
  (stleem ()
    '("Hello, World")
    stdout))

(defun fizzbuzz ()
  (stleem ()
    (seq 100)
    (lambda (x)
      (cond ((= (mod x 15) 0)
	     "FizzBuzz")
	    ((= (mod x 3) 0)
	     "Fizz")
	    ((= (mod x 5) 0)
	     "Buzz")
	    (t
	     x)))
    stdout))

;; from 04emit.strm
(defun repeat-twice ()
  (stleem ()
    (seq 100)
    (lambda (x)
      (emit x)
      x)
    stdout))

;; from 04skip.strm
(defun pick-even-numbers ()
  (stleem ()
    (seq 100)
    (lambda (x)
      (when (= (mod x 2) 1)
	(skip))
      x)
    stdout))
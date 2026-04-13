;;; FIXME: This test suite is not currently portable, since the
;;; SRFI doesn't take a position on whether write-array prints both
;;; bounds or just the upper bounds when the lower bound is zero.
(define-library (tests write-array)
  (export run-tests write-array-to-string)
  (import (scheme base)
          (srfi 64)
          (srfi 231)
          (srfi 268 write)
          )
  (begin
    (define (write-array-to-string array)
      (call-with-port
       (open-output-string)
       (lambda (port)
	 (write-array array port)
	 (get-output-string port))))

    (define (run-tests)
      (test-group "write-array"
	(test-equal "1d generic array (1)"
	  "#a((0 3)) (0 0 0)"
	  (write-array-to-string (list*->array 1 '(0 0 0))))

	(test-equal "1d generic array (2)"
	  "#a((0 3)) (1 2 3)"
	  (write-array-to-string (list*->array 1 '(1 2 3))))

	(test-equal "1d u8 array (1)"
	  "#au8((0 3)) (0 0 0)"
	  (write-array-to-string
	   (list*->array 1
			 '(0 0 0)
			 u8-storage-class)))

	(test-equal "1d u8 array (2)"
	  "#au8((0 3)) (1 2 3)"
	  (write-array-to-string
           (list*->array 1
			 '(1 2 3)
			 u8-storage-class)))

	(test-equal "2d generic array (1)"
	  "#a((0 2) (0 2)) ((a b) (c d))"
	  (write-array-to-string
	   (list*->array 2 '((a b) (c d)))))

	(test-equal "2d generic array (2)"
	  "#a((2 4) (2 4)) ((a b) (c d))"
	  (write-array-to-string
	   (list->array (make-interval '#(2 2) '#(4 4))
			'(a b c d))))
      ))
  ))

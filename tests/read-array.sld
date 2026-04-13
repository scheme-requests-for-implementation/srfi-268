(define-library (tests read-array)
  (export run-tests)
  (import (scheme base)
          (srfi 64)
          (srfi 231)
          (srfi 268 read)
	  (tests util)
          )
  (begin
    (define (string->array s)
      (call-with-port (open-input-string s) read-array))

    (define (run-tests)
      (test-group "read-array"
	(test-assert "1d generic array"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a(3) (1 2 3)")))
	(test-assert "1d generic array, both bounds given"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a((0 3)) (1 2 3)")))
	(test-assert "1d generic array, uppercase tag"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#A(3) (1 2 3)")))
	(test-assert "1d generic array, whitespace variation (1)"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a (3) (1 2 3)")))
	(test-assert "1d generic array, whitespace variation (2)"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a(3)(1 2 3)")))
	(test-assert "1d u16 array"
	  (array=? (list*->array 1 '(1 2 3) u16-storage-class)
		   (string->array "#au16(3) (1 2 3)")))
	(test-assert "1d u16 array, both bounds given, whitespace var."
	  (array=? (list*->array 1 '(1 2 3) u16-storage-class)
		   (string->array "#au16\t((0 3))\t(1 2 3)")))
	(test-assert "1d generic array, nonzero lower bound"
	  (array=? (list->array (make-interval '#(4) '#(7))
				'(1 2 3))
		   (string->array "#a((4 7)) (1 2 3)")))
	(test-assert "2d generic array"
	  (array=? (list*->array 2 '((a b) (c d)))
		   (string->array "#A(2 2) ((a b) (c d))")))
	(test-assert "2d generic array, mixed bound types"
	  (array=? (list*->array 2 '((a b) (c d)))
		   (string->array "#a((0 2) 2) ((a b) (c d))")))
	))
    ))

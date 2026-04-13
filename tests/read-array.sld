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
	(test-assert "1d generic array, uppercase tag"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#A(3) (1 2 3)")))
	(test-assert "1d generic array, whitespace variation (1)"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a (3) (1 2 3)")))
	(test-assert "1d generic array, whitespace variation (2)"
	  (array=? (list*->array 1 '(1 2 3))
		   (string->array "#a(3)(1 2 3)")))
	))
    ))

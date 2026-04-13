(import (scheme base)
	(srfi 64)
	(prefix (tests write-array) w:)
	)

(test-group "srfi-268"
  (w:run-tests))

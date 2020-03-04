
(in-package #:darts.lib.calendar-test)

(defun run-all-tests ()
  (local-date-representations-suite)
  (local-time-representations-suite)
  (local-timestamp-representations-suite))

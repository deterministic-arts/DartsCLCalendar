
(defpackage #:darts.lib.calendar-local-time
  (:use #:common-lisp #:darts.lib.calendar #:bordeaux-threads)
  (:local-nicknames (#:lt #:local-time) (#:cal #:darts.lib.calendar))
  (:export #:instant-to-local-time))

(in-package #:darts.lib.calendar-local-time)

(defun instant-to-local-time (ts)
  (lt:unix-to-timestamp (cal:instant-to-posix-time ts) :nsec (instant-nanos ts)))

(defmethod local-timestamp ((object lt:timestamp) &key (zone *zone*))
  (local-timestamp (instant object) :zone zone))

(defmethod local-date ((object lt:timestamp) &key (zone *zone*))
  (local-date (instant object) :zone zone))

(defmethod local-time ((object lt:timestamp) &key (zone *zone*))
  (local-time (instant object) :zone zone))

(defmethod instant ((object lt:timestamp) &key)
  (cal:posix-time-to-instant (lt:timestamp-to-unix object) (lt:nsec-of object)))

(defgeneric timezone-of (object))

(defclass interop-zone (cal:zone)
  ())

(defclass local-time-zone (interop-zone)
  ((name :initarg :name :reader name-of)
   (timezone :initarg :timezone :reader timezone-of)))

(defclass local-time-default-zone (interop-zone)
  ())

(defvar *timezones-loaded* nil)
(defvar *load-lock* (make-lock))

(defun ensure-timezones-loaded ()
  (with-lock-held (*load-lock*)
    (unless *timezones-loaded*
      (setf *timezones-loaded* t)
      (lt:reread-timezone-repository))))

(define-zone-constructor :local-time (region-name)
  (ensure-timezones-loaded)
  (make-instance 'local-time-zone
                 :name region-name
                 :timezone (lt:find-timezone-by-location-name region-name)))

(define-zone-constructor :local-time-default ()
  (make-instance 'local-time-default-zone))

(defmethod timezone-of ((object local-time-default-zone))
  lt:*default-timezone*)

(defmethod zone-identifier ((object local-time-zone))
  `(:local-time ,(name-of object)))

(defmethod zone-identifier ((object local-time-default-zone))
  `(:local-time-default))

(defmethod compute-zone-offset ((object instant) (zone interop-zone))
  (let ((ts (instant-to-local-time object)))
    (nth-value 0 (lt:timestamp-subtimezone ts (timezone-of zone)))))

(defconstant +local-posix-days+ 11017)

(defun compute-local-offset (object zone)
  (let* ((epoch (+ cal::+posix-time-offset+
                   (cal:encode-epoch-seconds (local-year object) (local-month object) (local-day object)
                                             (local-hour object) (local-minute object) (local-second object)))))
    (multiple-value-bind (days seconds) (floor epoch cal::+seconds-per-day+)
      (lt::%guess-offset seconds (- days +local-posix-days+)
                         (timezone-of zone)))))
                                          
(defmethod compute-zone-offset ((object local-timestamp) (zone interop-zone))
  (compute-zone-offset object zone))

(defmethod compute-zone-offset ((object local-date) (zone interop-zone))
  (compute-zone-offset object zone))

(defmethod compute-zone-offset ((object local-time) (zone interop-zone))
  (compute-zone-offset object zone))

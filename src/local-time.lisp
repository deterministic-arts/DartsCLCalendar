
(defpackage #:darts.lib.calendar-local-time
  (:use #:common-lisp #:darts.lib.calendar #:bordeaux-threads)
  (:local-nicknames (#:lt #:local-time) (#:cal #:darts.lib.calendar))
  (:export #:instant-to-local-time))

(in-package #:darts.lib.calendar-local-time)

;;; The nice thing here is, that local-time and darts.lib.calendar
;;; use the same epoch, i.e., 2000-03-01T00:00:00Z This makes the
;;; conversions almost trivial.

(defun instant-to-local-time (ts)
  (multiple-value-bind (date time) (floor (instant-seconds ts) cal::+seconds-per-day+)
    (lt:make-timestamp :day date :sec time :nsec (instant-nanos ts))))

(defmethod local-timestamp ((object lt:timestamp) &key (zone *zone*))
  (local-timestamp (instant object) :zone zone))

(defmethod local-date ((object lt:timestamp) &key (zone *zone*))
  (local-date (instant object) :zone zone))

(defmethod local-time ((object lt:timestamp) &key (zone *zone*))
  (local-time (instant object) :zone zone))

(defmethod instant ((object lt:timestamp) &key)
  (make-instant (+ (* cal::+seconds-per-day+ (lt:day-of object)) (lt:sec-of object))
                (lt:nsec-of object)))

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

(defun compute-local-offset (object zone)
  (cal::with-local-fields (year month day hour minute second) object
    (let ((epoch (cal:encode-epoch-seconds year month day hour minute second))) 
      (multiple-value-bind (days seconds) (floor epoch cal::+seconds-per-day+)
        (lt::%guess-offset seconds days (timezone-of zone))))))
                                          
(defmethod compute-zone-offset ((object local-timestamp) (zone interop-zone))
  (compute-zone-offset object zone))

(defmethod compute-zone-offset ((object local-date) (zone interop-zone))
  (compute-zone-offset object zone))

(defmethod compute-zone-offset ((object local-time) (zone interop-zone))
  (compute-zone-offset object zone))


;;;
;;; This implementation of region time zones is not, what we actually
;;; want later on. But it is enough to get us started in the direction
;;; of a usable date/time library...
;;;
;;; This code must be removed as soon as proper time zone support lands
;;; in the calendar library.
;;;

(defun clean-region-name (primary seconary)
  (labels
      ((clean (str)
         (if (stringp str) str
             (string-capitalize (substitute #\_ #\- (string str))))))
    (concatenate 'string (clean primary) "/" (clean seconary))))

(defclass region-zone (interop-zone)
  ((timezone :initarg :timezone :reader timezone-of)
   (expression :initarg :expression :reader zone-identifier)))

(define-zone-constructor :region (primary secondary)
  (let ((name (clean-region-name primary secondary)))
    (ensure-timezones-loaded)
    (let ((zone (lt:find-timezone-by-location-name name)))
      (make-instance 'region-zone
                     :expression `(:region ,primary ,secondary)
                     :timezone zone))))

(defmethod print-object ((object region-zone) stream)
  (print-unreadable-object (object stream :type t)
    (princ (zone-identifier object) stream))
  object)

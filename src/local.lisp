#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Date and time library
  Copyright (c) 2020 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package #:darts.lib.calendar)

(defstruct (local-date (:copier nil)
                       (:predicate local-date-p)
                       (:conc-name local-date-)
                       (:constructor make-local-date-1 (year month day &optional (%dow +lazyday+))))
  (year (required-argument) :type (integer #.+min-local-year+ #.+max-local-year+) :read-only t)
  (month (required-argument) :type (integer 1 12) :read-only t)
  (day (required-argument) :type (integer 1 31) :read-only t)
  (%dow (required-argument) :type (integer 0 7)))

(defstruct (local-time (:copier nil)
                       (:predicate local-time-p)
                       (:conc-name local-time-)
                       (:constructor make-local-time-1 (hour minute second &optional (nanos 0))))
  (hour (required-argument) :type (integer 0 23) :read-only t)
  (minute (required-argument) :type (integer 0 59) :read-only t)
  (second (required-argument) :type (integer 0 59) :read-only t)
  (nanos (required-argument) :type nanos :read-only t))

(defstruct (local-timestamp (:copier nil)
                            (:predicate local-timestamp-p)
                            (:conc-name local-timestamp-)
                            (:constructor make-local-timestamp-1 (date time)))
  (date (required-argument) :type local-date :read-only t)
  (time (required-argument) :type local-time :read-only t))


(defvar +min-local-date+ (make-local-date-1 +min-local-year+ 1 1))
(defvar +max-local-date+ (make-local-date-1 +max-local-year+ 12 31))
(defvar +min-local-time+ (make-local-time-1 0 0 0 0))
(defvar +max-local-time+ (make-local-time-1 23 59 59 999999999))
(defvar +midnight+ +min-local-time+)
(defvar +noon+ (make-local-time-1 12 0 0 0))
(defvar +min-local-timestamp+ (make-local-timestamp-1 +min-local-date+ +min-local-time+))
(defvar +max-local-timestamp+ (make-local-timestamp-1 +max-local-date+ +max-local-time+))



(defun local-date-weekday (object)
  (let ((value (local-date-%dow object)))
    (if (not (eql value +lazyday+)) value
        (setf (local-date-%dow object)
              (compute-weekday (local-date-year object)
                               (local-date-month object)
                               (local-date-day object))))))

(defun local-time-millisecond (object)
  (nth-value 0 (floor (local-time-nanos object) 1000000)))

(defun local-time-microsecond (object)
  (mod (floor (local-time-nanos object) 1000) 1000))

(defun local-time-nanosecond (object)
  (mod (local-time-nanos object) 1000))



(defmethod local-year ((object local-date))
  (local-date-year object))

(defun local-year-of-era (object)
  (let ((year (local-year object)))
    (if (plusp year) year
        (+ (- year) 1))))

(defun local-era (object)
  (if (plusp (local-year object)) 1 0))

(defmethod local-month ((object local-date))
  (local-date-month object))

(defmethod local-day ((object local-date))
  (local-date-day object))

(defmethod local-weekday ((object local-date))
  (local-date-weekday object))

(defmethod local-hour ((object local-date)) 0)
(defmethod local-minute ((object local-date)) 0)
(defmethod local-second ((object local-date)) 0)
(defmethod local-nanos ((object local-date)) 0)

(defmethod local-hour ((object local-time))
  (local-time-hour object))

(defmethod local-minute ((object local-time))
  (local-time-minute object))

(defmethod local-second ((object local-time))
  (local-time-second object))

(defmethod local-nanos ((object local-time))
  (local-time-nanos object))

(defmethod local-year ((object local-time)) 2000)
(defmethod local-month ((object local-time)) 3)
(defmethod local-day ((object local-time)) 1)
(defmethod local-weekday ((object local-time)) +wednesday+)

(defmethod local-year ((object local-timestamp))
  (local-date-year (local-timestamp-date object)))

(defmethod local-month ((object local-timestamp))
  (local-date-month (local-timestamp-date object)))

(defmethod local-day ((object local-timestamp))
  (local-date-day (local-timestamp-date object)))

(defmethod local-weekday ((object local-timestamp))
  (local-date-weekday (local-timestamp-date object)))

(defmethod local-hour ((object local-timestamp))
  (local-time-hour (local-timestamp-time object)))

(defmethod local-minute ((object local-timestamp))
  (local-time-minute (local-timestamp-time object)))

(defmethod local-second ((object local-timestamp))
  (local-time-second (local-timestamp-time object)))

(defmethod local-nanos ((object local-timestamp))
  (local-time-nanos (local-timestamp-time object)))

(defun local-millisecond (object)
  (nth-value 0 (floor (local-nanos object) 1000000)))

(defun local-microsecond (object)
  (mod (floor (local-nanos object) 1000) 1000))

(defun local-nanosecond (object)
  (mod (local-nanos object) 1000))



(defun local-date= (d1 d2)
  (or (eq d1 d2)
      (and (eql (local-date-year d1) (local-date-year d2))
           (eql (local-date-month d1) (local-date-month d2))
           (eql (local-date-day d1) (local-date-day d2)))))

(define-lessp local-date<
  local-date-year local-date-month local-date-day)

(derive-ordering local-date)

(defun local-date-hash (object)
  (sxhash (+ (* 10000 (local-date-year object)) (* 100 (local-date-month object)) (local-date-day object))))

(defun local-time= (d1 d2)
  (or (eq d1 d2)
      (and (eql (local-time-hour d1) (local-time-hour d2))
           (eql (local-time-minute d1) (local-time-minute d2))
           (eql (local-time-second d1) (local-time-second d2))
           (eql (local-time-nanos d1) (local-time-nanos d2)))))

(define-lessp local-time<
  local-time-hour local-time-minute local-time-second local-time-nanos)

(derive-ordering local-time)

(defun local-time-hash (object)
  (sxhash (logxor (+ (* 10000 (local-time-hour object)) (* 100 (local-time-minute object)) (local-time-second object))
                  (local-time-nanos object))))

(defun local-timestamp= (d1 d2)
  (or (eq d1 d2)
      (and (local-date= (local-timestamp-date d1) (local-timestamp-date d2))
           (local-time= (local-timestamp-time d1) (local-timestamp-time d2)))))

(define-lessp local-timestamp<
  (local-timestamp-date local-date<)
  (local-timestamp-time local-time<))

(derive-ordering local-timestamp)

(defun local-timestamp-hash (object)
  (sxhash (logxor (local-date-hash (local-timestamp-date object))
                  (local-time-hash (local-timestamp-time object)))))


(defvar +epoch-date+ (make-local-date-1 2000 3 1 +wednesday+))
(defvar +midnight+ (make-local-time-1 0 0 0 0))
(defvar +epoch-timestamp+ (make-local-timestamp-1 +epoch-date+ +midnight+))

(defmethod local-date ((object local-date) &key zone)
  (declare (ignore zone))
  object)

(defmethod local-date ((object local-timestamp) &key zone)
  (declare (ignore zone))
  (local-timestamp-date object))

(defmethod local-date ((object local-time) &key zone)
  (declare (ignore zone))
  +epoch-date+)

(defmethod local-time ((object local-date) &key zone)
  (declare (ignore zone))
  +midnight+)

(defmethod local-time ((object local-time) &key zone)
  (declare (ignore zone))
  object)

(defmethod local-time ((object local-timestamp) &key zone)
  (declare (ignore zone))
  (local-timestamp-time object))

(defmethod local-timestamp ((object local-timestamp) &key zone)
  (declare (ignore zone))
  object)

(defmethod local-timestamp ((object local-date) &key zone)
  (declare (ignore zone))
  (make-local-timestamp-1 object +midnight+))

(defmethod local-timestamp ((object local-time) &key zone)
  (declare (ignore zone))
  (make-local-timestamp-1 +epoch-date+ object))



(defun print-local-time (object &optional (stream *standard-output*))
  (with-local-fields (hour minute second nanos) object
    (multiple-value-bind (sub scale)
        (cond
          ((zerop nanos) (values 0 0))
          ((not (zerop (mod nanos 1000))) (values nanos 9))
          ((not (zerop (mod nanos 1000000))) (values (floor nanos 1000) 6))
          (t (values (floor nanos 1000000) 3)))
      (format stream "~2,'0D:~2,'0D:~2,'0D" hour minute second)
      (when (plusp scale)
        (format stream ".~v,'0D" scale sub))))
  object)

(defun print-local-date (object &optional (stream *standard-output*))
  (with-local-fields (year month day) object
    (format stream "~4,'0D-~2,'0D-~2,'0D" year month day))
  object)

(defun print-local-timestamp (object &optional (stream *standard-output*))
  (print-local-date (local-timestamp-date object) stream)
  (write-char #\T stream)
  (print-local-time (local-timestamp-time object) stream)
  object)

(defmethod print-object ((object local-date) stream)
  (if (not *print-escape*)
      (print-local-date object stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-local-date object stream)))
  object)

(defmethod print-object ((object local-time) stream)
  (if (not *print-escape*)
      (print-local-time object stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-local-time object stream)))
  object)

(defmethod print-object ((object local-timestamp) stream)
  (if (not *print-escape*)
      (print-local-timestamp object stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-local-timestamp object stream)))
  object)



(defun make-local-date (year month day)
  (let* ((year (range-checked +min-local-year+ year +max-local-year+ year))
         (month (range-checked 1 month 12 month))
         (day (if (and (integerp day) (<= 1 day 28)) day
                  (let ((last (days-in-month year month)))
                    (if (eql day :last) last
                        (range-checked 1 day last day))))))
    (make-local-date-1 year month day)))

(defun make-local-time (hour minute second &key millisecond microsecond nanosecond nanos)
  (let ((hour (range-checked 0 hour 23 hour))
        (minute (range-checked 0 minute 59 minute))
        (second (range-checked 0 second 59 second))
        (nanos (cond
                 (nanos (range-checked 0 nanos 999999999 nanos))
                 ((or millisecond microsecond nanosecond) (range-checked 0 (+ (* (or millisecond 0) 1000000) (* (or microsecond 0) 1000) (or nanosecond 0)) 999999999 nanos))
                 (t 0))))
    (make-local-time-1 hour minute second nanos)))

(defun make-local-timestamp (year month day hour minute second
                             &key millisecond microsecond nanosecond nanos)
  (make-local-timestamp-1 (make-local-date year month day)
                          (make-local-time hour minute second
                                           :millisecond millisecond :microsecond microsecond
                                           :nanosecond nanosecond :nanos nanos)))

(defun compose-local-timestamp (date time)
  (make-local-timestamp-1 (local-date date) (local-time time)))



(defmethod make-load-form ((object local-date) &optional environment)
  (declare (ignore environment))
  (cond
    ((local-date= object +min-local-year+) '(progn +min-local-date+))
    ((local-date= object +max-local-year+) '(progn +max-local-date+))
    (t `(make-local-date-1 ,(local-date-year object) ,(local-date-month object)
                           ,(local-date-day object) ,(local-date-%dow object)))))

(defmethod make-load-form ((object local-time) &optional environment)
  (declare (ignore environment))
  (cond
    ((local-time= object +min-local-time+) '(progn +min-local-time+))
    ((local-time= object +max-local-time+) '(progn +max-local-time+))
    ((local-time= object +noon+) '(progn +noon+))
    (t `(make-local-time-1 ,(local-time-hour object) ,(local-time-minute object)
                           ,(local-time-second object) ,(local-time-nanos object)))))

(defmethod make-load-form ((object local-timestamp) &optional environment)
  (declare (ignore environment))
  (cond
    ((local-timestamp= object +min-local-timestamp+) '(progn +min-local-timestamp+))
    ((local-timestamp= object +max-local-timestamp+) '(progn +max-local-timestamp+))
    (t `(make-local-timestamp-1 ,(local-timestamp-date object)
                                ,(local-timestamp-time object)))))


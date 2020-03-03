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

(defvar *clock* 't)

(defstruct (instant (:copier nil)
                    (:predicate instantp)
                    (:conc-name instant-)
                    (:constructor make-instant-1 (seconds &optional (nanos 0))))
  (seconds (required-argument) :type epoch-second :read-only t)
  (nanos (required-argument) :type nanos :read-only t))

(defun instant= (i1 i2)
  (or (eql i1 i2)
      (and (eql (instant-seconds i1) (instant-seconds i2))
           (eql (instant-nanos i1) (instant-nanos i2)))))

(define-lessp instant<
  instant-seconds instant-nanos)

(derive-ordering instant)

(defun instant-hash (object)
  (sxhash (logxor (instant-seconds object) (instant-nanos object))))

(defun make-instant (seconds &optional (nanos 0))
  (make-instant-1 seconds nanos))



(defmethod print-object ((object instant) stream)
  (let ((nanos (instant-nanos object)))
    (multiple-value-bind (year month day hour minute second)
        (decode-epoch-seconds (instant-seconds object))
      (multiple-value-bind (sub scale)
          (cond
            ((zerop nanos) (values 0 0))
            ((not (zerop (mod nanos 1000))) (values nanos 9))
            ((not (zerop (mod nanos 1000000))) (values (floor nanos 1000) 6))
            (t (values (floor nanos 1000000) 3)))
        (flet ((show ()
                 (format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D" year month day hour minute second)
                 (when (plusp scale) (format stream ".~v,'0D" scale sub))
                 (write-char #\Z stream)))
          (if *print-escape*
              (print-unreadable-object (object stream :type t :identity nil) (show))
              (show))))))
  object)



(defgeneric clock-now (clock)
  (:method ((clock (eql 't)))
    (multiple-value-bind (seconds nanos) (%get-current-time)
      (make-instant-1 (- seconds +posix-time-offset+) nanos)))
  (:method ((clock (eql ':universal-time)))
    (make-instant-1 (- (get-universal-time) +universal-time-offset+) 0)))

(defun now (&optional (clock *clock*))
  (clock-now clock))



(defmethod instant ((object local-timestamp) &key (zone *zone*))
  (let* ((offset (compute-zone-offset object zone))
         (date (local-timestamp-date object))
         (time (local-timestamp-time object))
         (year (local-date-year date))
         (month (local-date-month date))
         (day (local-date-day date))
         (hour (local-time-hour time))
         (minute (local-time-minute time))
         (second (local-time-second time))
         (nanos (local-time-nanos time)))
    (make-instant-1 (- (encoding-epoch-seconds year month day hour minute second) offset)
                    nanos)))

(defmethod instant ((date local-date) &key (zone *zone*) defaults)
  (let* ((offset (compute-zone-offset date zone))
         (time (if defaults (local-time defaults) +midnight+))
         (year (local-date-year date))
         (month (local-date-month date))
         (day (local-date-day date))
         (hour (local-time-hour time))
         (minute (local-time-minute time))
         (second (local-time-second time))
         (nanos (local-time-nanos time)))
    (make-instant-1 (- (encoding-epoch-seconds year month day hour minute second) offset)
                    nanos)))

(defmethod local-timestamp ((object instant) &key (zone *zone*))
  (multiple-value-bind (offset) (compute-zone-offset object zone)
    (let ((serial (+ offset (instant-seconds object))))
      (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-seconds serial)
        (make-local-timestamp-1 (make-local-date-1 year month day weekday)
                                (make-local-time-1 hour minute second (instant-nanos object)))))))

(defmethod local-date ((object instant) &key (zone *zone*))
  (multiple-value-bind (offset) (compute-zone-offset object zone)
    (let ((serial (+ offset (instant-seconds object))))
      (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-seconds serial)
        (declare (ignore hour minute second))
        (make-local-date-1 year month day weekday)))))

(defmethod local-time ((object instant) &key (zone *zone*))
  (multiple-value-bind (offset) (compute-zone-offset object zone)
    (let ((serial (+ offset (instant-seconds object))))
      (multiple-value-bind (year month day hour minute second) (decode-epoch-seconds serial)
        (declare (ignore year month day))
        (make-local-time-1 hour minute second (instant-nanos object))))))


(defun instant-to-universal-time (instant)
  "Converts an instant into a universal time value, i.e., seconds since
   midnight, Jan 1st 1900 UTC. This function may fail, since the range
   of representable date/times using universal time values is quite limited
   compared to what `instant`s can represent. Also, this conversion will
   forget about any sub-second precision, which may be present in the 
   instant."
  (let ((uni (+ (instant-seconds instant) +universal-time-offset+)))
    (if (not (minusp uni)) uni
        (error "the instant ~S exceeds the supported range of universal time"
               instant))))

(defun universal-time-to-instant (uni &optional (nanos 0))
  "Converts a universal time value (seconds since midnight, Jan 1st 1900 
   UTC) into an instant. This conversion should never fail. If `nanos` is
   not zero, it provides additional sub-second precision for the result."
  (make-instant-1 (- uni +universal-time-offset+) nanos))

(defun instant-to-posix-time (instant)
  "Converts an instant to a POSIX time value, i.e., seconds since midnight,
   Jan 1st 1970 UTC. This conversion will forget about any sub-second precision, 
   which may be present in the instant."
  (+ (instant-seconds instant) +posix-time-offset+))

(defun posix-time-to-instant (psx &optional (nanos 0))
  "Converts a POSIX time value (seconds since midnight, Jan 1st 1970 
   UTC) into an instant. This conversion should never fail. If `nanos` is
   not zero, it provides additional sub-second precision for the result."
  (make-instant-1 (- psx +posix-time-offset+) nanos))

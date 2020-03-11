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

(defgeneric add-seconds (object seconds &optional nanos)
  (:documentation "Answers a temporal object of the same kind as `object`,
    which is generated by adding the given number of seconds to the date/time
    represented by `object`. If given, `nanos` are added as well."))

(defun add-duration (lhs rhs)
  (add-seconds lhs (duration-seconds rhs) (duration-nanos rhs)))

(defun subtract-duration (lhs rhs)
  (add-seconds lhs (- (duration-seconds rhs)) (- (duration-nanos rhs))))

(defun to-seconds-and-nanos (value unit)
  (ecase unit
    ((:nanoseconds :nanosecond) (values 0 value))
    ((:microseconds :microsecond) (values 0 (* value 1000)))
    ((:milliseconds :millisecond) (values 0 (* value 1000000)))
    ((:seconds :second) (values value 0))
    ((:minutes :minute) (values (* value 60) 0))
    ((:hours :hour) (values (* value 3600) 0))
    ((:days :day) (values (* value 86400) 0))
    ((:weeks :week) (values (* value #.(* 86400 7)) 0))))

(defun add-time-unit (object value unit)
  (multiple-value-bind (seconds nanos) (to-seconds-and-nanos value unit)
    (add-seconds object seconds nanos)))

(defun subtract-time-unit (object value unit)
  (multiple-value-bind (seconds nanos) (to-seconds-and-nanos value unit)
    (add-seconds object (- seconds) (- nanos))))

(defun subtract-seconds (object seconds &optional (nanos 0))
  (add-seconds object (- seconds) (- nanos)))

(defmethod add-seconds ((object instant) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (let ((new-seconds (+ (instant-seconds object) seconds))
            (new-nanos (+ (instant-nanos object) nanos)))
        (multiple-value-bind (add-seconds add-nanos) (floor new-nanos 1000000000)
          (make-instant-1 (+ new-seconds add-seconds) add-nanos)))))

(defmethod add-seconds ((object duration) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (let ((new-seconds (+ (duration-seconds object) seconds))
            (new-nanos (+ (duration-nanos object) nanos)))
        (multiple-value-bind (add-seconds add-nanos) (floor new-nanos 1000000000)
          (%make-duration (+ new-seconds add-seconds) add-nanos)))))

(defmethod add-seconds ((object local-timestamp) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos))
      object
      (let ((instant (encode-epoch-seconds (local-year object) (local-month object) (local-day object)
                                           (local-hour object) (local-minute object) (local-second object))))
        (multiple-value-bind (add-second add-nanos) (floor (+ (local-nanos object) nanos) 1000000000)
          (multiple-value-bind (year month day hour minute second) (decode-epoch-seconds (+ instant seconds add-second))
            (make-local-timestamp year month day hour minute second :nanos add-nanos))))))

(defmethod add-seconds ((object local-date) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos))
      object
      (let ((instant (encode-epoch-seconds (local-year object) (local-month object) (local-day object))))
        (multiple-value-bind (add-second) (floor (+ (local-nanos object) nanos) 1000000000)
          (multiple-value-bind (year month day dow) (decode-epoch-seconds (+ instant seconds add-second))
            (make-local-date-1 year month day dow))))))

(defmethod add-seconds ((object local-time) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos))
      object
      (let ((instant (encode-epoch-seconds 2000 3 1 (local-hour object) (local-minute object) (local-second object))))
        (multiple-value-bind (add-second add-nanos) (floor (+ (local-nanos object) nanos) 1000000000)
          (multiple-value-bind (year month day hour minute second) (decode-epoch-seconds (+ instant seconds add-second))
            (declare (ignore year month day))
            (make-local-time hour minute second :nanos add-nanos))))))



(defun local-iso-weekday (object)
  (let ((n (local-weekday object)))
    (if (eql n 0) 6 (1- n))))

(defun local-day-of-year (object)
  (let ((t1 '#(0 31 59 90 120 151 181 212 243 273 304 334))
        (t2 '#(0 31 60 91 121 152 182 213 244 274 305 335))
        (year (local-year object))
        (month (local-month object))
        (day (local-day object)))
    (+ day (svref (if (not (leap-year-p year)) t1 t2) (1- month)))))

(defun 53-week-year-p (year)
  ;; "any year ending on Thursday (D, ED) and any leap year ending on Friday (DC)"
  (let* ((last (make-local-date year 12 31))
         (day (local-weekday last)))
    (or (eql day 4)          ; Thursday
        (and (leap-year-p year)
             (eql day 5))))) ; Friday

(defun local-iso-week&year (object)
  (let* ((year (local-year object))
         (ordinal (local-day-of-year object))
         (weekday (1+ (local-iso-weekday object)))
         (number (floor (+ (- ordinal weekday) 10) 7)))
    (cond
      ((zerop number)
       (let ((py (1- year)))
         (values (if (53-week-year-p py) 53 52) py)))
      ((eql number 53)
       (case (local-iso-weekday object)
         ((3 4 5 6) (values 53 year))
         (otherwise (values 1 (1+ year)))))
      (t (values number year)))))

(defun local-iso-week-number (object)
  (nth-value 0 (local-iso-week&year object)))

(defun local-iso-week-year (object)
  (nth-value 1 (local-iso-week&year object)))

(defun weekday-number (object)
  (ecase object
    ((:sun :sunday 0) 0)
    ((:mon :monday 1) 1)
    ((:tue :tuesday 2) 2)
    ((:wed :wednesday 3) 3)
    ((:thu :thursday 4) 4)
    ((:fri :friday 5) 5)
    ((:sat :saturday 6) 6)))

(defun local-previous-day-of-week (weekday object)
  (let* ((requested (weekday-number weekday))
         (current (local-weekday object))
         (shift (if (<= requested current) (- requested current) (- (- requested current) 7))))
    (add-seconds object (* shift 24 60 60))))

(defun local-next-day-of-week (weekday object)
  (let* ((requested (weekday-number weekday))
         (current (local-weekday object))
         (shift (if (<= requested current) (- requested current) (- (- requested current) 7))))
    (add-seconds object (* (+ shift 7) 24 60 60))))


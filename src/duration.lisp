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

(defstruct (duration (:predicate durationp) (:copier nil)
                     (:constructor %make-duration (seconds nanos)))
  (seconds 0 :type (signed-byte 64) :read-only t)
  (nanos 0 :type (integer 0 999999999) :read-only t))

(defun make-duration (&key
                        (weeks 0) (days 0) (hours 0) (minutes 0) (seconds 0)
                        (milliseconds 0) (microseconds 0) (nanoseconds 0))
  (let ((base-seconds (+ (* #.(* 7 86400) weeks) (* 86400 days) (* 3600 hours)
                         (* 60 minutes) seconds))
        (base-nanos (+ (* 1000000 milliseconds) (* 1000 microseconds) nanoseconds)))
    (multiple-value-bind (dsec dnanos) (floor base-nanos 1000000000)
      (%make-duration (+ dsec base-seconds) dnanos))))

(defun duration= (d1 d2)
  (and (eql (duration-seconds d1) (duration-seconds d2))
       (eql (duration-nanos d1) (duration-nanos d2))))

(define-lessp duration<
  duration-seconds duration-nanos)

(derive-ordering duration)

(defun duration-hash (ob)
  (sxhash (duration-seconds ob)))

(defun duration-plusp (ob)
  (or (plusp (duration-seconds ob))
      (and (zerop (duration-seconds ob)) (plusp (duration-nanos ob)))))

(defun duration-minusp (ob)
  (minusp (duration-seconds ob)))

(defun duration-zerop (ob)
  (and (zerop (duration-seconds ob))
       (zerop (duration-nanos ob))))

(defun scale-duration (duration factor)
  (let ((fn (* factor (+ (* 1000000000 (duration-seconds duration)) (duration-nanos duration)))))
    (multiple-value-bind (s n) (floor fn 1000000000)
      (%make-duration s n))))

(defun negate-duration (duration)
  (if (zerop (duration-nanos duration))
      (%make-duration (- (duration-seconds duration)) 0)
      (scale-duration duration -1)))



(defgeneric duration-between (start end &key))

(defmethod duration-between ((start instant) (end instant) &key zone)
  (declare (ignore zone))
  (let ((seconds (- (instant-seconds end) (instant-seconds start)))
        (nanos (- (instant-nanos end) (instant-nanos start))))
    (cond
      ((and (plusp seconds) (minusp nanos)) (incf seconds))
      ((and (minusp seconds) (plusp nanos)) (decf seconds)))
    (make-duration seconds nanos)))

(defmethod duration-between ((start local-timestamp) (end local-timestamp) &key (zone *zone*))
  (duration-between (instant start :zone zone)
                    (instant end :zone zone)))

(defmethod duration-between ((start instant) (end local-timestamp) &key (zone *zone*))
  (duration-between start
                    (instant end :zone zone)))

(defmethod duration-between ((start local-timestamp) (end instant) &key (zone *zone*))
  (duration-between (instant start :zone zone)
                    end))

(defmethod duration-between ((start local-date) (end local-date) &key (zone *zone*))
  (duration-between (instant start :zone zone)
                    (instant end :zone zone)))

(defmethod duration-between ((start instant) (end local-date) &key (zone *zone*))
  (duration-between start
                    (instant end :zone zone)))

(defmethod duration-between ((start local-date) (end instant) &key (zone *zone*))
  (duration-between (instant start :zone zone)
                    end))

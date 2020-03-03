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

(defconstant +lazyday+ 0)
(defconstant +sunday+ 1)
(defconstant +monday+ 2)
(defconstant +tuesday+ 3)
(defconstant +wednesday+ 4)
(defconstant +thursday+ 5)
(defconstant +friday+ 6)
(defconstant +saturday+ 7)

(defconstant +seconds-per-minute+ 60)
(defconstant +seconds-per-hour+ (* 60 60))
(defconstant +seconds-per-day+ (* 24 60 60))
(defconstant +seconds-per-week+ (* 7 24 60 60))
(defconstant +nanos-per-second+ 1000000000)
(defconstant +years-per-cycle+ 400)
(defconstant +days-per-cycle+ 146097)
(defconstant +min-local-year+ -999999999)
(defconstant +max-local-year+ 999999999)
(defconstant +min-instant-second+ -31557015119088000)
(defconstant +max-instant-second+ 31556888912534399)
(defconstant +universal-time-offset+ 3160857600)
(defconstant +posix-time-offset+ 951868800)

(defgeneric local-date (object &key))
(defgeneric local-time (object &key))
(defgeneric local-timestamp (object &key))
(defgeneric instant (object &key))

(defgeneric local-year (object))
(defgeneric local-month (object))
(defgeneric local-day (object))
(defgeneric local-weekday (object))
(defgeneric local-hour (object))
(defgeneric local-minute (object))
(defgeneric local-second (object))
(defgeneric local-nanos (object))

(deftype epoch-second ()
  `(integer ,+min-instant-second+
            ,+max-instant-second+))

(deftype nanos ()
  '(integer 0 999999999))

(defun day-of-mar1st-year (month day)
  (+ (truncate (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) day -1))

(defun compute-weekday (year month day) ; --> 1 ~ Sun, 2 ~ Mon, ..., 7 ~ Sat
  (let ((year (- year (if (> month 2) 2000 2001))))
    (multiple-value-bind (cycle year-of-cycle) (floor year +years-per-cycle+)
      (declare (ignore cycle))
      (let* ((day-of-year (day-of-mar1st-year month day))
             (day-of-cycle (- (+ (* 365 year-of-cycle) (truncate year-of-cycle 4) day-of-year) (truncate year-of-cycle 100))))
        (1+ (nth-value 1 (floor (+ 3 day-of-cycle) 7)))))))

(defmacro define-lessp-aux (var1 var2 first &rest rest)
  (multiple-value-bind (getter test)
      (if (atom first)
          (values first '<)
          (values (first first) (second first)))
    (if (null rest)
        `(,test (,getter ,var1) (,getter ,var2))
        (let ((field1 (gensym)) (field2 (gensym)))
          `(let ((,field1 (,getter ,var1))
                 (,field2 (,getter ,var2)))
             (cond
               ((,test ,field1 ,field2) t)
               ((,test ,field2 ,field1) nil)
               (t (define-lessp-aux ,var1 ,var2 ,@rest))))))))

(defmacro define-lessp (name &body tests)
  `(defun ,name (object1 object2)
     (define-lessp-aux object1 object2 ,@tests)))

(defmacro derive-ordering (type-name)
  (flet ((symconcat (suffix)
           (intern (concatenate 'string
                                (symbol-name type-name)
                                (symbol-name suffix)))))
    (let ((ob-equal (symconcat '-equal))
          (ob= (symconcat '=))
          (ob/= (symconcat '/=))
          (ob<= (symconcat '<=))
          (ob>= (symconcat '>=))
          (ob< (symconcat '<))
          (ob> (symconcat '>)))
      `(progn
         (declaim (inline ,ob/= ,ob<= ,ob>= ,ob> ,ob-equal))
         (defun ,ob-equal (object1 object2) (,ob= object1 object2))
         (defun ,ob/= (object1 object2) (not (,ob= object1 object2)))
         (defun ,ob> (object1 object2) (,ob< object2 object1))
         (defun ,ob<= (object1 object2) (not (,ob> object1 object2)))
         (defun ,ob>= (object1 object2) (not (,ob< object1 object2)))))))

(defmacro with-local-fields ((&rest bindings) object-form &body body)
  (let ((getters '((year . local-year) (month . local-month) (day . local-day)
                   (weekday . local-weekday) (hour . local-hour) (minute . local-minute)
                   (second . local-second) (nanos . local-nanos) (millisecond . local-millisecond)
                   (microsecond . local-microsecond) (nanosecond . local-nanosecond)))
        (object (gensym)))
    `(let ((,object ,object-form))
       (let (,@(mapcar (lambda (binding)
                         (multiple-value-bind (variable getter)
                             (if (atom binding)
                                 (values binding (cdr (assoc binding getters :test #'string=)))
                                 (values (car binding) (cdr (assoc (cadr binding) getters :test #'string=))))
                           (list variable `(,getter ,object))))
                       bindings))
         ,@body))))

(defun leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun days-in-month (year month)
  (ecase month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    ((2) (if (leap-year-p year) 29 28))))

(defmacro range-checked (min value max field-rep)
  (let ((min-var (gensym))
        (max-var (gensym))
        (val-var (gensym)))
    `(let ((,min-var ,min) (,val-var ,value) (,max-var ,max))
       (if (<= ,min-var ,val-var ,max-var) ,val-var
           (error 'simple-type-error
                  :datum ,val-var :expected-type (list 'integer ,min-var ,max-var)
                  :format-control "value ~S for field ~A lies outside of the accepted range of ~S to ~S"
                  :format-arguments (list ,val-var ',field-rep ,min-var ,max-var))))))



(defmacro declaring-let* ((&rest bindings) &body body)
  (if (null bindings)
      `(let () ,@body)
      (destructuring-bind ((var init &optional (type 't have-type)) &rest more) bindings
        (if (not have-type)
            `(let ((,var ,init))
               (declaring-let* ,more
                 ,@body))
            (let ((temp (gensym (symbol-name var))))
              `(let ((,var (let ((,temp ,init))
                             (if (typep ,temp ',type) ,temp
                                 (error 'simple-type-error
                                        :datum ,temp :expected-type ',type
                                        :format-control "value ~S for variable ~S is not of expected type ~S"
                                        :format-arguments (list ,temp ',var ',type))))))
                 (declare (type ,type ,var))
                 (declaring-let* ,more
                   ,@body)))))))

(defmacro encoding-epoch-seconds (year-form month-form day-form &optional (hour-form 0) (minute-form 0) (second-form 0))
  (let ((year (gensym "YEAR")) (month (gensym "MONTH")) (day (gensym "DAY"))
        (hour (gensym "HOUR")) (minute (gensym "MINUTE")) (second (gensym "SECOND"))
        (cycle (gensym "CYCLE")) (year-of-cycle (gensym "YEAR-OF-CYCLE"))
        (day-of-year (gensym "DAY-OF-YEAR")) (day-of-cycle (gensym "DAY-OF-CYCLE"))
        (days (gensym "DAYS")))
    `(let ((,year ,year-form) (,month ,month-form) (,day ,day-form)
           (,hour ,hour-form) (,minute ,minute-form) (,second ,second-form))
       (multiple-value-bind (,cycle ,year-of-cycle) (floor (- ,year (if (> ,month 2) 2000 2001)) +years-per-cycle+)
         (let* ((,day-of-year (day-of-mar1st-year ,month ,day))
                (,day-of-cycle (- (+ (* 365 ,year-of-cycle) (truncate ,year-of-cycle 4) ,day-of-year) (truncate ,year-of-cycle 100)))
                (,days (+ (* ,cycle +days-per-cycle+) ,day-of-cycle)))
           (declare (type (integer 0 366) ,day-of-year))
           (+ (* +seconds-per-day+ ,days)
              (+ ,second (* ,minute +seconds-per-minute+) (* ,hour +seconds-per-hour+))))))))

(defun encode-epoch-seconds (year month day &optional (hour 0) (minute 0) (second 0))
  (declare (optimize (speed 3) (debug 0)))
  (declaring-let* ((year year (integer -1000000000 1000000000))
                   (month month (integer 1 12))
                   (day day (integer 1 31))
                   (hour hour (integer 0 23))
                   (minute minute (integer 0 59))
                   (second second (integer 0 59)))
    (encoding-epoch-seconds year month day hour minute second)))

(defun decode-epoch-seconds (value)
  (declare (optimize (speed 3) (debug 0)))
  (if (not (typep value 'epoch-second))
      (error 'simple-type-error
             :datum value :expected-type 'epoch-second
             :format-control "~S is not a valid seconds-since-epoch value"
             :format-arguments (list value))
      (locally (declare (type epoch-second value))
        (multiple-value-bind (day second-of-day) (floor value +seconds-per-day+)
          (multiple-value-bind (cycle day-of-cycle) (floor day +days-per-cycle+)
            (let* ((year-of-cycle (truncate (+ day-of-cycle
                                               (- (truncate day-of-cycle 1460))
                                               (truncate day-of-cycle 36524)
                                               (- (truncate day-of-cycle (1- +days-per-cycle+))))
                                            365))
                   (year (+ year-of-cycle (* cycle +years-per-cycle+))) ; not yet, but see below
                   (day-of-year (+ (- day-of-cycle
                                      (* 365 year-of-cycle)
                                      (truncate year-of-cycle 4))
                                   (truncate year-of-cycle 100)))
                   (mp (truncate (+ (* 5 day-of-year) 2) 153))
                   (day-of-month (1+ (- day-of-year (truncate (+ (* 153 mp) 2) 5))))
                   (month (+ mp (if (< mp 10) 3 -9))))
              (multiple-value-bind (hour rest) (floor second-of-day +seconds-per-hour+)
                (multiple-value-bind (minute second) (floor rest +seconds-per-minute+)
                  (values (+ year (if (> month 2) 2000 2001)) month day-of-month
                          hour minute second
                          (1+ (nth-value 1 (floor (+ 3 day-of-cycle) 7))))))))))))

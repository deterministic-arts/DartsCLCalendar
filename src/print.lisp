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


(defvar *default-locale* nil
  "The locale object to use, if none is provided explicitly. This
   variable doesn't belong here at all.")

(defvar *formatter-cache* (make-weak-hash-table :test 'equal))
(defvar *formatter-cache-lock* (make-lock "Formatter Cache Lock"))

(defgeneric localized-month-name (month locale))
(defgeneric localized-month-abbreviation (month locale))
(defgeneric localized-weekday-name (day locale))
(defgeneric localized-weekday-abbreviation (day locale))
(defgeneric localized-meridian (value locale))
(defgeneric localized-timestamp-format (type locale))
(defgeneric localized-beginning-of-week (locale))


(defparameter *default-month-names*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(defparameter *default-month-abbreviations*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *default-weekday-names*
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defparameter *default-weekday-abbreviations*
  #("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"))

(defparameter *default-meridian*
  #("am" "pm"))

(defvar *default-date-format* nil)
(defvar *default-time-format* nil)
(defvar *default-timestamp-format* nil)

(defmethod localized-month-name (month locale)
  (svref *default-month-names* (1- month)))

(defmethod localized-month-abbreviation (month locale)
  (svref *default-month-abbreviations* (1- month)))

(defmethod localized-weekday-name (day locale)
  (svref *default-weekday-names* day))

(defmethod localized-weekday-abbreviation (day locale)
  (svref *default-weekday-abbreviations* day))

(defmethod localized-meridian (value locale)
  (svref *default-meridian* value))

(defmethod localized-beginning-of-week (locale)
  :sunday)

(defmethod localized-timestamp-format (type locale)
  (case type
    ((:short-date :medium-date :long-date) *default-date-format*)
    ((:short-time :medium-time :long-time) *default-time-format*)
    ((:short-timestamp :medium-timestamp :long-timestamp) *default-timestamp-format*)
    (otherwise (call-next-method))))



(defvar *format-lock* (make-lock "Formatter Table Lock"))
(defvar *format-operators* (make-hash-table :test 'equal))

(defun make-timestamp-printer-function (commands)
  (lambda (object &key (stream *standard-output*) (locale *default-locale*) (zone *zone*))
    (let ((ts (local-timestamp object :zone zone)))
      (dolist (cmd commands)
        (if (stringp cmd)
            (write-string cmd stream)
            (funcall cmd ts zone locale stream)))
      object)))

(defun compile-timestamp-printer-pattern (list)
  (labels
      ((parse-directive (thing)
         (cond
           ((stringp thing) thing)
           ((characterp thing) (string thing))
           ((symbolp thing)
            (let ((handler (with-lock-held (*format-lock*) (gethash (symbol-name thing) *format-operators*))))
              (if handler
                  (funcall handler nil)
                  (error "unknown timestamp format operator: ~S" thing))))
           ((consp thing)
            (if (not (symbolp (car thing)))
                (error "malformed timestamp format directive: ~S" thing)
                (let ((handler (with-lock-held (*format-lock*) (gethash (symbol-name (car thing)) *format-operators*))))
                  (if handler
                      (funcall handler (cdr thing))
                      (error "unknown thimestamp format operator: ~S" (car thing))))))
           (t (error "malformed timestamp format directive: ~S" thing))))
       (push-command (cmd list)
         (cond
           ((null list) (list cmd))
           ((not (stringp (car list))) (cons cmd list))
           ((not (stringp cmd)) (cons cmd list))
           (t (cons (concatenate 'string (car list) cmd) (cdr list))))))
    (let ((sequence nil))
      (dolist (directive list)
        (setf sequence (push-command (parse-directive directive) sequence)))
      (make-timestamp-printer-function (nreverse sequence)))))
               
(defmacro define-print-operator (name (&rest args) &body body)
  (let ((temp (gensym)))
    `(with-lock-held (*format-lock*)
       (setf (gethash ,(symbol-name name) *format-operators*)
             (lambda (,temp)
               (block ,name
                 (destructuring-bind (,@args) ,temp
                   ,@body)))))))


(macrolet
    ((print-lambda (&body body)
       `(lambda (ts tzinfo locale stream)
          (declare (ignorable tzinfo locale))
          ,@body)))

(define-print-operator year (&key (width 1))
  (print-lambda
    (format stream "~v,'0D" width (local-year ts))))

(define-print-operator month (&key (format :number) (width 1))
  (ecase format
    ((:name)
     (print-lambda (write-string (localized-month-name (local-month ts) locale) stream)))
    ((:abbreviation :abbrev)
     (print-lambda (write-string (localized-month-abbreviation (local-month ts) locale) stream)))
    ((:number)
     (print-lambda (format stream "~v,'0D" width (local-month ts))))))

(define-print-operator day (&key (format :number) (width 1))
  (ecase format
    ((:number) (print-lambda (format stream "~v,'0D" width (local-day ts))))
    ((:english-ordinal) (print-lambda (let ((n (local-day ts)))
                                        (format stream "~D~A" n
                                                (case n
                                                  ((11 12 13) "th")
                                                  (otherwise (case (mod n 10)
                                                               ((1) "st")
                                                               ((2) "nd")
                                                               ((3) "rd")
                                                               (otherwise "th"))))))))))

(define-print-operator weekday (&key (format :name) (width 1))
  (ecase format
    ((:name) (print-lambda (write-string (localized-weekday-name (local-weekday ts) locale) stream)))
    ((:abbreviation :abbrev) (print-lambda (write-string (localized-weekday-abbreviation (local-weekday ts) locale) stream)))
    ((:number) (print-lambda (format stream "~v,'0D" width (1+ (local-weekday ts)))))))

(define-print-operator hour (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (local-hour ts))))

(define-print-operator hour12 (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (mod (local-hour ts) 12))))

(define-print-operator minute (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (local-minute ts))))

(define-print-operator second (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (local-second ts))))

(define-print-operator subsecond (&key (precision :nanosecond))
  (if (eq precision :auto)
      (print-lambda
        (let ((nanos (local-nanos ts)))
          (if (zerop nanos)
              (write-string "0" stream)
              (multiple-value-bind (ms rest) (floor nanos 1000000)
                (multiple-value-bind (us ns) (floor rest 1000)
                  (format stream "~3,'0D~@[~3,'0D~]~@[~3,'0D~]"
                          ms (and (or (plusp us) (plusp ns)) us)
                          (and (plusp ns) ns)))))))
      (multiple-value-bind (divisor width)
          (ecase precision
            ((:millisecond :ms) (values 1000000 3))
            ((:microsecond :us) (values 1000 6))
            ((:nanosecond :ns) (values 1 9)))
        (print-lambda
          (format stream "~v,'0D" width (floor (local-nanos ts) divisor))))))

(define-print-operator meridian ()
  (print-lambda (write-string (localized-meridian (if (< (local-hour ts) 12) 0 1) locale) stream)))

(define-print-operator week (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (local-iso-week-number ts))))

(define-print-operator week-year (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (local-iso-week-year ts))))

(define-print-operator day-of-year (&key (width 1))
  (print-lambda (format stream "~v,'0D" width (local-day-of-year ts))))

nil)                                    ; macrolet


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +literal-chars+ '(#\. #\- #\: #\, #\; #\( #\) #\[ #\] #\! #\? #\space #\tab
                            #\return #\linefeed)))

(defun parse-timestamp-format-string (string &key (start 0) end)
  (let* ((string (string string))
         (end (or end (length string)))
         (list nil))
    (labels
        ((parse-literal (pos)
           (let ((buffer (make-array (- end pos) :element-type 'character :fill-pointer 0)))
             (loop
               (if (>= pos end)
                   (error "unterminated literal")
                   (let ((ch (char string pos)))
                     (if (not (eql ch #\'))
                         (vector-push-extend ch buffer)
                         (if (or (>= (1+ pos) end) (not (eql (char string (1+ pos)) #\')))
                             (progn
                               (push (coerce buffer 'simple-string) list)
                               (return (parse-command (1+ pos))))
                             (progn
                               (vector-push-extend #\' buffer)
                               (incf pos))))
                     (incf pos))))))
         (parse-direct-literal (pos)
           (let ((buffer (make-array (- end pos) :element-type 'character :fill-pointer 0)))
             (loop
                while (and (< pos end) (find (char string pos) #.(concatenate 'string +literal-chars+)))
                do (vector-push-extend (char string pos) buffer)
                   (incf pos))
             (push (coerce buffer 'simple-string) list)
             (parse-command pos)))
         (parse-directive (char pos)
           (let ((count 1))
             (loop
                while (and (< pos end) (eql (char string pos) char))
                do (incf pos) (incf count))
             (case char
               ((#\y) (ecase count
                        ((2 4) (push `(year :width ,count) list))))
               ((#\W) (ecase count
                        ((2 4) (push `(week-year :width ,count) list))))
               ((#\M) (case count
                        ((1 2) (push `(month :format :number :width ,count) list))
                        ((3) (push `(month :format :abbreviation) list))
                        (otherwise (push `(month :format :name) list))))
               ((#\d) (ecase count
                        ((1 2) (push `(day :format :number :width ,count) list))
                        ((3) (push `(day :format :english-ordinal) list))))
               ((#\D) (ecase count
                        ((1 2 3) (push `(day-of-year :width ,count) list))))
               ((#\w) (ecase count
                        ((1 2) (push `(week :width ,count) list))))
               ((#\e) (case count
                        ((1 2) (push `(weekday :format :number :width ,count) list))
                        ((3) (push `(weekday :format :abbreviation) list))
                        (otherwise (push `(weekday :format :name) list))))
               ((#\H) (ecase count
                        ((1 2) (push `(hour :width ,count) list))))
               ((#\h) (ecase count
                        ((1 2) (push `(hour12 :width ,count) list))))
               ((#\a) (ecase count
                        ((1 2) (push `(meridian) list))))
               ((#\m) (ecase count
                        ((1 2) (push `(minute :width ,count) list))))
               ((#\s) (ecase count
                        ((1 2) (push `(second :width ,count) list))))
               ((#\S) (ecase count
                        ((1) (push `(subsecond :precision :auto) list))
                        ((3) (push `(subsecond :precision :ms) list))
                        ((6) (push `(subsecond :precision :us) list))
                        ((9) (push `(subsecond :precision :ns) list))))
               (otherwise (error "unknown command character ~C" char)))
             (parse-command pos)))
         (parse-command (pos)
           (when (< pos end)
             (let ((char (char string pos)))
               (case char
                 ((#\') (parse-literal (1+ pos)))
                 (#.+literal-chars+ (parse-direct-literal pos))
                 (otherwise (cond
                              ((char<= #\a char #\z) (parse-directive char (1+ pos)))
                              ((char<= #\A char #\Z) (parse-directive char (1+ pos)))
                              ((char<= #\0 char #\9)
                               (push char list)
                               (parse-command (1+ pos)))
                              (t (error "unsupported format character ~C" char)))))))))
      (parse-command start)
      (nreverse list))))
                  
(defun compile-timestamp-printer (pattern)
  (if (not (stringp pattern))
      (compile-timestamp-printer-pattern pattern)
      (let ((compiled (with-lock-held (*formatter-cache-lock*)
                        (gethash pattern *formatter-cache*))))
        (or compiled
            (let ((compiled (compile-timestamp-printer-pattern (parse-timestamp-format-string pattern))))
              (with-lock-held (*formatter-cache-lock*)
                (or (gethash pattern *formatter-cache*)
                    (setf (gethash pattern *formatter-cache*) compiled))))))))
     
(defun print-timestamp (object
                        &key (stream *standard-output*) (zone *zone*)
                             (locale *default-locale*)
                             (format (localized-timestamp-format :medium-timestamp locale)))
  (funcall (if (not (typep format 'function))
               (compile-timestamp-printer format)
               format)
           object
           :stream stream :zone zone
           :locale locale))
  
(defun format-timestamp (stream pattern object
                         &key (locale *default-locale*) (zone *zone*))
  (let ((formatter (if (not (typep pattern 'function))
                       (compile-timestamp-printer pattern)
                       pattern)))
    (if stream
        (funcall formatter object
                 :stream (if (eq stream 't) *terminal-io* stream)
                 :locale locale :zone zone)
        (with-output-to-string (stream)
          (funcall formatter object
                   :stream stream
                   :locale locale :zone zone)))))

(setf *default-date-format*
      (compile-timestamp-printer "yyyy-MM-dd"))

(setf *default-time-format*
      (compile-timestamp-printer "HH:mm:ss.S"))

(setf *default-timestamp-format*
      (compile-timestamp-printer "yyyy-MM-dd'T'HH:mm:ss.S"))

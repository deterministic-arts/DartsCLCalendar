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

(defvar *formatter-cache* (make-weak-hash-table :test 'equal))
(defvar *formatter-cache-lock* (make-lock "Formatter Cache Lock"))

(defgeneric localized-month-name (month locale))
(defgeneric localized-month-abbreviation (month locale))
(defgeneric localized-weekday-name (day locale))
(defgeneric localized-weekday-abbreviation (day locale))
(defgeneric localized-meridian (value locale))
(defgeneric localized-timestamp-format (type locale))
(defgeneric localized-beginning-of-week (locale))
(defgeneric localized-era-designator (era locale))


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

(defparameter *default-era-designators*
  #("BCE" "CE"))

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

(defmethod localized-era-designator (era locale)
  (svref *default-era-designators* era))



(defvar *format-lock* (make-lock "Formatter Table Lock"))
(defvar *format-operators* (make-hash-table :test 'equal))

(defun make-timestamp-printer-function (commands)
  (lambda (object &key (stream *standard-output*) (locale *locale*) (zone *zone*))
    (locally (declare (ftype (function (&optional t) t) resolve-calendar-symbols))
      (let ((locale (resolve-calendar-symbols locale))
            (ts (local-timestamp object :zone zone)))
        (dolist (cmd commands)
          (if (stringp cmd)
              (write-string cmd stream)
              (funcall cmd ts zone locale stream)))
        object))))

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
                   ,@body))))
       ',name)))

(defmacro define-simple-print-operator (name (&rest outer-lambda-list) (&rest inner-lambda-list) &body body)
  (let ((ignored-vars nil)
        ts-var tz-var locale-var stream-var)
    (loop
       for name in inner-lambda-list
       do (cond
            ((string= :stream name) (setf stream-var name))
            ((string= :zone name) (setf tz-var name))
            ((string= :locale name) (setf locale-var name))
            ((string= :object name) (setf ts-var name))
            (t (error "unsupported parameter name ~S" name))))
    (unless ts-var
      (push (setf ts-var (gensym)) ignored-vars))
    (unless tz-var
      (push (setf tz-var (gensym)) ignored-vars))
    (unless locale-var
      (push (setf locale-var (gensym)) ignored-vars))
    (unless stream-var
      (push (setf stream-var (gensym)) ignored-vars))
    `(define-print-operator ,name (,@outer-lambda-list)
       (lambda (,ts-var ,tz-var ,locale-var ,stream-var)
         (declare (ignore ,@ignored-vars))
         ,@body))))



(macrolet
    ((print-lambda (&body body)
       `(lambda (ts tzinfo locale stream)
          (declare (ignorable tzinfo locale))
          ,@body)))

(define-simple-print-operator year (&key (width 1)) (object stream)
  (format stream "~v,'0D"
          width (if (eql width 2) (rem (local-year object) 100)
                    (local-year object))))

(define-simple-print-operator year-of-era (&key (width 1)) (object stream)
  (format stream "~v,'0D"
          width (if (eql width 2) (rem (local-year-of-era object) 100)
                    (local-year-of-era object))))

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

(define-print-operator era (&key (format :name) (width 1))
  (ecase format
    ((:number) (print-lambda (format stream "~v,'0D" width (local-era ts))))
    ((:name) (print-lambda (write-string (localized-era-designator (local-era ts) locale)
                                         stream)))))

(define-simple-print-operator hour (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (local-hour object)))

(define-simple-print-operator hour12 (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (mod (local-hour object) 12)))

(define-simple-print-operator minute (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (local-minute object)))

(define-simple-print-operator second (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (local-second object)))

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

(define-simple-print-operator meridian () (object locale stream)
  (write-string (localized-meridian (if (< (local-hour object) 12) 0 1) locale) stream))

(define-simple-print-operator week (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (local-iso-week-number object)))

(define-simple-print-operator week-year (&key (width 1)) (object stream)
  (format stream "~v,'0D" width
          (if (eql width 2) (rem (local-iso-week-number object) 100)
              (local-iso-week-year object))))

(define-simple-print-operator day-of-year (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (local-day-of-year object)))

(define-simple-print-operator zone-offset (&key (precision :hours) (colons nil) (zero nil)) (object zone stream)
  (let ((offset (compute-zone-offset object zone)))
    (if (and (zerop offset) zero)
        (princ zero stream)
        (multiple-value-bind (minutes* seconds) (floor offset 60)
          (multiple-value-bind (hours minutes) (floor minutes* 60)
            (format stream "~A~2,'0D~:[~;~A~2,'0D~:[~;~A~2,'0D~]~]"
                    (if (minusp hours) #\- #\+)
                    (abs hours)
                    (or (plusp minutes) (plusp seconds) (member precision '(:minutes :seconds)))
                    (if colons ":" "")
                    minutes
                    (or (plusp seconds) (eq precision :seconds))
                    (if colons ":" "")
                    seconds))))))

nil)                                    ; macrolet


(defparameter +literal-chars+ '(#\. #\- #\: #\, #\; #\( #\) #\[ #\] #\! #\? #\/ #\space #\tab
                                #\return #\linefeed))

;;; See https://unicode-org.github.io/icu-docs/apidoc/released/icu4j/com/ibm/icu/text/SimpleDateFormat.html
;;; for a description of the origin of the format codes used here.

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
                while (and (< pos end) (find (char string pos) +literal-chars+))
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
                        ((1 2 3 4) (push `(year :width ,count) list))))
               ((#\u) (ecase count
                        ((1 2 3 4) (push `(year-of-era :width ,count) list))))
               ((#\Y) (ecase count
                        ((1 2 3 4) (push `(week-year :width ,count) list))))
               ((#\G) (ecase count
                        ((1 2 3) (push `(era :format :name) list))))
               ((#\M) (case count
                        ((1 2) (push `(month :format :number :width ,count) list))
                        ((3) (push `(month :format :abbreviation) list))
                        (otherwise (push `(month :format :name) list))))
               ((#\d) (ecase count
                        ((1 2) (push `(day :format :number :width ,count) list))))
               ((#\D) (ecase count
                        ((1 2 3) (push `(day-of-year :width ,count) list))))
               ((#\E) (case count
                        ((1 2) (push `(weekday :format :number :width ,count) list))
                        ((3) (push `(weekday :format :abbreviation) list))
                        ((4) (push `(weekday :format :name) list))))
               ((#\w) (ecase count
                        ((1 2) (push `(week :width ,count) list))))
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
               ((#\x) (ecase count
                        ((1) (push `(zone-offset :precision :hours :colons nil) list))
                        ((2) (push `(zone-offset :precision :minutes :colons nil) list))
                        ((3) (push `(zone-offset :precision :minutes :colons t) list))
                        ((4) (push `(zone-offset :precision :seconds :colons nil) list))
                        ((5) (push `(zone-offset :precision :seconds :colons t) list))))
               ((#\X) (ecase count
                        ((1) (push `(zone-offset :precision :hours :colons nil :zero #\Z) list))
                        ((2) (push `(zone-offset :precision :minutes :colons nil :zero #\Z) list))
                        ((3) (push `(zone-offset :precision :minutes :colons t :zero #\Z) list))
                        ((4) (push `(zone-offset :precision :seconds :colons nil :zero #\Z) list))
                        ((5) (push `(zone-offset :precision :seconds :colons t :zero #\Z) list))))
               (otherwise (error "unknown command character ~C" char)))
             (parse-command pos)))
         (parse-command (pos)
           (when (< pos end)
             (let ((char (char string pos)))
               (cond
                 ((eql char #\') (parse-literal (1+ pos)))
                 ((member char +literal-chars+) (parse-direct-literal pos))
                 ((char<= #\a char #\z) (parse-directive char (1+ pos)))
                 ((char<= #\A char #\Z) (parse-directive char (1+ pos)))
                 ((char<= #\0 char #\9)
                  (push char list)
                  (parse-command (1+ pos)))
                 (t (error "unsupported format character ~C" char)))))))
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

(defun resolve-format (format locale)
  (etypecase format
    (function format)
    (string (compile-timestamp-printer format))
    (cons (compile-timestamp-printer format))
    (keyword (localized-timestamp-format format locale))))

(defun print-timestamp (object
                        &key (stream *standard-output*) (zone *zone*)
                             (locale *locale*)
                             (format :medium-timestamp))
  (let* ((locale (resolve-calendar-symbols locale))
         (printer (resolve-format format locale)))
    (funcall printer object
             :stream stream :zone zone
             :locale locale)))
  
(defun format-timestamp (stream pattern object
                         &key (locale *locale*) (zone *zone*))
  (let ((locale (resolve-calendar-symbols locale))
        (formatter (resolve-format pattern locale)))
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



(defun trim-string (value)
  (string-trim #.(concatenate 'string '(#\space #\tab #\newline #\return))
               value))

(defun parse-string-vector (input &key (separator #\,))
  (let ((entries (mapcar #'trim-string (split-sequence separator input))))
    (make-array (length entries)
                :element-type 't
                :initial-contents entries)))

(defun parse-printer (input)
  (compile-timestamp-printer input))

(define-locale-category calendar-symbols
    ((month-names
       :default *default-month-names* :parser parse-string-vector)
     (month-abbreviations
       :default *default-month-abbreviations* :parser parse-string-vector)
     (weekday-names
       :default *default-weekday-names* :parser parse-string-vector)
     (weekday-abbreviations
       :default *default-weekday-abbreviations* :parser parse-string-vector)
     (meridian-names
       :default *default-meridian* :parser parse-string-vector)
     (era-names
       :default *default-era-designators* :parser parse-string-vector)
     (first-day-of-week
       :default :sunday :parser (lambda (str)
                                  (or (find (trim-string str) '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday)
                                            :test #'string-equal)
                                      +inherit+)))
     (short-date-format
       :default *default-date-format* :parser parse-printer)
     (medium-date-format
       :default *default-date-format* :parser parse-printer)
     (long-date-format
       :default *default-date-format* :parser parse-printer)
     (short-time-format
       :default *default-time-format* :parser parse-printer)
     (medium-time-format
       :default *default-time-format* :parser parse-printer)
     (long-time-format
       :default *default-time-format* :parser parse-printer)
     (short-timestamp-format
       :default *default-timestamp-format* :parser parse-printer)
     (medium-timestamp-format
       :default *default-timestamp-format* :parser parse-printer)
     (long-timestamp-format
      :default *default-timestamp-format* :parser parse-printer))
  (:cache-function t))

(defun resolve-calendar-symbols (key)
  (typecase key
    (null nil)
    (locale (calendar-symbols key))
    (string (calendar-symbols (locale key)))
    (t key)))

(defmethod localized-month-name (month (locale locale))
  (svref (calendar-symbols-month-names (calendar-symbols locale)) (1- month)))

(defmethod localized-month-name (month (locale calendar-symbols))
  (svref (calendar-symbols-month-names locale) (1- month)))

(defmethod localized-month-abbreviation (month (locale locale))
  (svref (calendar-symbols-month-abbreviations (calendar-symbols locale)) (1- month)))

(defmethod localized-month-abbreviation (month (locale calendar-symbols))
  (svref (calendar-symbols-month-abbreviations locale) (1- month)))

(defmethod localized-weekday-name (day (locale locale))
  (svref (calendar-symbols-weekday-names (calendar-symbols locale)) day))

(defmethod localized-weekday-name (day (locale calendar-symbols))
  (svref (calendar-symbols-weekday-names locale) day))

(defmethod localized-weekday-abbreviation (day (locale locale))
  (svref (calendar-symbols-weekday-abbreviations (calendar-symbols locale)) day))

(defmethod localized-weekday-abbreviation (day (locale calendar-symbols))
  (svref (calendar-symbols-weekday-abbreviations locale) day))

(defmethod localized-meridian (value (locale locale))
  (svref (calendar-symbols-meridian-names (calendar-symbols locale)) value))

(defmethod localized-meridian (value (locale calendar-symbols))
  (svref (calendar-symbols-meridian-names locale) value))

(defmethod localized-era-designator (era (locale locale))
  (svref (calendar-symbols-era-names (calendar-symbols locale)) era))

(defmethod localized-era-designator (era (locale calendar-symbols))
  (svref (calendar-symbols-era-names locale) era))

(defmethod localized-beginning-of-week ((locale locale))
  (calendar-symbols-first-day-of-week (calendar-symbols locale)))

(defmethod localized-beginning-of-week ((locale calendar-symbols))
  (calendar-symbols-first-day-of-week locale))

(defmethod localized-timestamp-format (type (locale locale))
  (localized-timestamp-format type (calendar-symbols locale)))

(defmethod localized-timestamp-format (type (locale calendar-symbols))
  (case type
    ((:short-date) (calendar-symbols-short-date-format locale))
    ((:medium-date) (calendar-symbols-medium-date-format locale))
    ((:long-date) (calendar-symbols-long-date-format locale))
    ((:short-time) (calendar-symbols-short-time-format locale))
    ((:medium-time) (calendar-symbols-medium-time-format locale))
    ((:long-time) (calendar-symbols-long-time-format locale))
    ((:short-timestamp) (calendar-symbols-short-timestamp-format locale))
    ((:medium-timestamp) (calendar-symbols-medium-timestamp-format locale))
    ((:long-timestamp) (calendar-symbols-long-timestamp-format locale))
    (otherwise (call-next-method))))

(add-locale-resource-directory (asdf:system-relative-pathname '#:darts.lib.calendar "./data/"))

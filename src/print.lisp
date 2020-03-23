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

(defgeneric localized-month (month locale &optional format))
(defgeneric localized-weekday (day locale &optional format))
(defgeneric localized-meridian (value locale &optional format))
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
(defvar *default-timestamp-template* "{1} {0}")

(defmethod localized-month (month locale &optional format)
  (svref (ecase format
           ((:wide) *default-month-names*)
           ((:abbreviated :narrow :short) *default-month-abbreviations*))
         (1- month)))

(defmethod localized-weekday (day locale &optional format)
  (svref (ecase format
           ((:wide) *default-weekday-names*)
           ((:abbreviated :narrow :short) *default-weekday-abbreviations*))
         day))

(defmethod localized-meridian (value locale &optional format)
  (svref (ecase format
           ((:wide :abbreviated :narrow :short) *default-meridian*))
         value))

(defmethod localized-era (value locale &optional format)
  (svref (ecase format
           ((:wide :abbreviated :narrow :short) *default-era-designators*))
         value))

(defmethod localized-beginning-of-week (locale)
  +sunday+)

(defmethod localized-timestamp-format (type locale)
  (case type
    ((:short-date :medium-date :long-date :full-date) *default-date-format*)
    ((:short-time :medium-time :long-time :full-time) *default-time-format*)
    ((:short-timestamp :medium-timestamp :long-timestamp :full-timestamp) *default-timestamp-format*)
    (otherwise (call-next-method))))



(atomics:defstruct (time-format (:copier nil)
                                (:constructor make-time-format-1 (expression
                                                                  &key pattern
                                                                       ((:printer %printer) nil)
                                                                       ((:parser %parser) nil))))
  (pattern nil :type (or null string) :read-only t)
  (expression nil :type t :read-only t)
  (%printer nil :type t)
  (%parser nil :type t))

(defmethod print-object ((object time-format) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((pattern (time-format-pattern object)))
      (if pattern
          (write-string pattern stream)
          (write-string "(complex)" stream))))
  object)

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

(defun compile-timestamp-printer-commands (list)
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
      (nreverse sequence))))

(defun time-format-printer (object)
  (loop
     (let ((old (time-format-%printer object)))
       (if old
           (return old)
           (let* ((expression (time-format-expression object))
                  (commands (compile-timestamp-printer-commands expression))
                  (new (make-timestamp-printer-function commands)))
             (when (atomics:cas (time-format-%printer object) old new)
               (return new)))))))
               
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

(define-print-operator month (&key (format :numeric) (width 1))
  (ecase format
    ((:wide :short :narrow :abbreviated)
     (print-lambda (write-string (localized-month (local-month ts) locale format) stream)))
    ((:numeric)
     (print-lambda (format stream "~v,'0D" width (local-month ts))))))

(define-simple-print-operator day (&key (width 1)) (object stream)
  (format stream "~v,'0D" width (local-day object)))

(define-print-operator weekday (&key (format :numeric) (width 1))
  (ecase format
    ((:abbreviated :narrow :wide :short) (print-lambda (write-string (localized-weekday (local-weekday ts) locale format) stream)))
    ((:numeric)
     (print-lambda (format stream "~v,'0D" width (1+ (local-weekday ts)))))
    ((:local-numeric)
     (print-lambda
      (let* ((fow (localized-beginning-of-week locale))
             (dow (local-weekday ts))
             (loc (if (<= fow dow) (1+ (- dow fow)) (- 7 (- fow dow)))))
        (format stream "~v,'0D" width loc))))))        

(define-print-operator era (&key (format :numeric) (width 1))
  (ecase format
    ((:abbreviated :narrow :wide :short) (print-lambda (write-string (localized-era (local-era ts) locale format) stream)))
    ((:numeric) (print-lambda (format stream "~v,'0D" width (local-era ts))))))

(defun nonzero-or-else (other number)
  (if (zerop number) other number))

(define-print-operator hour (&key (width 1) (range 24) (from-zero t))
  (ecase range
    ((12)
     (if from-zero
         (print-lambda (format stream "~v,'0D" width (mod (local-hour ts) 12)))
         (print-lambda (format stream "~v,'0D" width (nonzero-or-else (mod (local-hour ts) 12) 12)))))
    ((24)
     (if from-zero
         (print-lambda (format stream "~v,'0D" width (local-hour ts)))
         (print-lambda (format stream "~v,'0D" width (nonzero-or-else (local-hour ts)
                                                                      24)))))))

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

(define-print-operator meridian (&key (format :numeric) (width 1))
  (ecase format
    ((:numeric) (print-lambda (format stream "~v,'0D" width (if (< (local-hour ts) 12) 0 1))))
    ((:narrow :short :wide :abbreviated)
     (print-lambda (write-string (localized-meridian (if (< (local-hour ts) 12) 0 1) locale format)
                                 stream)))))

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
             ;; We try to keep the meaning of the counts as close to what they
             ;; mean in the ICU library. That is probably a bad idea, given that
             ;; there are a few very strange conventions there...
             (case char
               ((#\y) (ecase count
                        ((1 2 3 4) (push `(year :width ,count) list))))
               ((#\u) (ecase count
                        ((1 2 3 4) (push `(year-of-era :width ,count) list))))
               ((#\Y) (ecase count
                        ((1 2 3 4) (push `(week-year :width ,count) list))))
               ((#\G) (ecase count
                        ((1 2 3) (push `(era :format :abbreviated) list))
                        ((4) (push `(era :format :wide) list))
                        ((5) (push `(era :format :narrow) list))))
               ((#\M) (ecase count
                        ((1 2) (push `(month :format :numeric :width ,count) list))
                        ((3) (push `(month :format :abbreviated) list))
                        ((4) (push `(month :format :wide) list))
                        ((5) (push `(month :format :narrow) list))))
               ((#\d) (ecase count
                        ((1 2) (push `(day :width ,count) list))))
               ((#\D) (ecase count
                        ((1 2 3) (push `(day-of-year :width ,count) list))))
               ((#\e #\c) (case count
                            ((1 2) (push `(weekday :format :local-numeric :width ,count) list))
                            ((4) (push `(weekday :format :wide) list))
                            ((5) (push `(weekday :format :narrow) list))
                            ((6) (push `(weekday :format :short) list))))
               ((#\E) (case count
                        ((1 2 3) (push `(weekday :format :abbreviated :width ,count) list))
                        ((4) (push `(weekday :format :wide) list))
                        ((5) (push `(weekday :format :narrow) list))
                        ((6) (push `(weekday :format :short) list))))
               ((#\w) (ecase count
                        ((1 2) (push `(week :width ,count) list))))
               ((#\H) (ecase count
                        ((1 2) (push `(hour :width ,count :range 24 :from-zero t) list))))
               ((#\K) (ecase count
                        ((1 2) (push `(hour :width ,count :range 12 :from-zero t) list))))
               ((#\k) (ecase count
                        ((1 2) (push `(hour :width ,count :range 24 :from-zero nil) list))))
               ((#\h) (ecase count
                        ((1 2) (push `(hour :width ,count :range 12 :from-zero nil) list))))
               ((#\a) (ecase count
                        ((1) (push `(meridian :format :abbreviated) list)))) ; ?
               ((#\m) (ecase count
                        ((1 2) (push `(minute :width ,count) list))))
               ((#\s) (ecase count
                        ((1 2) (push `(second :width ,count) list))))
               ((#\S) (ecase count
                        ((1) (push `(subsecond :precision :auto) list))
                        ((3) (push `(subsecond :precision :ms) list))
                        ((6) (push `(subsecond :precision :us) list))
                        ((9) (push `(subsecond :precision :ns) list))))
               ((#\x #\z) (ecase count
                        ((1) (push `(zone-offset :precision :hours :colons nil) list))
                        ((2) (push `(zone-offset :precision :minutes :colons nil) list))
                        ((3) (push `(zone-offset :precision :minutes :colons t) list))
                        ((4) (push `(zone-offset :precision :seconds :colons nil) list))
                        ((5) (push `(zone-offset :precision :seconds :colons t) list))))
               ((#\X #\Z) (ecase count
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
                 ((or (char<= #\0 char #\9) (> (char-code char) 127))
                  (push char list)
                  (parse-command (1+ pos)))
                 (t (error "unsupported format character ~:@C at ~D in ~S" char pos string)))))))
      (parse-command start)
      (nreverse list))))
                  
(defun time-format (pattern)
  (etypecase pattern
    (time-format pattern)
    (cons (make-time-format-1 pattern))
    (string (let ((compiled (with-lock-held (*formatter-cache-lock*)
                              (gethash pattern *formatter-cache*))))
              (or compiled
                  (let* ((commands (parse-timestamp-format-string pattern))
                         (format (make-time-format-1 commands :pattern pattern)))
                    (with-lock-held (*formatter-cache-lock*)
                      (or (gethash pattern *formatter-cache*)
                          (setf (gethash pattern *formatter-cache*) format)))))))))

(defun resolve-format (format locale)
  (etypecase format
    (time-format format)
    (string (time-format format))
    (cons (time-format format))
    (keyword (time-format (localized-timestamp-format format locale)))))

(defun print-timestamp (object
                        &key (stream *standard-output*) (zone *zone*)
                             (locale *locale*)
                             (format :medium-timestamp))
  (let* ((locale (resolve-calendar-symbols locale))
         (printer (resolve-format format locale)))
    (funcall (time-format-printer printer) object
             :stream stream :zone zone
             :locale locale)))
  
(defun format-timestamp (stream pattern object
                         &key (locale *locale*) (zone *zone*))
  (let ((locale (resolve-calendar-symbols locale))
        (formatter (resolve-format pattern locale)))
    (if stream
        (funcall (time-format-printer formatter) object
                 :stream (if (eq stream 't) *terminal-io* stream)
                 :locale locale :zone zone)
        (with-output-to-string (stream)
          (funcall (time-format-printer formatter) object
                   :stream stream
                   :locale locale :zone zone)))))

(setf *default-date-format*
      (time-format "yyyy-MM-dd"))

(setf *default-time-format*
      (time-format "HH:mm:ss.S"))

(setf *default-timestamp-format*
      (time-format "yyyy-MM-dd'T'HH:mm:ss.S"))



(defun trim-string (value)
  (string-trim #.(concatenate 'string '(#\space #\tab #\newline #\return))
               value))

(defun parse-string-vector (input)
  (labels
      ((spacep (char)
         (find char #.(concatenate 'string '(#\space #\tab #\newline #\return))))
       (collect-entry (position)
         (let ((buffer (make-array (- (length input) position) :element-type 'character :adjustable t :fill-pointer 0))
               (space-seen nil)
               (end (length input)))
           (loop
              while (< position end)
              do (let ((char (char input position)))
                   (cond
                     ((eql char #\,) (loop-finish))
                     ((spacep char)
                      (setf space-seen (plusp (fill-pointer buffer)))
                      (incf position))
                     ((eql char #\\)
                      (when space-seen (vector-push-extend #\space buffer))
                      (vector-push-extend (char input (1+ position)) buffer)
                      (setf space-seen nil)
                      (incf position 2))
                     (t
                      (when space-seen (vector-push-extend #\space buffer))
                      (vector-push-extend char buffer)
                      (setf space-seen nil)
                      (incf position)))))
           (values (coerce buffer 'simple-string)
                   (if (eql position end) nil (1+ position))))))
    (loop
       with position = 0 and list = nil and count = 0
       do (multiple-value-bind (elt npos) (collect-entry position)
            (push elt list)
            (incf count)
            (setf position npos)
            (unless position
              (return (make-array count
                                  :element-type 'string
                                  :initial-contents (reverse list))))))))
  
(defun parse-printer (input)
  (time-format (trim-string input)))

(define-locale-resource calendar-symbols
    ((months-narrow
       :property "months[narrow]" :type (array string (12)) :default *default-month-abbreviations*
       :parser parse-string-vector)
     (months-abbreviated
       :property "month[abbreviated]" :type (array string (12)) :default *default-month-abbreviations*
       :parser parse-string-vector)
     (months-wide
       :property "months[wide]" :type (array string (12)) :default *default-month-names*
       :parser parse-string-vector)
     (weekdays-narrow
       :property "weekdays[narrow]" :type (array string (7)) :default *default-weekday-abbreviations*
       :parser parse-string-vector)
     (weekdays-short
       :property "weekdays[short]" :type (array string (7)) :default *default-weekday-abbreviations*
       :parser parse-string-vector)
     (weekdays-abbreviated
       :property "weekdays[abbreviated]" :type (array string (7)) :default *default-weekday-abbreviations*
       :parser parse-string-vector)
     (weekdays-wide
       :property "weekdays[wide]" :type (array string (7)) :default *default-weekday-names*
       :parser parse-string-vector)
     (eras-narrow
       :property "eras[narrow]" :type (array string (2)) :default *default-era-designators*
       :parser parse-string-vector)
     (eras-abbreviated
       :property "eras[abbreviated]" :type (array string (2)) :default *default-era-designators*
       :parser parse-string-vector)
     (eras-wide
       :property "eras[wide]" :type (array string (2)) :default *default-era-designators*
       :parser parse-string-vector)
     (meridians-narrow
       :property "meridians[narrow]" :type (array string (2)) :default *default-meridian*
       :parser parse-string-vector)
     (meridians-abbreviated
       :property "meridians[abbreviated]" :type (array string (2)) :default *default-meridian*
       :parser parse-string-vector)
     (meridians-wide
       :property "meridians[wide]" :type (array string (2)) :default *default-meridian*
       :parser parse-string-vector)
     (date-format-short
       :property "date-format[short]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (date-format-medium
       :property "date-format[medium]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (date-format-long
       :property "date-format[long]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (date-format-full
       :property "date-format[full]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (time-format-short
       :property "time-format[short]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (time-format-medium
       :property "time-format[medium]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (time-format-long
       :property "time-format[long]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (time-format-full
       :property "time-format[full]" :type time-format :default *default-date-format*
       :parser parse-printer)
     (timestamp-template-short
       :property "timestamp-template[short]" :type string :default "{1}, {0}"
       :parser trim-string)
     (timestamp-template-medium
       :property "timestamp-template[medium]" :type string :default "{1}, {0}"
       :parser trim-string)
     (timestamp-template-long
       :property "timestamp-template[long]" :type string :default "{1}, {0}"
       :parser trim-string)
     (timestamp-template-full
       :property "timestamp-template[full]" :type string :default "{1}, {0}"
       :parser trim-string)
     (first-day-of-week
      :default +sunday+ :type (integer 0 6)
      :parser (lambda (str) (or (position (trim-string str) '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday) :test #'string-equal)
                                +inherit+))))
  (:conc-name calendar-)
  (:cache-function t))

(defun resolve-calendar-symbols (key)
  (typecase key
    (null nil)
    (locale (calendar-symbols key))
    (string (calendar-symbols (locale key)))
    (t key)))


(defmethod localized-month (month (locale locale) &optional (format :wide))
  (localized-month month (calendar-symbols locale) format))

(defmethod localized-weekday (month (locale locale) &optional (format :wide))
  (localized-weekday month (calendar-symbols locale) format))

(defmethod localized-meridian (month (locale locale) &optional (format :wide))
  (localized-meridian month (calendar-symbols locale) format))

(defmethod localized-era (month (locale locale) &optional (format :wide))
  (localized-era month (calendar-symbols locale) format))

(defmethod localized-beginning-of-week ((locale locale))
  (localized-beginning-of-week (calendar-symbols locale)))

(defmethod localized-timestamp-format (type (locale locale))
  (localized-timestamp-format type (calendar-symbols locale)))


(defmethod localized-month (month (locale calendar-symbols) &optional (format :wide))
  (let ((vector (ecase format
                  ((:narrow) (calendar-months-wide locale))
                  ((:short :abbreviated) (calendar-months-abbreviated locale))
                  ((:wide) (calendar-months-wide locale)))))
    (if (eq vector +inherit+) (call-next-method) (aref vector (1- month)))))

(defmethod localized-weekday (day (locale calendar-symbols) &optional (format :wide))
  (let ((vector (ecase format
                  ((:narrow) (calendar-weekdays-narrow locale))
                  ((:short) (calendar-weekdays-short locale))
                  ((:abbreviated) (calendar-weekdays-abbreviated locale))
                  ((:wide) (calendar-weekdays-wide locale)))))
    (if (eq vector +inherit+) (call-next-method) (aref vector day))))

(defmethod localized-meridian (value (locale calendar-symbols) &optional (format :wide))
  (let ((vector (ecase format
                  ((:short :narrow :abbreviated) (calendar-meridians-abbreviated locale))
                  ((:wide) (calendar-meridians-wide locale)))))
    (if (eq vector +inherit+) (call-next-method) (aref vector value))))

(defmethod localized-era (era (locale calendar-symbols) &optional (format :wide))
  (let ((vector (ecase format
                  ((:narrow) (calendar-eras-narrow locale))
                  ((:short :abbreviated) (calendar-eras-abbreviated locale))
                  ((:wide) (calendar-eras-wide locale)))))
    (if (eq vector +inherit+) (call-next-method) (aref vector era))))

(defmethod localized-beginning-of-week ((locale calendar-symbols))
  (let ((result (calendar-first-day-of-week locale)))
    (if (eq result +inherit+) (call-next-method) result)))

(defmacro unless-inherited ((&rest bindings) &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((var form) &rest more) bindings
        `(let ((,var ,form))
           (if (eq ,var +inherit+) +inherit+
               (unless-inherited (,@more) ,@body))))))

(defun derive-timestamp-format (locale property date-fmt time-fmt template-fmt)
  (let ((cached (property-value locale property)))
    (or cached
        (unless-inherited ((date-part (funcall date-fmt locale))
                           (time-part (funcall time-fmt locale))
                           (template-part (funcall template-fmt locale)))
          (let ((template-str (funcall template-fmt locale))
                (date-str (and date-part (time-format-pattern date-part)))
                (time-str (and time-part (time-format-pattern time-part))))
            (if (not (and date-str time-str template-str))
                ;; FIXME: Punting if we have no combined template string
                ;; is ok. Punting, if the date or the time format does not
                ;; have a pattern string is not. We should have a way to
                ;; combine the things anyway.
                (setf (property-value locale property) *default-timestamp-format*)
                (let* ((date-pos (search "{1}" template-str))
                       (time-pos (search "{0}" template-str))
                       (pattern (if (< date-pos time-pos)
                                    (concatenate 'string
                                                 (subseq template-str 0 date-pos)
                                                 date-str
                                                 (subseq template-str (+ date-pos 3) time-pos)
                                                 time-str
                                                 (subseq template-str (+ time-pos 3)))
                                    (concatenate 'string
                                                 (subseq template-str 0 time-pos)
                                                 time-str
                                                 (subseq template-str (+ time-pos 3) date-pos)
                                                 date-str
                                                 (subseq template-str (+ date-pos 3))))))
                  (setf (property-value locale property) (time-format pattern)))))))))

(defmethod localized-timestamp-format (type (locale calendar-symbols))
  (let ((result (case type
                  ((:short-date) (calendar-date-format-short locale))
                  ((:medium-date) (calendar-date-format-medium locale))
                  ((:long-date) (calendar-date-format-long locale))
                  ((:full-date) (calendar-date-format-full locale))
                  ((:short-time) (calendar-time-format-short locale))
                  ((:medium-time) (calendar-time-format-medium locale))
                  ((:long-time) (calendar-time-format-long locale))
                  ((:full-time) (calendar-time-format-full locale))
                  ((:short-timestamp) (derive-timestamp-format locale 'short-timestamp-format #'calendar-date-format-short #'calendar-time-format-short #'calendar-timestamp-template-short))
                  ((:medium-timestamp) (derive-timestamp-format locale 'medium-timestamp-format #'calendar-date-format-medium #'calendar-time-format-medium #'calendar-timestamp-template-medium))
                  ((:long-timestamp) (derive-timestamp-format locale 'long-timestamp-format #'calendar-date-format-long #'calendar-time-format-long #'calendar-timestamp-template-long))
                  ((:full-timestamp) (derive-timestamp-format locale 'full-timestamp-format #'calendar-date-format-full #'calendar-time-format-full #'calendar-timestamp-template-full))
                  (otherwise +inherit+))))
    (if (eq result +inherit+) (call-next-method) result)))

(add-locale-resource-directory (asdf:system-relative-pathname '#:darts.lib.calendar "./data/"))

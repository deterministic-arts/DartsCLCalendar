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

(defvar *zone* nil
  "The default time zone. Note, that the value of this variable should
   always be a proper instance of `zone`, never a designator.")

(defclass zone () ()
  (:documentation "Base class for time zone implementations. This class
    is primarily intended to type discrimination purpose. It does not
    provide any useful functionality of its own."))

(defgeneric compute-zone-offset (temporal zone)
  (:documentation "Compute the offset in seconds to use for the given
    temporal object. The value passed as `temporal` may either be an 
    instant, or a local date/time representation."))

(defgeneric zone-identifier (object)
  (:documentation "Answers a value, which uniquely (and unambiguously)
    identifies the time zone. The library uses the value returned from 
    this method to determine, whether two `zone` implementations actually
    refer to the same time zone, by comparing the values returned from 
    this method using `equal`.

    The canonical form of a time zone identifier is a list, whose first
    element is a symbol, that describes the type of the time zone. The
    remaining elements provide additional parameters (such as zone offsets,
    region identifiers, etc.), and must be interpreted with respect to
    the zone type.

    Note, that there is a strong expectation, that zone identifiers
    can be given to the `zone` function, and the result should be a
    conceptually equal time zone."))

(defun same-zone-p (z1 z2)
  "Answers true, if `z1` and `z2` represent the same time zone."
  (or (eq z1 z2)
      (equal (zone-identifier z1) (zone-identifier z2))))


(defclass utc-zone (zone) ()
  (:documentation "The time zone implementation, which represents UTC."))

(defparameter +utc-zone+ (make-instance 'utc-zone))

(defun utc-zone ()
  "Answers a time zone implementation, which represents the UTC time zone."
  +utc-zone+)

(defmethod compute-zone-offset (temporal (zone utc-zone))
  (values 0 nil "UTC"))

(defmethod zone-identifier ((zone utc-zone))
  '(:utc))

;;; This is only so, that we have a meaningful default. It should be overridden
;;; later

(setf *zone* +utc-zone+)


(defvar *fixed-zone-lock* (make-lock "Fixed offset zones lock"))
(defvar *fixed-zone-cache* (make-weak-hash-table :test 'eql :weakness :value))

(defclass fixed-zone (zone)
  ((offset
     :type (integer -86399 86399) :initarg :offset
     :reader fixed-zone-offset)))

(defmethod compute-zone-offset (temporal (zone fixed-zone))
  (values (fixed-zone-offset zone) nil nil))

(defmethod zone-identifier ((object fixed-zone))
  (list :offset (fixed-zone-offset object)))

(defmethod print-object ((object fixed-zone) stream)
  (let* ((seconds (fixed-zone-offset object))
         (sign (if (minusp seconds) #\- #\+)))
    (multiple-value-bind (hours minutes) (floor (floor (abs seconds) 60) 60)
      (print-unreadable-object (object stream :type t :identity nil)
        (format stream "~A~2,'0D:~2,'0D" sign hours minutes)))))

(defun make-fixed-zone (offset)
  (cond
    ((zerop offset) +utc-zone+)
    ((not (typep offset '(integer -86399 86399))) (error 'type-error :datum offset :expected-type '(integer -86399 86399)))
    (t (with-lock-held (*fixed-zone-lock*)
         (let ((present (gethash offset *fixed-zone-cache*)))
           (or present
               (let ((object (make-instance 'fixed-zone :offset offset)))
                 (setf (gethash offset *fixed-zone-cache*) object)
                 object)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *zone-factory-lock* (make-lock "Zone factory table lock"))
  (defvar *zone-factories* (make-hash-table :test 'eql))

  (defun known-zone-constructor-symbol-p (object)
    (and (symbolp object)
         (or (member object '(t nil))
             (with-lock-held (*zone-factory-lock*)
               (and (gethash object *zone-factories*)
                    t))))))

(deftype known-zone-constructor-symbol ()
  '(and symbol (satisfies known-zone-constructor-symbol-p)))

(defun make-complex-zone (type parameters)
  (let ((factory (with-lock-held (*zone-factory-lock*)
                   (gethash type *zone-factories*))))
    (if (not factory)
        (error 'simple-type-error
               :datum type :expected-type 'known-zone-constructor-symbol
               :format-control "~S is not the name of a known time zone constructor"
               :format-arguments (list type))
        (apply factory parameters))))
  
(defun zone (designator)
  "Coerce the given `designator` into an instance of class `zone`, or signal
   a condition (preferably a `type-error`), if this is not possible. The `designator`
   may be one of

    - a `zone` instance, which is returned unchanged
    - `nil`, which is expanded into the UTC zone
    - `t`, which is expanded into the value of `*zone*`
    - a constructor expression for a time zone

   Constructor expressions are lists of the form `(type arg1 ...)`, where
   `type` is a symbol, naming one of the time zone constructors defined via the
   `define-zone-constructor` operator, and the remaining elements are parameters,
   which are passed to the registered constructor function.

   The following zone constructors are pre-defined:

    - `(:utc)` the UTC zone
    - `(:offset <offs>)` a fixed offset time zone
    - `(:military <spec>)` military time zone abbreviation for fixed offset time zones
    - `(:region <region> <spot>)` time zone of the given location"

  (typecase designator
    (zone designator)
    (null +utc-zone+)
    ((eql t) *zone*)
    (symbol (make-complex-zone designator nil))
    ((cons symbol list) (make-complex-zone (car designator) (cdr designator)))
    (t (error 'simple-type-error
              :datum designator :expected-type '(or zone symbol (cons symbol list))
              :format-control "~S is not a well-formed time zone designator"
              :format-arguments (list designator)))))

(defmethod compute-zone-offset (object zone)
  (compute-zone-offset object (zone zone)))

(defmethod zone-identifier (object)
  (zone-identifier (zone object)))

(defmacro define-zone-constructor (name lambda-list &body body)
  "Defines a new constructor symbol for time zones. The given `name` (a symbol)
   becomes defined as zone constructor symbol. The constructor arguments are 
   interpreted according to the `lambda-list` (an ordinary lambda list) and
   may be accessed during the execution of `body`. The `body` forms must produce
   a single value, an instance of `zone`."
  (unless (and (symbolp name) (not (member name '(t nil))))
    (error "~S is not a valid time zone constructor name" name))
  `(with-lock-held (*zone-factory-lock*)
     (setf (gethash ',name *zone-factories*) (lambda ,lambda-list ,@body))
     ',name))

(defmacro with-zone (form &body body)
  "Binds `*zone*` to the time zone result of evaluating `form`, then evaluates
   all forms in `body` sequentially in the manner of `progn`, returning the values
   produced by the last form in `body`. The result of `form` is coerced into a 
   `zone` instance by passing it through function `zone` before it is being bound
   to `*zone*`."
  `(let ((*zone* (zone ,form))) ,@body))


(define-zone-constructor :utc (&rest params)
  (if (not (null params))
      (error "the constructor ~S does not take any parameters" :utc)
      +utc-zone+))

(define-zone-constructor :offset (&rest params)
  (if (not (typep params '(cons (integer -86399 86399) null)))
      (error "the constructor ~S takes a single argument, which must be a UTC offset" :offset)
      (make-fixed-zone (first params))))

(define-zone-constructor :military (&rest params)
  (if (or (null params) (not (null (cdr params))))
      (error "the constructor ~S takes a single argument" :military)
      (let ((head (car params)))
        (if (not (or (stringp head) (symbolp head) (characterp head)))
            (error "the argument ~S to constructor ~S is not a string designator" head :military)
            (let ((canonical (string-upcase head)))
              (if (not (eql (length canonical) 1))
                  (error "the argument ~S to constructor ~S is not a single latin letter" head :military)
                  (let ((nth (- (char-code (char canonical 0)) #.(char-code #\A))))
                    (cond
                      ((<= 0 nth 8) (make-fixed-zone (* 3600 (1+ nth))))
                      ((<= 10 nth 12) (make-fixed-zone (* 3600 nth)))
                      ((<= 13 nth 24) (make-fixed-zone (* 3600 (+ 12 (- nth)))))
                      ((= 25 nth) +utc-zone+)
                      ((= 9 nth) *zone*)
                      (t (error "the argument ~S to constructor ~S is not understood"
                                head :military))))))))))

(define-zone-constructor :region (&rest params)
  (error "sorry, not yet implemented: ~S" (cons :region params)))

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

(defpackage #:darts.lib.calendar
  (:use #:common-lisp #:alexandria #:bordeaux-threads #:trivial-garbage #:darts.lib.locale)
  (:export
     #:*clock* #:*zone* #:+friday+ #:+max-local-date+ #:+max-local-time+
     #:+max-local-timestamp+ #:+max-local-year+ #:+min-local-date+
     #:+min-local-time+ #:+min-local-timestamp+ #:+min-local-year+ #:+monday+
     #:+saturday+ #:+sunday+ #:+sunday+ #:+thursday+ #:+tuesday+ #:+wednesday+
     #:add-duration #:add-seconds #:add-time-unit #:clock-now
     #:compile-timestamp-printer #:compile-timestamp-printer-pattern
     #:compose-local-timestamp #:compute-zone-offset #:days-in-month
     #:decode-epoch-seconds #:define-zone-constructor #:duration #:duration-between
     #:duration-hash #:duration-hash #:duration-minusp #:duration-nanos
     #:duration-plusp #:duration-seconds #:duration-zerop #:duration/= #:duration<
     #:duration<= #:duration= #:duration> #:duration>= #:durationp
     #:encode-epoch-seconds #:format-timestamp #:instant #:instant-equal
     #:instant-hash #:instant-nanos #:instant-seconds #:instant-to-posix-time
     #:instant-to-universal-time #:instant/= #:instant< #:instant<= #:instant=
     #:instant> #:instant>= #:instantp #:leap-year-p #:local-date #:local-date-day
     #:local-date-equal #:local-date-hash #:local-date-month #:local-date-p
     #:local-date-weekday #:local-date-year #:local-date/= #:local-date<
     #:local-date<= #:local-date= #:local-date> #:local-date>= #:local-day
     #:local-day-of-year #:local-hour #:local-iso-week&year #:local-iso-week-number
     #:local-iso-week-year #:local-iso-weekday #:local-microsecond
     #:local-millisecond #:local-minute #:local-month #:local-nanos
     #:local-nanosecond #:local-next-day-of-week #:local-previous-day-of-week
     #:local-second #:local-time #:local-time-equal #:local-time-hash
     #:local-time-hour #:local-time-microsecond #:local-time-millisecond
     #:local-time-minute #:local-time-nanos #:local-time-nanosecond #:local-time-p
     #:local-time-second #:local-time/= #:local-time< #:local-time<= #:local-time=
     #:local-time> #:local-time>= #:local-timestamp #:local-timestamp-date
     #:local-timestamp-equal #:local-timestamp-hash #:local-timestamp-p
     #:local-timestamp-time #:local-timestamp/= #:local-timestamp<
     #:local-timestamp<= #:local-timestamp= #:local-timestamp> #:local-timestamp>=
     #:local-weekday #:local-year #:localized-beginning-of-week
     #:localized-meridian #:localized-month-abbreviation #:localized-month-name
     #:localized-timestamp-format #:localized-weekday-abbreviation
     #:localized-weekday-name #:make-duration #:make-instant #:make-local-date
     #:make-local-time #:make-local-timestamp #:negate-duration #:now
     #:parse-timestamp-format-string #:posix-time-to-instant #:print-timestamp
     #:same-zone-p #:scale-duration #:subtract-duration #:subtract-time-unit
     #:universal-time-to-instant #:with-zone #:zone #:zone-identifier))

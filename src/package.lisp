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
  (:use #:common-lisp #:alexandria #:bordeaux-threads #:trivial-garbage)
  (:export #:local-date #:local-date-year #:local-date-month #:local-date-day
           #:local-date-weekday #:local-date-p #:local-date< #:local-date<=
           #:local-date>= #:local-date> #:local-date= #:local-date/= #:local-date-hash
           #:local-date-equal #:local-time #:local-time-hour #:local-time-minute
           #:local-time-second #:local-time-millisecond #:local-time-microsecond
           #:local-time-nanosecond #:local-time-nanos #:local-time-p #:local-time<
           #:local-time<= #:local-time>= #:local-time> #:local-time= #:local-time/=
           #:local-time-hash #:local-time-equal #:local-timestamp #:local-timestamp-date
           #:local-timestamp-time #:local-timestamp-p #:local-timestamp< #:local-timestamp<=
           #:local-timestamp>= #:local-timestamp> #:local-timestamp= #:local-timestamp/=
           #:local-timestamp-hash #:local-timestamp-equal #:local-year #:local-month
           #:local-day #:local-weekday #:local-hour #:local-minute #:local-second
           #:local-millisecond #:local-microsecond #:local-nanosecond #:local-nanos
           #:instant #:instantp #:instant< #:instant<= #:instant>= #:instant> #:instant/=
           #:instant= #:instant-equal #:instant-hash #:+sunday+ #:+monday+ #:+tuesday+
           #:+wednesday+ #:+thursday+ #:+friday+ #:+saturday+ #:+sunday+ #:make-local-date
           #:make-local-time #:make-local-timestamp #:*zone* #:zone #:zone-identifier
           #:define-zone-constructor #:with-zone #:*clock* #:clock-now #:now
           #:encode-epoch-seconds #:decode-epoch-seconds #:instant-to-universal-time
           #:universal-time-to-instant #:instant-to-posix-time #:posix-time-to-instant
           #:make-instant #:duration #:duration-seconds #:duration-nanos #:duration=
           #:duration< #:duration<= #:duration>= #:duration> #:duration/= #:duration-hash
           #:duration-hash #:duration-plusp #:duration-minusp #:duration-zerop #:scale-duration
           #:negate-duration #:durationp #:add-seconds #:add-duration #:subtract-duration
           #:add-time-unit #:subtract-time-unit #:duration-between #:local-iso-weekday
           #:local-day-of-year #:local-iso-week&year #:local-iso-week-year #:local-iso-week-number
           #:local-previous-day-of-week #:local-next-day-of-week #:localized-month-name
           #:localized-month-abbreviation #:localized-weekday-name #:localized-weekday-abbreviation
           #:localized-meridian #:localized-timestamp-format #:localized-beginning-of-week
           #:print-timestamp #:format-timestamp #:compile-timestamp-printer #:compile-timestamp-printer-pattern
           #:parse-timestamp-format-string #:same-zone-p #:compute-zone-offset #:instant-seconds
           #:instant-nanos #:days-in-month #:leap-year-p #:compose-local-timestamp
           #:+min-local-date+ #:+max-local-date+ #:+min-local-time+ #:+max-local-time+
           #:+min-local-timestamp+ #:+max-local-timestamp+ #:+min-local-year+
           #:+max-local-year+

           ;; This one does not belong here!
           #:*default-locale*
           ))

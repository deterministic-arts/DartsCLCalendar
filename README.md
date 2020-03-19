 
# Darts Date/Time Library

This repository contains a small date/time library, which (given time) hopefully
evolves into something, which can replace the venerable `local-time` library. It
incorporates ideas found in `local-time` as well as other places, such as (in no
particular order):

 - the [Joda date/time][joda] library for java
 - the new [java8 date/time][java8-time] library
 - E. Naggum's classic essay "[The Long, Painful History of Time][naggum]"
 - H. Hinnant's [C++ date/time proposal][hinnant]

**This library is work in progress** It is currently somewhat usable as a `local-time` 
replacement for limited use cases. Proper time zone support still requires `local-time`
since we currently abuse local time's support for compiled time zone files. 
Formatting date/time values is more flexible than in `locale-time` due to the
support for internationalization and locale-specific date/time patterns.

## Date/Time Representations

This library strongly distinguishes between "instants", which are points on
the conceptual time line, and "local" representations, which can be thought
of as symbolic labels for instants. All local representations need to be interpreted
in the context of a time zone to make any sense.

Instants are measured in nanoseconds since the epoch, which this library defines to 
be at midnight, March 1st 2000 UTC. Internally, instants use two fields to store this 
offset: the full number of seconds from the epoch, which is a signed quantity (negative 
values are before the epoch, positive values represent instants after the epoch), and 
the number of nanoseconds, which is always in the range 0 to 999,999,999.

The number of seconds since epoch is stored as an `(integer -31557015119088000 31556888912534399)`,
(which happens to be a subtype of `fixnum` on my machine on CCL as well as on SBCL).
The boundaries represent the points

 - `-31557015119088000` &rarr; `-1000000000-01-01T00:00:00Z`
 - `31556888912534399` &rarr; `1000000000-12-31T23:59:59Z`

On 32 bit Lisps, these values are probably not `fixnum`s, but hey, 64 bit is
the future, you know... :-)

The local representations are stored using the individual date/time fields.
The following types are defined:

 - `local-date`, which holds `year`, `month`, `day`, and `weekday`
 - `local-time`, which holds `hour`, `minute`, `second`, and `nanos`
 - `local-timestamp`, which is a combination of the above

Since the purpose of the local representations is to extract the date/time
fields, I decided not to encode these values in some fancy way, but store
them in expanded form, so that reading the values is fast.

## Durations and Arithmetic

The library provides a type `duration`, which represents the (signed) difference
between two points in time, measured in nanoseconds. Internally, this is stored
again by splitting the value into full seconds (signed), and the number of nanoseconds
in the range 0 to 999,999,999.

## Dictionary

### Durations

 - Type `duration`
 - Function `make-duration` `&key` _weeks_ _days_ _hours_ _minutes_ _seconds_ _milliseconds_ _microseconds_ _nanoseconds_ &rarr; _duration_
 - Function `scale-duration` _duration_ _factor_ &rarr; _new-duration_
 - Function `negate-duration` _duration_ &rarr; _new-duration_
 - Function `durationp` _object_ &rarr; _boolean_
 - Function `duration-minusp` _object_ &rarr; _boolean_
 - Function `duration-zerop` _object_ &rarr; _boolean_
 - Function `duration-plusp` _object_ &rarr; _boolean_ 
 - Function `duration-hash` _object_ &rarr; _value_
 - Function `duration-nanos` _object_ &rarr; _value_
 - Function `duration-seconds` _object_ &rarr; _value_
 - Function `duration=` _object1_ _object2_ &rarr; _boolean_
 - Function `duration/=` _object1_ _object2_ &rarr; _boolean_
 - Function `duration<` _object1_ _object2_ &rarr; _boolean_
 - Function `duration<=` _object1_ _object2_ &rarr; _boolean_
 - Function `duration>=` _object1_ _object2_ &rarr; _boolean_
 - Function `duration>` _object1_ _object2_ &rarr; _boolean_

### Time Zones

 - Variable `*zone*`
 - Class `zone`
 - Function `zone` _designator_ &rarr; _zone_
 - Function `compute-zone-offset` _object_ _zone_ &rarr; _offset_ _period_ _abbreviation_
 - Function `zone-identifier` _zone_ &rarr; _object_
 - Function `same-zone-p` _zone1_ _zone2_ &rarr; _boolean_
 - Macro `define-zone-constructor` _name_ _lambda-list_ `&body` _body_
 - Macro `with-zone` _form_ `&body` _body_

### Instants

Conceptual points on the abstract time line with nano-second resolution. Instants
are measured in nanoseconds since the epoch, which is defined as midnight, March
1st 2000.
 
 - Type `instant`
 - Function `instant` _object_ `&key` _zone_ _defaults_ &rarr; _instant_ 
 - Function `instant-hash` _object_ &rarr; _value_
 - Function `instant-nanos` _object_ &rarr; _value_
 - Function `instant-seconds` _object_ &rarr; _value_
 - Function `instant=` _object1_ _object2_ &rarr; _boolean_
 - Function `instant/=` _object1_ _object2_ &rarr; _boolean_
 - Function `instant<` _object1_ _object2_ &rarr; _boolean_
 - Function `instant<=` _object1_ _object2_ &rarr; _boolean_
 - Function `instant>=` _object1_ _object2_ &rarr; _boolean_
 - Function `instant>` _object1_ _object2_ &rarr; _boolean_

### Local Dates

Pure local dates (i.e., dates without time-of-day information). 

 - Type `local-date`
 - Function `local-date` _object_ `&key` _zone_ _defaults_ &rarr; _date_ 
 - Function `make-local-date` `&key` _year_ _month_ _day_ _defaults_ &rarr; _value_
 - Function `local-date-p` _object_ &rarr; _boolean_
 - Function `local-date-day` _object_ &rarr; _value_
 - Function `local-date-hash` _object_ &rarr; _value_
 - Function `local-date-month` _object_ &rarr; _value_
 - Function `local-date-weekday` _object_ &rarr; _value_
 - Function `local-date-year` _object_ &rarr; _value_
 - Function `local-date=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-date/=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-date<` _object1_ _object2_ &rarr; _boolean_
 - Function `local-date<=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-date>=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-date>` _object1_ _object2_ &rarr; _boolean_

### Local Date/Times

A combination of a local date and a local time.

 - Type `local-timestamp`
 - Function `local-timestamp` _object_ `&key` _zone_ _defaults_ &rarr; _date-time_ 
 - Function `make-local-timestamp` `&key` _year_ _month_ _day_ _hour_ _minute_ _second_ _nanos_ _millisecond_ _microsecond_ _nanosecond_ _defaults_ &rarr; _value_
 - Function `local-timestamp-p` _object_ &rarr; _boolean_
 - Function `local-timestamp-date` _object_ &rarr; _value_
 - Function `local-timestamp-time` _object_ &rarr; _value_
 - Function `local-timestamp=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-timestamp/=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-timestamp<` _object1_ _object2_ &rarr; _boolean_
 - Function `local-timestamp<=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-timestamp>=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-timestamp>` _object1_ _object2_ &rarr; _boolean_

### Local Times

Pure local times. These are always represented with (potentially) nanosecond
resolution.

 - Type `local-time`
 - Function `local-time` _object_ `&key` _zone_ _defaults_ &rarr; _time_
 - Function `make-local-time` `&key` _hour_ _minute_ _second_ _nanos_ _millisecond_ _microsecond_ _nanosecond_ &rarr; _value_
 - Function `local-time-p` _object_ &rarr; _boolean_
 - Function `local-time-hour` _object_ &rarr; _value_
 - Function `local-time-minute` _object_ &rarr; _value_
 - Function `local-time-second` _object_ &rarr; _value_
 - Function `local-time-microsecond` _object_ &rarr; _value_
 - Function `local-time-millisecond` _object_ &rarr; _value_
 - Function `local-time-nanosecond` _object_ &rarr; _value_
 - Function `local-time-nanos` _object_ &rarr; _value_
 - Function `local-time=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-time/=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-time<` _object1_ _object2_ &rarr; _boolean_
 - Function `local-time<=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-time>=` _object1_ _object2_ &rarr; _boolean_
 - Function `local-time>` _object1_ _object2_ &rarr; _boolean_

### Arithmetic

These functions all return a temporal object of the same type as the
first argument. Internally, they all boil down to calls of `add-seconds`
with suitably massaged arguments.

 - Function `add-duration` _target_ _duration_ &rarr; _value_
 - Function `add-seconds` _target_ _seconds_ `&optional` _nanos_ &rarr; _value_
 - Function `add-time-unit` _target_ _amount_ _unit_ &rarr; _value_
 - Function `subtract-duration` _target_ _duration_ &rarr; _value_
 - Function `subtract-seconds` _target_ _seconds_ `&optional` _nanos_ &rarr; _value_
 - Function `subtract-time-unit` _target_ _amount_ _unit_ &rarr; _value_

### Conversions

 - Function `instant-to-universal-time` _instant_ &rarr; _value_
 - Function `universal-time-to-instant` _integer_ `&optional` _nanos_ &rarr; _instant_
 - Function `instant-to-posix-time` _instant_ &rarr; _value_ 
 - Function `posix-time-to-instant` _integer_ `&optional` _nanos_ &rarr; _instant_
 
### Generic Accessors

These accessor functions are generally defined for all instances of the
`local-xxx` variety. When an accessor is applied to a type, for which it is
not really applicable (say, `local-year` applied to a `local-time`), the
value is generally defaulted to the corresponding value from the full "epoch"
`local-timestamp` value (i.e., 2000-03-01T00:00:00.0Z)
 
 - Function `local-year` _object_ &rarr; _value_
 - Function `local-month` _object_ &rarr; _value_
 - Function `local-day` _object_ &rarr; _value_
 - Function `local-weekday` _object_ &rarr; _value_
 - Function `local-hour` _object_ &rarr; _value_
 - Function `local-minute` _object_ &rarr; _value_
 - Function `local-second` _object_ &rarr; _value_
 - Function `local-millisecond` _object_ &rarr; _value_
 - Function `local-microsecond` _object_ &rarr; _value_
 - Function `local-nanosecond` _object_ &rarr; _value_
 - Function `local-nanos` _object_ &rarr; _value_
 - Function `local-iso-weekday` _object_ &rarr; _value_
 - Function `local-day-of-year` _object_ &rarr; _value_
 - Function `local-iso-week&year` _object_ &rarr; _week_ _year_
 - Function `local-iso-week-number` _object_ &rarr; _value_
 - Function `local-iso-week-year` _object_ &rarr; _value_

### Utilities

 - Function `leap-year-p` _integer_ &rarr; _boolean_
 - Function `days-in-month` _year_ _month_ &rarr; _count_
 - Function `decode-epoch-seconds` _value_ &rarr; _year_ _month_ _day_ _hour_ _minute_ _second_ _weekday_
 - Function `encode-epoch-seconds` _year_ _month_ _day_ _hour_ _minute_ _second_ &rarr; _value_
 
### Formatting

 - Type `time-format`
 - Function `time-format` _object_ &rarr; _value_
 - Function `time-format-pattern` _object_ &rarr; _value_
 - Function `time-format-expression` _object_ &rarr; _value_
 - Function `format-timestamp` _stream_ _pattern_ _object_ `&key` _locale_ _zone_ &rarr; _result_
 - Function `print-timestamp` _object_ `&key` _stream_ _zone_ _locale_ _format_ &rarr; _object_
 
## Internationalization

This library ships with a bunch of localization files for various locales;
this includes localized month names, names for the days of the week, date
and time formatting patterns, etc. These localization files have been 
generated from the [CLDR database][CLDR] by an [external tool][l10ntool].
The update process right now is "semi-automatic" and involves me downloading
new versions of the database, extracting the files, and starting the 
conversion tool.

A few locales are not currently supported (sorry!) namely all those, which
use numeric codes to represent the country/territory in their identifiers
in the CLDR database.
 
## Random notes

 - Goal: provide clean date/time values representations; in particular, provide a
   clean distinction between "a unique point on the conceptual time line", and
   "localized labels for points on the time line"

 - the names chosen here to represent those concepts heavily borrow from the
   new java8 library, which I consider to be quite well-designed. Also, the
   internal representation of the `instant` type (a point on the time line)
   borrows heavily from the java8 representation

 - unlike the java8 implementation, this library uses an epoch, which makes
   some computations easier (namely midnight, March 1st 2000). This is an
   implementation detail, though. Conversion to and from POSIX timestamps and
   Lisp's universal time values is simple, and the library provides utility
   functions, to translate from/to both alternative representations.

Non-goals:

 - this library will probably never provide any calendar system beyond the ISO
   one ("gregorian"). I simply do not know anything about other calendar systems,
   and designing in support for something, I don't know anything about, is simply
   beyond my capabilities. If it can be done without imposing too much of a burden
   on the core parts, I may consider it later, but right now, I don't plan to add
   any support for non-Gregorian calendars.

[joda]: http://joda-time.sourceforge.net/quickstart.html
[java8-time]: http://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
[naggum]: http://naggum.no/lugm-time.html
[hinnant]: http://howardhinnant.github.io/date_algorithms.html
[CLDR]: http://cldr.unicode.org/index/downloads
[l10ntool]: https://github.com/deterministic-arts/DartsCLLocalization

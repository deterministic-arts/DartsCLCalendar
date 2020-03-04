
(in-package #:darts.lib.calendar-test)

(defun make-list-lessp (&optional (predicates #'<))
  (lambda (list1 list2)
    (labels
        ((relation (elt1 elt2 tests)
           (let ((pred (if (consp tests) (car tests) tests)))
             (cond
               ((funcall pred elt1 elt2) -1)
               ((funcall pred elt2 elt1) 1)
               (t 0))))
         (recurse (list1 list2 tests)
           (cond
             ((null list1) (not (null list2)))
             ((null list2) nil)
             (t (let ((rel (relation (car list1) (car list2) tests)))
                  (ecase rel
                    ((-1) t)
                    ((1) nil)
                    ((0) (recurse (cdr list1) (cdr list2) (if (consp tests) (cdr tests) tests)))))))))
      (recurse list1 list2 predicates))))

(in-root-suite)

(defsuite local-date-representations-suite)
(defsuite local-time-representations-suite)
(defsuite local-timestamp-representations-suite)

(in-suite local-date-representations-suite)
    
(deftest local-date-generic-getters ()
  (let ((data `((1976 1 13 ,+tuesday+)
                (2000 3 1 ,+wednesday+)
                (2038 12 31 ,+friday+))))
    (loop
       for (year month day weekday) in data
       for date = (make-local-date year month day)
       do (is (eql year (local-date-year date)))
          (is (eql year (local-year date)))
          (is (eql month (local-date-month date)))
          (is (eql month (local-month date)))
          (is (eql day (local-date-day date)))
          (is (eql day (local-day date)))
          (is (eql weekday (local-date-weekday date)))
          (is (eql weekday (local-weekday date)))
          (is (eql 0 (local-hour date)))
          (is (eql 0 (local-minute date)))
          (is (eql 0 (local-second date)))
          (is (eql 0 (local-nanos date)))
          (is (eql 0 (local-millisecond date)))
          (is (eql 0 (local-microsecond date)))
         (is (eql 0 (local-nanosecond date))))))

(deftest local-date-last-day-of-month-constructor ()
  (is (local-date= (make-local-date 2020 2 29) (make-local-date 2020 2 :last)))
  (is (local-date= (make-local-date 2021 2 28) (make-local-date 2021 2 :last)))
  (loop
     for month in '( 1  3  4  5  6  7  8  9 10 11 12)
     for count in '(31 31 30 31 30 31 31 30 31 30 31)
     do (is (local-date= (make-local-date 2020 month count) (make-local-date 2020 month :last)))))

(deftest local-date-ordering ()
  (let ((listlessp (make-list-lessp))
        (data `((1976 1 13) (3192 4 17) (651 9 30) (2000 3 1)
                (2038 12 31) (-3321 12 31))))
      (loop
         for outer in data
         for outer-date = (apply #'make-local-date outer)
         do (loop
               for inner in data
               for inner-date = (apply #'make-local-date inner)
               do (let ((o<i (local-date< outer-date inner-date))
                        (o<=i (local-date<= outer-date inner-date))
                        (o>=i (local-date>= outer-date inner-date))
                        (o>i (local-date> outer-date inner-date))
                        (o=i (local-date= outer-date inner-date))
                        (o/=i (local-date/= outer-date inner-date)))
                    (is (eql 1 (+ (if o<i 1 0) (if o=i 1 0) (if o>i 1 0))))
                    (cond
                      ((funcall listlessp outer inner)
                       (is o<i) (is o<=i) (is o/=i) (is (not o>i)) (is (not o>=i)) (is (not o=i)))
                      ((funcall listlessp inner outer)
                       (is o>i) (is o>=i) (is o/=i) (is (not o<i)) (is (not o<=i)) (is (not o=i)))
                      (t
                       (is (eql (local-date-hash outer-date) (local-date-hash inner-date)))
                       (is (not o>i)) (is o>=i) (is (not o/=i)) (is (not o<i)) (is o<=i)
                       (is o=i))))))))



(in-suite local-time-representations-suite)

(deftest local-time-subsecond-constructor ()
  (let ((data '((111222333 nil 111 222 333)
                (111222333 111222333 nil nil nil)
                (1 nil nil nil 1)
                (1000 nil nil 1 nil)
                (1000000 nil 1 nil nil)
                (1 1 10 20 30))))
    (loop
       for (expected-nanos ns milli micro nano) in data
       for date = (make-local-time 0 0 0 :nanos ns :millisecond milli :microsecond micro :nanosecond nano)
       do (is (eql expected-nanos (local-time-nanos date))))))

(deftest local-time-generic-getters ()
  (let ((data `((0 0 0 0 0 0 0)
                (0 0 0 1 0 0 1)
                (0 0 0 999 0 0 999)
                (0 0 0 999999 0 999 999)
                (0 0 0 999999999 999 999 999)
                (23 59 59 999999999 999 999 999))))
    (loop
       for (hour minute second nanos millisecond microsecond nanosecond) in data
       for date = (make-local-time hour minute second :nanos nanos)
       do (is (eql hour (local-time-hour date)))
          (is (eql hour (local-hour date)))
          (is (eql minute (local-time-minute date)))
          (is (eql minute (local-minute date)))
          (is (eql second (local-time-second date)))
          (is (eql second (local-second date)))
          (is (eql nanos (local-time-nanos date)))
          (is (eql nanos (local-nanos date)))
          (is (eql millisecond (local-time-millisecond date)))
          (is (eql millisecond (local-millisecond date)))
          (is (eql microsecond (local-time-microsecond date)))
          (is (eql microsecond (local-microsecond date)))
          (is (eql nanosecond (local-time-nanosecond date)))
          (is (eql nanosecond (local-nanosecond date)))
          (is (eql 2000 (local-year date)))
          (is (eql 3 (local-month date)))
          (is (eql 1 (local-day date)))
          (is (eql +wednesday+ (local-weekday date))))))
          
(deftest local-time-ordering ()
  (let ((listlessp (make-list-lessp))
        (data `((0 0 0 0) (23 59 59 999999999) (12 0 0 0) (12 0 0 1) (12 0 0 999999999))))
      (loop
         for outer in data
         for outer-date = (make-local-time (car outer) (cadr outer) (caddr outer) :nanos (cadddr outer))
         do (loop
               for inner in data
               for inner-date = (make-local-time (car inner) (cadr inner) (caddr inner) :nanos (cadddr inner))
               do (let ((o<i (local-time< outer-date inner-date))
                        (o<=i (local-time<= outer-date inner-date))
                        (o>=i (local-time>= outer-date inner-date))
                        (o>i (local-time> outer-date inner-date))
                        (o=i (local-time= outer-date inner-date))
                        (o/=i (local-time/= outer-date inner-date)))
                    (is (eql 1 (+ (if o<i 1 0) (if o=i 1 0) (if o>i 1 0))))
                    (cond
                      ((funcall listlessp outer inner)
                       (is o<i) (is o<=i) (is o/=i) (is (not o>i)) (is (not o>=i)) (is (not o=i)))
                      ((funcall listlessp inner outer)
                       (is o>i) (is o>=i) (is o/=i) (is (not o<i)) (is (not o<=i)) (is (not o=i)))
                      (t
                       (is (eql (local-time-hash outer-date) (local-time-hash inner-date)))
                       (is (not o>i)) (is o>=i) (is (not o/=i)) (is (not o<i)) (is o<=i)
                       (is o=i))))))))


(in-suite local-timestamp-representations-suite)

(deftest local-timestamp-ordering ()
  (let* ((listlessp (make-list-lessp (list #'local-date< #'local-time<)))
         (all-dates (mapcar (lambda (list) (apply #'make-local-date list))
                            '((1976 1 13) 
                              (2038 12 31)
                              (-3321 12 31))))
         (all-times (mapcar (lambda (list) (apply #'make-local-time list))
                            '((0 0 0 :nanos 0)
                              (12 13 14 :nanos 15016017)
                              (23 59 59 :nanos 999999999))))
         (data (loop
                  for date in all-dates
                  nconcing (loop
                              for time in all-times
                              collecting (list date time)))))
    (loop
       for outer in data
       for outer-date = (compose-local-timestamp (car outer) (cadr outer))
       do (loop
             for inner in data
             for inner-date = (compose-local-timestamp (car inner) (cadr inner))
             do (let ((o<i (local-timestamp< outer-date inner-date))
                      (o<=i (local-timestamp<= outer-date inner-date))
                      (o>=i (local-timestamp>= outer-date inner-date))
                      (o>i (local-timestamp> outer-date inner-date))
                      (o=i (local-timestamp= outer-date inner-date))
                      (o/=i (local-timestamp/= outer-date inner-date)))
                  (is (eql 1 (+ (if o<i 1 0) (if o=i 1 0) (if o>i 1 0))))
                  (cond
                    ((funcall listlessp outer inner)
                     (is o<i) (is o<=i) (is o/=i) (is (not o>i)) (is (not o>=i)) (is (not o=i)))
                    ((funcall listlessp inner outer)
                     (is o>i) (is o>=i) (is o/=i) (is (not o<i)) (is (not o<=i)) (is (not o=i)))
                    (t
                     (is (eql (local-timestamp-hash outer-date) (local-timestamp-hash inner-date)))
                     (is (not o>i)) (is o>=i) (is (not o/=i)) (is (not o<i)) (is o<=i)
                     (is o=i))))))))

    
          

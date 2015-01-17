;; must be a prime number

;; 1000003  ;; about 976 k
;; 1000033
;; 1000333
;; 100003   ;; about 97 k
;; 100333
;; 997
;; 499
;; 230      ;; for a special test

(defparameter *size#name-table*
  100333)

(defparameter *size#entry#name-table*
  100)

(defparameter *name-table*
  (make-array
   (list *size#name-table* *size#entry#name-table*)
   :initial-element nil))
(defparameter *max-carry-position* 22)

(defun string->natural-number (string
                               &key
                                 (counter 0)
                                 (sum 0))
  (if (string#empty? string)
      sum
      (multiple-value-bind
            (head#char
             tail#char
             string)
          (string->head#char string)
        (string->natural-number
         tail#char
         :counter (if (< counter *max-carry-position*)
                      (add1 counter)
                      0)
         :sum (+ sum
                 (shift#left
                  :step counter
                  :number (char->code head#char)))))))

;; (string->natural-number "")
;; (string->natural-number "@")
;; (string->natural-number "@@@")
(defun natural-number->index (natural-number)
  (if (not (natural-number? natural-number))
      (error "argument of natural-number->index must be a natural-number")
      (mod natural-number *size#name-table*)))

;; (natural-number->index 0)
;; (natural-number->index 123)
;; (natural-number->index 123.123)
(defun name? (x)
  (and (array? x)
       (= 1 (array-rank x))
       (= 2 (array-dimension x
                             0))
       (equal :<name>
              (aref x
                    0))))

(name? #(:<name> "123"))

(defun print-name (name
                   &key (stream t))
  (format stream
          "#name: ~A"
          '><))


(defun help#string->symbol#find-old-or-creat-new
    ())

(defun string->name (string)
  (let ((index
         (natural-number->index
          (string->natural-number string))))
    ()
    ))


(defun symbol->string (index)
  ())


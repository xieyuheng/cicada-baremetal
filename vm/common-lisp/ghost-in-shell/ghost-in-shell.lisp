(in-package :cicada-vm)
(defun shell ()
  (catch 'bye
    (welcome)
    (help#shell#loop)))

(defun welcome ()
  (newline)
  (format t "  welcome to GIS ^-^ ~%"))

(defun prompt ()
  (newline)
  (format t "  [*]  ")
  (format t "~A" *default-pathname-defaults*)
  (newline))

(defun help#shell#loop ()
  (prompt)
  (let* ((list#word (string->list#word (read#line)))
         (name (car list#word))
         (argument-list (cdr list#word)))
    (multiple-value-bind
          (function
           success)
        (gethash name *command-hash-table*)
      (cond
        ((eq t success)
         (funcall function
                  :argument-list argument-list))
        (:else
         (format t "nothing~%")))))
  (help#shell#loop))

(defparameter *command-hash-table*
  (make-hash-table :test (function equal)))
(defun kkk (&key argument-list)
  (format t "kkk took my baby away!")
  (newline))

(defun bye (&key argument-list)
  (format t "  bye bye ^-^")
  (newline)
  (throw 'bye :bye-bye!))
;; type chack of argument should be done
;; in the following functions


(defun bind (name function)
  (setf (gethash name *command-hash-table*)
        function))

(defun bind#many (&rest list)
  (mapcar (lambda (pattern)
            (let ((name (first pattern))
                  (function (second pattern)))
              (bind name function)))
          (group list :number 2)))


(defun unbind (name)
  (remhash name *command-hash-table*))

(defun unbind#many (&rest list)
  ;; list of string
  (mapcar (function unbind)
          list))


(bind#many "kkk" (function kkk)
           "p" (lambda (&key argument-list)
                 (kkk)
                 (kkk)
                 (kkk))
           "bye" (function bye)
           )

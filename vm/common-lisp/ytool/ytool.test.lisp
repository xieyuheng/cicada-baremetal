(in-package :cicada-vm)
;; (series [l] h [i])
(deftest series
    (ytool)
  (ensure
      (list (series 10 20 3)
            (series 10 22 3)
            (series 10 20)
            (series 10))
      ==>
      '((10 13 16 19)
        (10 13 16 19 22)
        (10 11 12 13 14 15 16 17 18 19 20)
        (1 2 3 4 5 6 7 8 9 10))))
(deftest full-scope-test--123
    (ytool)
  (ensure
      (help ((defun help1 (number)
               (add1 (help2 number))))
        (set! *zero* 0)
        (help1 *zero*)
        :where
        (defun help2 (number) (add1 (help3 number)))
        (defun help3 (number) (add1 number)))
      ==>
      3))

(deftest full-scope-test--321
    (ytool)
  (ensure
      (help ((defun help1 (number)
               (add1 number)))
            (set! *zero* 0)
            (help3 *zero*)
            :where
            (defun help2 (number) (add1 (help1 number)))
            (defun help3 (number) (add1 (help2 number))))
      ==>
      3))

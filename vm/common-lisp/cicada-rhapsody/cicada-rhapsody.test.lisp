(in-package :cicada-vm)
(deftest cicada-rhapsody#in-line
    (cicada-rhapsody)
  (ensure
      [ (: list :) [ [] ] ]
      ==>
      (list " (: list :) [ [] ] ")
      ))

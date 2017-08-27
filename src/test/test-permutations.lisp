;;;; permutations function tests

#|
(load "utils.lisp")
(load "test-permutations.lisp")
|#

(permutations nil)
(permutations (list 0))
(permutations (list 0 1))
(permutations (list 0 1 2))
(permutations (list 0 1 2 3))
(length (permutations (list 0 1 2 3 4 5 6 7 8)))
(= (length (permutations (list 0 1 2 3 4 5 6 7 8)))
    362880) ; 9! is 362880

(mapcar #'print
       (sort (permutations (list 0 1 2 3))
             #'list<))

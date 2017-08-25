;;; prints in a single visual row variable number of arrays
;;; provided that each array has equal dimension NxN and equal length
(defun print-arrays (&rest arrays)
    (when (listp (car arrays)) ; a small bug fix :)
        (setf arrays (car arrays)))
    (let ((dim (array-dimension (car arrays) 0)))
        (dotimes (k dim)
            (dolist (elt arrays)
                (print-array-row elt k dim)
                (format t "  "))
            (terpri)))
    (terpri))

;;; prints a single row of a multidimensional array in one incomplete line
(defun print-array-row (arr row dim)
    (dotimes (i dim)
        (let ((elt (aref arr row i)))
            (format t "~A " (if elt elt "-")))))

;;; checks whether a player has a victorious combination on the board
;;; if so, returns the combination
;;; otherwise returns nil
;;; board - 2-dimensional array
;;; player - symbol representing current player
(defun check-victory (board player)
    (find board combs :test
        #'(lambda (arr1 arr2)
            (dotimes (i (array-total-size arr2) t)
                (if (and (eq 'v (row-major-aref arr2 i))
                         (not (eq (row-major-aref arr1 i) player)))
                    (return nil))))))

;;; prints a two-dimensional array 'board'
;;; where board is 3x3; or (possibly) any board where
;;; (mod (array-total-size board) 3) => 0
(defun print-board (board)
    (let ((dim (array-dimension board 0))
          (tmp nil)
          (size (array-total-size board)))
        (dotimes (i size)
            (setf tmp
                (append tmp
                        (list (gethash (row-major-aref board i) hashtable)))))
        (let ((first  (subseq tmp 0 dim))
              (second (subseq tmp dim (* dim 2)))
              (third  (subseq tmp (* dim 2))))
            (print-arrays first)
            (print-arrays second)
            (print-arrays third))))

;;; returns all possible permutations of elements of lst
;;; lst must contain unique elements, otherwise behaviour is undefined
(defun permutations (lst &optional (lst-used nil))
    (if (null lst)
        (list lst-used)
        (let ((diff (set-difference lst lst-used))
              (res))
            (dolist (elt diff res)
                (setf res (append res (permutations
                    (remove elt lst)
                    (adjoin elt lst-used))))))))

;;; compares two lists of the same length
(defun list< (lst1 lst2)
    (if lst1
        (if (< (car lst1) (car lst2))
            t
            (if (= (car lst1) (car lst2))
                (list< (cdr lst1) (cdr lst2))
                nil))))


#|
(mapcar #'print
       (sort (permutations (list 0 1 2 3))
             #'list<))

|#

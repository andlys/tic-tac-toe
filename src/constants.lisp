;;; an array used to visualize cell of player X on the board
(defconstant X (make-array '(2 2) :initial-element 'x))

;;; an array used to visualize cell of player O on the board
(defconstant O (make-array '(2 2) :initial-element 'o))

;;; an array used to visualize empty cell on the board
(defconstant EMPTY (make-array '(2 2) :initial-element '-))

;;; a hashtable used in visualizing a board
(defparameter hashtable (make-hash-table :size 3 :test #'equalp))
(setf (gethash 'x hashtable) X)
(setf (gethash 'o hashtable) O)
(setf (gethash '- hashtable) EMPTY)

;;; a seed used to generate random numbers
(defconstant SEED (make-random-state t))

;;; a list of arrays that represent all posible
;;; victorious combinations that can happen on the board
;;; nil stands for unoccupied cell
;;; v (as in "victory") stands for a cell occupied by current player
(defconstant combs (list
    #2A((v   nil nil)
        (nil v   nil)
        (nil nil v  ))

    #2A((nil nil v  )
        (nil v   nil)
        (v   nil nil))

    #2A((v   v   v  )
        (nil nil nil)
        (nil nil nil))

    #2A((nil nil nil)
        (v   v   v  )
        (nil nil nil))

    #2A((nil nil nil)
        (nil nil nil)
        (v   v   v  ))

    #2A((nil nil v  )
        (nil nil v  )
        (nil nil v  ))

    #2A((nil v   nil)
        (nil v   nil)
        (nil v   nil))

    #2A((v   nil nil)
        (v   nil nil)
        (v   nil nil))
))

;;;; ... two players...
;;;; symbol X used to represent player X
;;;; symbol O used to represent player O

(load "constants.lisp")

(load "utils.lisp")

(load "minimax-tree.lisp")

(load "player.lisp")

(write-line "Building a minimax tree... This may take a few seconds...")
(defconstant root-node (build-tree))
(write-line "Done!")

(defun play ()
    (let ((players (list (make-player :sym 'x :strategy 'make-move-ai)
                         (make-player :sym 'o :strategy 'make-move-random)))
          (curr) ; current player
          (pattern) ; used to verify if current player wins
          (node root-node)
          ;; a 2-d array used to store current state of the board
          (board (make-array '(3 3) :initial-element '-)))
        (format t "Two players loaded: ~%~A~%~A~%"
                (first players) (second players))
        (dotimes (n 9)
            (setf curr (first players))
            (print-board board)
            (format t "Player ~A makes a move!~%" (player-sym curr))
            (when nil ; TODO remove this
                (format t "CHOICE: ~A~% MINIMAX-VALUE: ~A~% CHILDREN?: ~A~%"
                        (node-choice node)
                        (node-minimax-value node)
                        (null (node-children node))))
            (setf node (funcall (player-strategy curr) board
                                (player-sym curr) node))
            (setf pattern (check-victory board (player-sym curr)))
            (when pattern
                    (print-board board)
                    (format t "Victory! Player ~A won the game ~
                              with the pattern:~%"
                            (player-sym curr))
                    (print-arrays pattern)
                    (return))
            (setf players (reverse players))
            (when (= n 8)
                (format t "It's a draw: nobody won")))))

(play)

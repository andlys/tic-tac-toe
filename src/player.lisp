(defstruct player
    ;; a symbol that can be either 'x or 'o
    sym
    ;; a way in which a player makes moves
    ;; can be one of three options:
    ;;      'make-move-human
    ;;      'make-move-random
    ;;      'make-move-ai
    ;;      'make-move-emulate
    ;; each strategy but last marks a board and returns a node
    ;; each strategy but last accepts a node as the 3rd argument
    strategy)

;;; makes a move randomly choosing from among empty cells on the board
;;; sym - symbol of the current player
(defun make-move-random (board sym node)
    (format t "Press enter for computer to make a move...~%")
    (read-line)
    (clear-input)
    (let (lst)
        (dotimes (i (array-total-size  board))
            (let ((val (row-major-aref board i)))
                (if (eq val '-)
                    (push i lst))))
        (let ((choice (nth (random (length lst) SEED)
                            lst)))
            (setf (row-major-aref board choice)
                   sym)
            (format t "~A~%" choice)
            (find choice (node-children node) :key #'node-choice))))

;;; makes a move according to current position in minimax tree
;;; sym - symbol of the current player
(defun make-move-ai (board sym node)
    (format t "Press enter for computer to make a move...~%")
    (read-line)
    (clear-input)
    (prog1
        (setf node (node-next node))
        (make-move-emulate board sym (node-choice node))))

;;; makes a move according to choice variable
(defun make-move-emulate (board sym choice)
    (setf (row-major-aref board choice) sym))

;;; makes a move, lets a human input choice from the keyboard
;;; sym - symbol of the current player
(defun make-move-human (board sym node)
    (let ((in (read)))
        (clear-input)
        (let ((choice (case in
                            (7 0)
                            (8 1)
                            (9 2)
                            (4 3)
                            (5 4)
                            (6 5)
                            (1 6)
                            (2 7)
                            (3 8)
                            (otherwise nil))))
        ;(format t "board index to mark: ~A~%" choice)
        (if choice
            (if (eq (row-major-aref board choice) '-) ; cell is not busy
                (progn
                    (setf (row-major-aref board choice) sym)
                    (find choice (node-children node) :key #'node-choice))
                (progn
                    (format t "This cell is taken! Try another one~%")
                    (make-move-human board sym node)))
            (progn
                (format t "Please, Input a correct number~%")
                (make-move-human board sym node))))))

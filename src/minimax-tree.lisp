;;; a node used in constructing a minimax tree
(defstruct node
    ;; two bools
    ;; one of alpha? and beta? values must be t but not both simustaneously
    ;; (and alpha? beta?) => nil
    ;; (or  alpha? beta?) => t
    alpha?
    beta?
    ;; a bool indicating whether children list is nil
    leaf?
    ;; a number representing move (marked index of cell on the board array)
    ;; integer from 0 to 8
    choice
    ;; next node chosen according to set-minimax-values function
    (next nil)
    ;; a number value
    ;; if node is a leaf? then minimax value is either -1, 0 or 1
    ;; otherwise:
    ;; alpha? t - max of all minimax values of this node's direct children
    ;; beta?  t - min of all minimax values of this node's direct children
    minimax-value
    ;; a list of children
    children)

;;; returns a modified tree
;;; sets node-minimax-value, node-next values of all nodes in the tree
(defun set-minimax-values (tree)
    (unless (node-leaf? tree)
        (let ((lst (node-children tree)))
            (dolist (node lst)
                (unless (node-minimax-value node)
                    (set-minimax-values node)))
            (setf (node-minimax-value tree)
                (if (node-alpha? tree)
                    (reduce #'max lst :key #'node-minimax-value)
                    (reduce #'min lst :key #'node-minimax-value)))
            (setf (node-next tree)
                  (find (node-minimax-value tree) lst
                        :key #'node-minimax-value))))
    tree)

;;; builds a minimax tree
(defun build-tree ()
    (let ((root (make-node))
          (inputs-lst (permutations (list 0 1 2 3 4 5 6 7 8))))
        (dolist (inputs inputs-lst)
            (let ((board (make-array '(3 3) :initial-element '-))
                  (players (list (make-player :sym 'x
                                      :strategy 'make-move-emulate)
                                 (make-player :sym 'o
                                      :strategy 'make-move-emulate)))
                  (move-number 0)
                  (currp) ; current player
                  (pattern)
                  (tree root))
                (dolist (choice inputs)
                    (let ((node (find choice (node-children tree) :key #'node-choice))) ; TODO refactor the line
                        (unless node
                            (setf node
                                (make-node :alpha? (evenp move-number)
                                           :beta?  (oddp  move-number)
                                           :choice choice)))
                          (setf currp (first players))
                          (funcall (player-strategy currp)
                                   board
                                   (player-sym currp)
                                   choice)
                          (setf pattern
                               (check-victory board (player-sym currp)))
                          (when pattern ; player currp wins
                              (setf (node-leaf? node) t)
                              (setf (node-minimax-value node)
                                    (case (player-sym currp)
                                          (x 1)
                                          (o -1))))
                          (when (= move-number 8) ; draw: nobody wins
                              (setf (node-leaf? node) t)
                              (setf (node-minimax-value node) 0))
                          ;; stop iterating if this move was the last on board
                          (unless (find choice (node-children tree) :key #'node-choice) ; TODO refactor this
                            (push node (node-children tree)))
                          (setf tree node)
                          (when (node-leaf? node) (return))
                          (setf players (reverse players))
                          (incf move-number)))))
        (set-minimax-values root)))

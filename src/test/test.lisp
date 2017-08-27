;;;; misc tests, running this file may result in errors

; (load "utils.lisp")
; (load "tree.lisp")

(defparameter root (make-node))

(dolist (lst (permutations (list 0 1)))
    (let ((tree root)
          (move-number 0))
        (dolist (choice lst)
            (let ((node (find choice (node-children tree) :key #'node-choice)))
                (when (null node)
                    (setf node
                        (make-node :alpha? (zerop (mod move-number 2))
                                   :beta?  (not (zerop (mod move-number 2)))
                                   :leaf?  (= move-number 1)
                                   :choice choice
                                   :minimax-value (if (= move-number 1) -1 nil)))
                    ;; set other fields...
                    (push node (node-children tree))
                (setf tree node))
                (incf move-number))))


(reduce #'max (list 1 4 5))

(reduce #'max (list 1 4))

(reduce #'max (list 1))

(reduce #'max (list) :initial-value -1)


(length (node-children (build-tree)))

(length (node-children (find 3 (node-children (build-tree)) :key #'node-choice)))

(find 1
      (node-children (build-tree))
      :key #'node-choice)

(defconstant root-node (build-tree))

;;; finds the first leaf of a tree
(defun leaf (tree)
    (if (node-leaf? tree)
        tree
        (progn
            (print (node-choice tree))
            (terpri)
            (leaf (first (node-children tree))))))

;; old version of the function...
(defun permutations (lst &optional (lst-used nil))
    (if (null lst)
        (list lst-used)
        (let ((diff (set-difference lst lst-used)) ; verbose
              (res)) ;verbose
            (dolist (elt diff res) ; use collect
                (setf res (append res (permutations
                    (remove elt lst)
                    (adjoin elt lst-used)))))))) ; adjoin? all elements are unique by default, why not push?

(loop for elt in '((a b) (a c) (a d))
    collect elt)

;(leaf root-node)

; (setf tree (funcall ...) )

;(play)
;(play2)

;(node-choice
    ;(find (node-minimax-value node)
          ;(node-children node)
          ;:key #'node-minimax-value))

;;; player.lisp
;;; minimax-tree.lisp
;;; constants.lisp
;;; ???
;;; main.lisp

;(make-move-human board 'o)
;(setf (aref board 0 0) 'x)
;(setf (aref board 0 1) 'x)
;(setf (aref board 0 2) 'x)
;(setf (aref board 1 0) 'o)
;(setf (aref board 1 1) 'o)
;(setf (aref board 1 2) 'o)
;(print-board board)
;(print (check-victory board 'x))
;(print (check-victory board 'o))
;(load "my9-tic-tac-toe-game.lisp")
;(print-arrays (nth 3 combs))

;(setf (aref board 1 1) 'x)
;(setf (aref board 1 0) 'o)
;(print-board board)
;(print-arrays O O X)
;(print-arrays EMPTY O EMPTY)
;(print-arrays board)

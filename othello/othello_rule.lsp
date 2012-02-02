
(defun init-game ()
  (setf *othello-board* (make-board))
  (init-turn))
  


(defun init-turn ()
  (setf *turn* 'white))

(defun change-turn ()
  (if (eq *turn* 'white)
    (setf *turn* 'black)
    (setf *turn* 'white)))


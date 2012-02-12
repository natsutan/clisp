(defconstant *dir-table* '((0 1) (1 1) (1 0) (1 -1) 
			(0 -1) (-1 -1) (-1 0) (-1 1)))
          

(defun init-game ()
  (setf *othello-board* (make-board))
  (init-history)
  (init-turn)
  )

(defstruct history move (tunerd '()))

(defstruct turnalbe-list dir l)

(defun init-history ()
  (setq *history* '()))

(defun init-turn ()
  (setf *turn* 'white))

(defun add-history (history)
  (push *history* history))


(defun change-turn ()
  (if (eq *turn* 'white)
    (setf *turn* 'black)
    (setf *turn* 'white)))


(defun move (bd x y sym)
  (set-disk bd x y *turn*)
  (change-turn))

; count score
(defun count-score (sym)
  (let ((cnt 0))
    (do ((x 0 (+ x 1))) ((>= x *board-size*))
      (do ((y 0 (+ y 1))) ((>= y *board-size*))
        (let ((disk (get-disk *othello-board* x y)))
          (if (eq sym disk)
            (incf cnt)))))
    cnt))



(defun gameoverp (bd)
  (let ((empty_cnt (count-score 'empty)))
    (eq empty_cnt 0)))
                
(defun pass ()
  (change-turn))




(defun turnlist (bd x y)
  (< x 3))
    
;  (let ((tl '()))
;    (dolist (dir *dir-table*)
;      (let ((tb (turnable-disks bd x y dir)))
;        (when tb
;          (setf tl (setf tl (append tl (list tb)))))))
;    tl
;    ))
          
(defun turnable-disks (bd x y dir)
  (if (< x 3)
    '(1 2)
    NIL))


(defun movable-positoinp-p (bd x y)
  (if (out-of-board-p x y)
    nil
    (let ((sym (get-disk bd x y)))
      (if (eq sym 'empty)
          (turnlist bd x y)
        nil))))


(defun out-of-board-p (x y)
  (not (and (>= x 0)
            (<  x  *board-size*)
            (>= y 0)
            (<  y *board-size*))))

; move

; init
; undo
; current-color
; movable-positon
; turns
; update

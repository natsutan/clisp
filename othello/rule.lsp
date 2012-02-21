(defconstant *dir-table* 
  '((N . (0 -1))
    (NE . (-1 -1))
    (W . (-1 0))
    (SE . (-1 1))
    (S . (0 1))
    (SW .(1 1))
    (W . (1 0))
    (NW . (1 -1))))

; assoc          
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
  (let ((movable-list (movable-positoinp-p bd x y)))
    (dolist (tlist movable-list)
      (dolist (turn tlist)
        (turn-disk bd (car turn) (cdr turn))))
    (set-disk bd x y *turn*)
    (change-turn)
    (when (equal (count-score 'empty) 0)
      (show-result 0 0)
      )))

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
  (let ((tl '()))
    (dolist (dir *dir-table*)
      (let ((tb (turnable-disks bd x y (cdr dir))))
        (when tb
          (if tl
            (setf tl (append tl (list tb)))
            (setf tl (list tb))))))
    tl
    ))

(defun get-reverse-disk (sym)
  (case sym
    ('white 'black)
    ('black 'white)
    (t nil)))


(defun turnable-disks (bd x y dir)
  (let ((target (get-disk bd x y)))
    (if (not (eql target 'empty))
      '()
      (let ((sym *turn*) (result '())  (next-x (+ x (first dir))) (next-y (+ y (second dir))))
        (if (out-of-board-p next-x next-y)
          '()
          (let ((rev (get-reverse-disk *turn*)) (next-disk (get-disk bd next-x next-y)))
            (if (not (equal rev next-disk))
              '()
              (search-sequance-disks bd next-x next-y *turn* dir))))))))

(defun search-sequance-disks (bd x y sym dir)
  (let ((next-x (+ x (first dir))) (next-y (+ y (second dir))) (rev (get-reverse-disk sym)))
    (if (out-of-board-p next-x next-y)  ; 
      '()
      (let ((next-disk (get-disk bd next-x next-y)))
        (cond
         ((eql next-disk 'empty) '())
         ((eql next-disk sym) (list (cons x y)))  
         (t     ; (eq next-disk rev) continue search
          (let ((r (search-sequance-disks bd next-x next-y sym dir)))
            (cond 
             ((eql r '()) '())
             (t
              (cons (cons x y) r))))))))))

      
(defun movable-positoinp-p (bd x y)
  (if (out-of-board-p x y)
    nil
    (let ((sym (get-disk bd x y)))
      (if (equal sym 'empty)
          (turnlist bd x y)
        nil))))


(defun out-of-board-p (x y)
  (not (and (>= x 0)
            (<  x  *board-size*)
            (>= y 0)
            (<  y *board-size*))))

; (trace turnable-disks)
; (trace search-sequance-disks)
;  (setf ndir '(0 1)) 
; (turnable-disks *othello-board* 4 1 ndir)
; (4 5) (5 5)             
  

; move

; init
; undo
; current-color
; movable-positon
; turns
; update


(defconstant *board-size* 8)
(defconstant *grid-string-x* '(#\a #\b #\c #\d #\e #\f #\g #\h)) 
(defconstant *grid-string-y* '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8))
(defconstant *grid-string-offset* 10)
(defconstant *square-size* 50)
(defconstant *board-margin-left* 30)
(defconstant *board-margin-right* 30)
(defconstant *board-margin-top* 30)
(defconstant *board-margin-bottom* 30)

(defvar *hilited*) 

(defun board-length ()
  (* *board-size* *square-size*))

(defun get-view-height ()
  (+ *board-margin-top*
     (board-length)
     *board-margin-bottom*)) 

(defun get-view-width ()
  (+ *board-margin-left*
     (board-length)
     *board-margin-right*))

(defun board-top-position ()
  *board-margin-top*)

(defun board-bottom-position ()
  (+ (board-top-position) (board-length)))

(defun board-left-position ()
  *board-margin-left*)

(defun board-right-position ()
  (+ (board-left-position) (board-length)))

(defun nth-x-position (n)
  (+ (board-left-position) (* *square-size* n)))

(defun nth-y-position (n)
  (+ (board-top-position) (* *square-size* n)))


(defun outer-line (pane x y width height)
  (let ((thick 3))
    (gp:draw-line pane
                  (board-left-position) (board-top-position)
                  (board-right-position) (board-top-position)
                  :thickness thick)
    (gp:draw-line pane
                  (board-right-position) (board-top-position)
                  (board-right-position) (board-bottom-position)
                  :thickness thick)
    (gp:draw-line pane
                  (board-right-position) (board-bottom-position)
                  (board-left-position) (board-bottom-position)
                  :thickness thick)
    
    (gp:draw-line pane
                  (board-left-position) (board-bottom-position)
                  (board-left-position) (board-top-position)
                  :thickness thick)
  ))

(defun grid (pane x y width height)
  (let ((thick 1))
    (do ((x 0 (+ x 1)))
        ((> x *board-size* ))
      (gp:draw-line pane
                    (nth-x-position x) (board-top-position)
                    (nth-x-position x) (board-bottom-position)
                    :thicness thick
                    ))
    (do ((y 0 (+ y 1)))
        ((> y *board-size* ))
      (gp:draw-line pane
                    (board-left-position) (nth-y-position y)
                    (board-right-position) (nth-y-position y)
                    :thickness thick
    ))))

(defun grid-text (pane x y width height)
  (let ((string-font (gp:find-best-font
                      pane
                      (gp:make-font-description 
                       :size 16
                       ))))
      (do ((x 0 (+ x 1)))
          ((> x *board-size*))
        (gp:draw-character  pane
                            (nth x *grid-string-x*)
                          (+ (nth-x-position x) (/ *square-size* 2))  
                          (- (board-top-position) *grid-string-offset*)
                          :font string-font
                          ))
    (do ((y 0 (+ y 1)))
        ((> y *board-size*))
      (gp:draw-character pane
                          (nth y *grid-string-y*)
                          (- (board-left-position) *grid-string-offset* 10)
                          (+ (nth-y-position y) (/ *square-size* 2) 10)
                          :font string-font))))


(defun calc-ball-pos ()
  (let ((x1  (nth-x-position 2)) (x2 (nth-x-position 6))
        (y1  (nth-y-position 2)) (y2 (nth-y-position 6)))
    (list (list x1 y1) (list x1 y2) (list x2 y1) (list x2 y2))))
        

(defun grid-ball (pane x y width height)
  (let ((balls (calc-ball-pos)))
    (dolist (pos balls)
      (gp:draw-circle pane (first pos) (second pos)  3
                  :filled t
                  :foreground :black
                  ))))

(defun draw-disks (pane xp yp widht height)
  (do ((x 0 (+ x 1))) ((>= x *board-size*))
    (do ((y 0 (+ y 1))) ((>= y *board-size*))
      (let ((sym (get-disk *othello-board* x y))
            (centor-x (+ (nth-x-position x) (/ *square-size* 2)))
            (centor-y (+ (nth-x-position y) (/ *square-size* 2)))
            (radius (* *square-size* 0.4)))
        (draw-disk pane centor-x centor-y radius sym)))))


(defun draw-disk (pane x y radius sym)
  (cond ((eq sym 'black)
         (gp:draw-circle pane x y radius :filled t :foreground :black))
        ((eq sym 'white)
         (gp:draw-circle pane x y radius :filled t :foreground :white))
        (t 'None)))

(defun update-disks (pane)
  (draw-disk pane 0 0 0 0))
                              

(defun draw-board-functions ()
  (list #'outer-line #'grid #'grid-text #'grid-ball #'draw-disks) )

(defun set-disk (bd x y s)
  (setf (aref bd x y) s))

(defun get-disk (bd x y)
  (aref bd x y))

(defun turn-disk (bd x y)
  (let ((sym (get-disk bd x y)))
    (cond ((eq sym 'white)
           (set-disk bd x y 'black))
          ((eq sym 'black)
           (set-disk bd x y 'white))
          (t (error "no disk or illegal disk")))))

(defun make-board ()
  (let ((bd (make-array (list *board-size* *board-size*) :initial-element 'empty)))
    (setf *hilited* nil)
    (set-disk bd 3 3 'white)
    (set-disk bd 4 4 'white)
    (set-disk bd 3 4 'black)
    (set-disk bd 4 3 'black)

;    (set-disk bd 3 4 'white)
;    (set-disk bd 3 5 'white)
;    (set-disk bd 4 4 'black)
;    (set-disk bd 4 5 'black)
    
    
    bd))


; convert display positon to grid nubmer in the board
; if positon is out of grid, return nil
; else return grid number in the board
(defun calc-grid (x y)
  (let ((gx (calc-grid-x x))
        (gy (calc-grid-y y)))
    (if (and gx gy)
      (list gx gy)
      nil)))

(defun calc-grid-x (x)
  (if (or (< x (board-left-position)) (< (board-right-position) x))
    nil
    (let ((lx (- x *board-margin-left* 1)))
      (truncate lx *square-size*))))

(defun calc-grid-y (y)
  (if (or (< y (board-top-position)) (< (board-bottom-position) y))
    nil
    (let ((ly (- y *board-margin-top* 1)))
      (truncate ly *square-size*))))


; convert grid number to display position
; return a grid square list  (x1 x2 y1 y2)
(defun calc-square-from-grid (gx gy)
  (let ((x1 (nth-x-position gx))
        (x2 (nth-x-position (+ gx 1)))
        (y1 (nth-y-position gy))
        (y2 (nth-y-position (+ gy 1))))
    (list (+ x1 2) (- x2 1)
          (+ y1 2) (- y2 1))))
            

(defun lighten (pane gx gy)
  (progn 
    (if (not (equal *hilited* (list gx gy)))
      (restore-grid pane))
    (lighten-grid pane gx gy)
    (setf *hilited* (list gx gy))))

(defun lighten-grid (pane gx gy)
  (fill-grid pane gx gy :yellowgreen))

(defun restore-grid (pane)
  (if *hilited*
    (progn
      (fill-grid pane (first *hilited*)  (second *hilited*) :darkgreen)
      (setf *hilited* nil)
      (gp:invalidate-rectangle pane)
      )))


(defun fill-grid (pane gx gy color)    
  (let ((pos (calc-square-from-grid gx gy)))
    (let ((x (first pos))
          (y (third pos))
          (width (- (second pos) (first pos)))
          (height (- (fourth pos) (third pos))))
      (gp:draw-rectangle pane x y width height
                         :foreground color
                         :filled t
                         ))))
 


; Callback function
(defun mouse-move (pane x y)
  (let ((pos (calc-grid x y)))
    (if pos
      (let ((gx (first pos)) (gy (second pos)))
        (if (movable-positoinp-p *othello-board* gx gy)
          (lighten pane gx gy)
        (restore-grid pane))))))


(defun mouse-click (pane x y)
  (let ((pos (calc-grid x y)) (bd *othello-board*))
    (if (eq pos nil)
      nil
      (let ((gx (first pos)) (gy (second pos)))
        (let ((disk (get-disk bd gx gy)))
          (if (and (eq disk 'empty) (movable-positoinp-p bd gx gy))
            (move bd gx gy *turn*)
          ))))))


(defun mouse-release (pane x y char1 char2)
  (gp:invalidate-rectangle pane)
  (gp:invalidate-rectangle char1)
  (gp:invalidate-rectangle char2)
  )







        
        





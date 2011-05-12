;
; lispworksで作るグラフィックス関連
;
(load "c:/home/Dropbox/clisp/mylib/digital_photo.lsp")
(load "c:/home/Dropbox/clisp/mylib/arrow.lsp")

(defvar *xoffset* 50)
(defvar *yoffset* 50)
(defvar *gpxmax* 300)
(defvar *gpymax* 300)
(defvar *scale* 5)
(setf *scale* 5)

(defvar *the-ellipce*)


(setq *graphics-option*
      (list
       '(arrow-size-h  . 5)
       '(arrow-size-w . 5)
       '(x-string-offset-x . 10)
       '(x-string-offset-y . 10)
       '(y-string-offset-x . 10)
       '(y-string-offset-y . 10)
       '(scale-max-x . 30)
       '(scale-max-y . 30)
       ))

(defun get-opt (symbol)
  (cdr (assoc symbol *graphics-option*)))

(get-opt 'arrow-size-w)

(defun draw-probabilty-ellipse (ellipse)
  (setf *the-ellipce* ellipse)
  (capi:contain
   (make-instance
    'capi:output-pane
    :display-callback 'draw-all-object
    )
   :best-width 300
   :best-height 300))


;; 座標変換系関数
(defun xmax ()
  (- *gpxmax* *xoffset*))

(defun ymax ()
  (- *gpymax* *yoffset*))

(defun xmin ()
  (- *xoffset*))

(defun ymin ()
  (- *yoffset*))
  
(defun gpx (x)
  (+ x *xoffset*))

(defun gpy (y)
  (- *gpymax* *yoffset* y))


; 論理的な大きさをグラフィックス上の大きさに変換
(defun l2b (logical_pos)
  (* *scale* logical_pos))

; 論理座標からグラフィクスの座標へ変換
(defun l2gx (logical_pos)
  (gpx (l2b logical_pos)))

(defun l2gy (logical_pos)
  (gpy (l2b logical_pos)))

 
(defun draw-all-object (pane x y width height)
  (setf  *gpxmax* (- width 1))
  (setf  *gpymax* (- height 1))
  (draw-coordinate-axis pane))

(defun draw-coordinate-axis (pane)
  (let ((x-axis (make-arrow-list  0 (gpy 0) *gpxmax* (gpy 0) :w (get-opt 'arrow-size-w) :h (get-opt 'arrow-size-h)))
        (y-axis (make-arrow-list  (gpx 0) *gpymax* (gpx 0)  0 :w (get-opt 'arrow-size-w) :h (get-opt 'arrow-size-h))))
    (dolist (v (append x-axis y-axis))
      (gp:draw-line pane                                        ; 描画
                    (vec-from-x v)
                    (vec-from-y v)
                    (vec-to-x v)
                    (vec-to-y v))))
  (gp:draw-string pane "x"
                  (- *gpxmax* (get-opt 'x-string-offset-x)) (+ (gpy 0) (get-opt 'x-string-offset-y)))
  (gp:draw-string pane "y"
                  (- (gpx 0) (get-opt 'y-string-offset-x))   (get-opt 'y-string-offset-y)))

    



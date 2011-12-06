;;;;-*- encoding: :utf-8 mode: lisp -*-;;;;
;;;; digital photo
;;;; 最小二乗法と測量網平均の基礎　

; 二乗
(defun square (x)
  (* x x))

; √x^2 + y^2
(defun distance (x y)
  (sqrt (+ (square x) (square y))))

; 平均
(defun average (dlist)
  (let ((n (length dlist)) (sum (apply #'+ dlist)))
    (/ sum n)))

; 共分散
; σxy = ρxy σx σy 
(defun covariance (ρxy σx σy)
  (* ρxy σx σy))

; 単位変換
(defun m->cm (x) (* 100 x ))
(defun cm->mm (x) (* 10 x ))
(defun m->mm (x )
  (cm->mm (m->cm x)))
(defun mm->cm (x) (/ x 10 ))
(defun cm->m (x) (/ x 100))
(defun mm->m (x)
  (cm->m (mm->cm x)))

;;; -------------------------------------------------------------------------------      
;;;　直行座標の標準偏差から、極座標の標準偏差へ変換
;;;    s^2 = x^2 + y^2
(defun convert-standard-deviation-rectangular->polar (x  y  σx  σy  &key (ρxy 0))
  (let ((s (distance x y)))
    (sqrt (+ (* (square (/ x s)) (square σx))
             (* (square (/ y x)) (square σy))
             (* 2 (/ x s) (/ y y) (covariance ρxy σx σy))
              ))))


;;;--------------------------------------------------------------------------------
;;; 重み付き測定値から、真値を推測する。
;;;--------------------------------------------------------------------------------
; 重み付きデータ
(defstruct  v-with-weight value (weight 1))
; 標準偏差付きデータ
(defstruct v-with-stddeviation value stddev)

; 標準偏差付きのデータを作る。データはm、標準偏差はmmとする
(defun make-vs-m-mm (data-m stddev-mm)
  (make-v-with-stddeviation :value data-m :stddev (mm->m stddev-mm)))

; 標準偏差付きのデータを重み付きデータに変換する。
(defun v-with-stddeviation->v-with-weight (sdata)
  (make-v-with-weight
   :value (v-with-stddeviation-value sdata)
   :weight (/ 1 (square (v-with-stddeviation-stddev sdata)))))

; 重み付きデータから、平均値を計算する。
(defun average-from-v-with-weight (wlist)
  (average (mapcar #'v-with-weight-value wlist)))

; 重み付きデータから、重みの二乗積と基準値からの差とを計算する。
; pn^2 dn
(defun product-weight-delta (wdata d0)
  (* (v-with-weight-weight wdata) (- d0 (v-with-weight-value wdata))))

;;pn^2 dnの輪を取る
(defun sum-of-pd (wlist d0)
  (let ((sum 0))
    (dolist (wdata wlist)
      (let ((v (* (v-with-weight-weight wdata)  (- (v-with-weight-value wdata) d0))))
        (print "in sum of pd")
        (print (list (v-with-weight-weight wdata) (- (v-with-weight-value wdata) d0)))
        (setq sum (+ sum v))))
    sum))


;; 重み付きデータから真値を推測する
;; d = d0 + (p1^2 d1+p2^2 d2+p3^2 d3 ...)/(p1+p2+p3+...)
;; d0 平均値
;; d1, d2 ... 測定値と平均値の差
 (defun truevalue-from-weighted (wlist)
   (let
       ((d0 (average-from-v-with-weight wlist))
        (sum-of-weight  (reduce #'+ (mapcar #'v-with-weight-weight wlist))))
     (+ d0  (/  (sum-of-pd wlist d0)  sum-of-weight))))

;;;
;;; 確率長円を求める
;;;
(defstruct ellipse x y axe1 axe2 rot_rad)

(defun sigma^2x (sigma)
  (first sigma))

(defun sigma_xy (sigma)
  (second sigma))

(defun sigma^2y (sigma)
  (fourth sigma))


; 座標軸を回転させ、σu、σvを求める。
;
; b = (σx^2 + σy^2)+4σxy
;
(defun sigma^2u (sigma)
  (let ((s2x (sigma^2x sigma))
        (sxy (sigma_xy sigma))
        (s2y (sigma^2y sigma)))
    (let ((b (+ (square (- s2x s2y)) (* 4 (square sxy)))))
      (/ (+ s2x s2y (sqrt b))
	 2))))


(defun sigma^2v (sigma)
  (let ((s2x (sigma^2x sigma))
        (sxy (sigma_xy sigma))
        (s2y (sigma^2y sigma)))
    (let ((b (+ (square (- s2x s2y)) (* 4 (square sxy)))))
      (/ (- (+ s2x s2y) (sqrt b))
	 2))))

; 2次元正規分布にしたがう確率変数X,Yが楕円の中にある確率
(defun c-from-p (p)
  (if (= p 95.0)
      2.448
    nil))
      

;楕円の半径を求める
(defun axe (σ^2 probabilty)
  (* (c-from-p probabilty)
     (sqrt σ^2)))


; 回転角
(defun rot-angle (sigma)
  (let ((s2u (sigma^2u sigma))
        (sxy (sigma_xy sigma))
        (s2x (sigma^2x sigma)))
    (atan (/ (- s2u s2x) sxy))))

(defun probabilty-ellipse (sigma ux uy probabilty)
  (let ((s2u (sigma^2u sigma))
        (s2v (sigma^2v sigma)))
    (make-ellipse
     :x ux
     :y uy
     :axe1  (axe s2u probabilty)
     :axe2 (axe s2v probabilty)
     :rot_rad  (rot-angle sigma))))



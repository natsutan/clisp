;
; 矢を描くデモ
;
(in-package "CL-USER")
(defvar *width*)
(defvar *height*)

; capi の初期化
(defun draw-arrow-demo ()
  (capi:contain
   (make-instance
    'capi:output-pane
    :input-model '(((:motion )
                    draw-arrow-cb))
    :display-callback 'draw-resize-cb
    )
   :best-width 300
   :best-height 300))

;　window サイズ変更時のコールバック関数
(defun draw-resize-cb (pane x y width height)
  (setf *width* width)
  (setf *height* height))

; ベクトルの定義
(defstruct vec from-x from-y to-x to-y)

(defun mv (from-x from-y to-x to-y)
  (make-vec :from-x from-x :from-y from-y :to-x to-x :to-y to-y))

; 単位ベクトルの作成
(defun unit (v)
  (let ((base-x (- (vec-to-x v) (vec-from-x v) )) (base-y (- (vec-to-y v) (vec-from-y v))))
    (let ((vlen (sqrt (+ (* base-x base-x) (* base-y base-y)))))
      (mv 0 0 (/ base-x vlen) (/ base-y vlen)))))

; 矢印の作成
(defun make-arrow-list (from-x from-y to-x to-y  &optional (w 10) (h 10))
  (let ((center-line (mv from-x from-y to-x to-y))) 　; 基本となる線ベクトル
    (let ((uv (unit center-line)))                                 ; 基本となる線ベクトルから単位ベクトルを作成
      (let ((ux (vec-to-x uv)) (uy (vec-to-y uv)))        ; 単位ベクトルのx成分、y成分 
        (let ((left-line  (mv                                            ; 左側の線
                           to-x
                           to-y
                           (- (- to-x  (* uy w)) (* ux h))
                           (- (+ to-y (* ux w)) (* uy h))))
              (right-line (mv                                            ; 右側の線
                           to-x
                           to-y
                           (- (+ to-x (* uy w)) (* ux h))
                           (- (- to-y (* ux w)) (* uy h)))))
          (list center-line right-line left-line))))))          ;　3本の線をリストで返す

; マウスが移動したときのコールバック関数
(defun draw-arrow-cb (pane x y)
  (gp:clear-rectangle pane 0 0 *width* *height*)  ; clear screen
  (let ((center-x (/ *width* 2)) (center-y (/ *height* 2)))  ;　window中央の座標を計算
    (let ((plist (make-arrow-list center-x center-y x y)))      ;  windowの中央からマウスの座標までを引数に、矢印の座標を計算
      (dolist (v plist)
        (gp:draw-line pane                                        ; 描画
                      (vec-from-x v)
                      (vec-from-y v)
                      (vec-to-x v)
                      (vec-to-y v))))))

; アプリケーション起動
;(draw-arrow-demo)



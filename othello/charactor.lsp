
(defconstant *char-dir* "othello/char/")

(defstruct charactor name file color)



(defun mk-char (l)
  (make-charactor :name (first l)
                  :file (second l)
                  :color (third  l)))


(setf *char-list* (mapcar #'mk-char '(
                                    ("つぼみ" "tubomi.jpg" ())
                                    ("えりか" "erika.jpg"  ())
                                    ("いつき" "ituki.jpg"  ())
                                    ("ゆり" "yuri.jpg" ())
                                    ("ポプリ" "popuri.jpg" ())
                                    ("コフレ" "kohure.jpg" ())
                                    ("シフレ" "shipre.jpg" ())
                                    ("コロン" "koron.jpg" ())
                                    ("ダークプリキュア" "dark.jpg" ())
                                    ("コブラージャ" "kobura.jpg" ())
                                    ("クモジャンキー" "kumo.jpg" ())
                                    ("サバーク博士" "sabaku.jpg" ())
                                    ("スナッキー" "sunakki.jpg" ())
                                    ("長門" "yuki.jpg" ())
					)))


(defun char-filename (n)
  (let ((ch (nth n *char-list*)))
    (format nil "~A~A" *char-dir* (charactor-file ch))))




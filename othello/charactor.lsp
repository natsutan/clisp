;キャラクターの情報と、日本語関連

(defconstant *char-dir* "othello/char/")

(defstruct charactor name file color)



(defun mk-char (l)
  (make-charactor :name (first l)
                  :file (second l)
                  :color (third  l)))


(setf *char-list* (mapcar #'mk-char '(
                                    ("つぼみ" "tubomi.jpg" ())
                                    ("えりか" "erika.jpg"  ())
				    ("キュアハッピー" "happy.jpg" ())
				    ("キュアサニー" "sunny.jpg" ())
				    ("キュアピース" "peace.jpg" ())
				    ("キュアマーチ" "march.jpg" ())
    				    ("キュアビューティ" "beauty.jpg" ())
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

; 日本語
(defconstant *pass_msg* "パス")
(defconstant *start_msg* "はじめ")
(defconstant *end_msg* "おしまい")

(defconstant *win_msg* "のかち")
(defconstant *draw_msg* "ひきわけ")


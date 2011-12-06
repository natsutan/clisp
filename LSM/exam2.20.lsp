;;;-*- encoding: utf-8 mode: lisp;  -*-
;(setf *mylib-path* "~/Dropbox/clisp/mylib/")
(load "~/Dropbox/clisp/mylib/digital_photo.lsp")
;(load "~/dropbox/Dropbox/clisp/mylib/digital_photo.lsp")


;;; 例題 2-20
;;; 確率変数X,Yの平均値が2次元正規分布にしたがい、分散共分散行列が
;;;
;;; Σ = ((σx^2 σxy) (σxy σy^2)) = ((16.00 5.48) (5.48 9.00))
;:;
;;; とあるとする。
;;; X、Yの平均値がそれぞれμx=24.0、μy=12.0とするとき
;;; 95%確率長円を図に示せ


(setf sigma '(16.00 5.48 5.48 9.00))
(setf ux 24.0)
(setf uy 12.0)
(setf p 95.0)
(setf pe (probabilty-ellipse sigma ux uy p))

(format t  "楕円の中央は(~A,~A)。半径は~A、~A。傾きは~Arad~%"
        (ellipse-x pe) (ellipse-y pe)
        (ellipse-axe1 pe) (ellipse-axe2 pe)
        (ellipse-rot_rad pe)
        )


(format t  "The centor of ellipse =(~A,~A). Axes are~A,~A. angle=~Arad~%"
        (ellipse-x pe) (ellipse-y pe)
        (ellipse-axe1 pe) (ellipse-axe2 pe)
        (ellipse-rot_rad pe)
        )








	     

;;;-*- encoding: utf-8 mode: lisp;  -*-
;(setf *mylib-path* "~/Dropbox/clisp/mylib/")
(load "~/Dropbox/clisp/mylib/digital_photo.lsp")

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
(probabilty-ellipse sigma ux uy p)








	     

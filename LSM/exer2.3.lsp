;(setf *mylib-path* "~/Dropbox/clisp/mylib/")
(load "~/Dropbox/clisp/mylib/digital_photo.lsp")

;;; exercise 2.3
;;; ���g�����V��p����AB�Ԃ̋����̑����4�񑪒肵�A�\�̌��ʂ𓾂��B
;;; AB�Ԃ̍Ŋm�l�͂����炩
;         ���ʒl�@�W���΍�
; 1. 1326.236mm  10mm 
; 2. 1326.242mm   6mm
; 3. 1326.252mm   8mm
; 4. 1326.246mm   4mm

(setq slist
      (list
       (make-vs-m-mm 1326.236  10)
       (make-vs-m-mm 1326.242  6)
       (make-vs-m-mm 1326.252 8)
       (make-vs-m-mm 1326.246 4)))

(setq wlist
      (mapcar #'v-with-stddeviation->v-with-weight  slist))

(truevalue-from-weighted wlist)

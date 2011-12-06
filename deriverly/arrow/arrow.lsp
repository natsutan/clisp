;
; ���`���f��
;
(in-package "CL-USER")
(defvar *width*)
(defvar *height*)

; capi �̏�����
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

;�@window �T�C�Y�ύX���̃R�[���o�b�N�֐�
(defun draw-resize-cb (pane x y width height)
  (setf *width* width)
  (setf *height* height))

; �x�N�g���̒�`
(defstruct vec from-x from-y to-x to-y)

(defun mv (from-x from-y to-x to-y)
  (make-vec :from-x from-x :from-y from-y :to-x to-x :to-y to-y))

; �P�ʃx�N�g���̍쐬
(defun unit (v)
  (let ((base-x (- (vec-to-x v) (vec-from-x v) )) (base-y (- (vec-to-y v) (vec-from-y v))))
    (let ((vlen (sqrt (+ (* base-x base-x) (* base-y base-y)))))
      (mv 0 0 (/ base-x vlen) (/ base-y vlen)))))

; ���̍쐬
(defun make-arrow-list (from-x from-y to-x to-y  &optional (w 10) (h 10))
  (let ((center-line (mv from-x from-y to-x to-y))) �@; ��{�ƂȂ���x�N�g��
    (let ((uv (unit center-line)))                                 ; ��{�ƂȂ���x�N�g������P�ʃx�N�g�����쐬
      (let ((ux (vec-to-x uv)) (uy (vec-to-y uv)))        ; �P�ʃx�N�g����x�����Ay���� 
        (let ((left-line  (mv                                            ; �����̐�
                           to-x
                           to-y
                           (- (- to-x  (* uy w)) (* ux h))
                           (- (+ to-y (* ux w)) (* uy h))))
              (right-line (mv                                            ; �E���̐�
                           to-x
                           to-y
                           (- (+ to-x (* uy w)) (* ux h))
                           (- (- to-y (* ux w)) (* uy h)))))
          (list center-line right-line left-line))))))          ;�@3�{�̐������X�g�ŕԂ�

; �}�E�X���ړ������Ƃ��̃R�[���o�b�N�֐�
(defun draw-arrow-cb (pane x y)
  (gp:clear-rectangle pane 0 0 *width* *height*)  ; clear screen
  (let ((center-x (/ *width* 2)) (center-y (/ *height* 2)))  ;�@window�����̍��W���v�Z
    (let ((plist (make-arrow-list center-x center-y x y)))      ;  window�̒�������}�E�X�̍��W�܂ł������ɁA���̍��W���v�Z
      (dolist (v plist)
        (gp:draw-line pane                                        ; �`��
                      (vec-from-x v)
                      (vec-from-y v)
                      (vec-to-x v)
                      (vec-to-y v))))))

; �A�v���P�[�V�����N��
;(draw-arrow-demo)



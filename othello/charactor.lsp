;�L�����N�^�[�̏��ƁA���{��֘A

(defconstant *char-dir* "othello/char/")

(defstruct charactor name file color)



(defun mk-char (l)
  (make-charactor :name (first l)
                  :file (second l)
                  :color (third  l)))


(setf *char-list* (mapcar #'mk-char '(
                                    ("�ڂ�" "tubomi.jpg" ())
                                    ("���肩" "erika.jpg"  ())
				    ("�L���A�n�b�s�[" "happy.jpg" ())
				    ("�L���A�T�j�[" "sunny.jpg" ())
				    ("�L���A�s�[�X" "peace.jpg" ())
				    ("�L���A�}�[�`" "march.jpg" ())
    				    ("�L���A�r���[�e�B" "beauty.jpg" ())
                                    ("����" "ituki.jpg"  ())
                                    ("���" "yuri.jpg" ())
                                    ("�|�v��" "popuri.jpg" ())
                                    ("�R�t��" "kohure.jpg" ())
                                    ("�V�t��" "shipre.jpg" ())
                                    ("�R����" "koron.jpg" ())
                                    ("�_�[�N�v���L���A" "dark.jpg" ())
                                    ("�R�u���[�W��" "kobura.jpg" ())
                                    ("�N���W�����L�[" "kumo.jpg" ())
                                    ("�T�o�[�N���m" "sabaku.jpg" ())
                                    ("�X�i�b�L�[" "sunakki.jpg" ())
                                    ("����" "yuki.jpg" ())
					)))


(defun char-filename (n)
  (let ((ch (nth n *char-list*)))
    (format nil "~A~A" *char-dir* (charactor-file ch))))

; ���{��
(defconstant *pass_msg* "�p�X")
(defconstant *start_msg* "�͂���")
(defconstant *end_msg* "�����܂�")

(defconstant *win_msg* "�̂���")
(defconstant *draw_msg* "�Ђ��킯")


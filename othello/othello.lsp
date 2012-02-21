(load "othello/board.lsp")
(load "othello/charactor.lsp")
(load "othello/rule.lsp")
(defconstant *img-dir* "othello/img/")
(defvar *top*)
(defvar *option-dialog*)
(defvar *turn*)
(defvar *othello-board*)
(defvar *history*)
(setf *char-1* 0)
(setf *char-2* 1)


(defun draw-face (pane file)
  (let  ((img (gp:read-and-convert-external-image
               pane
               file)))
    (gp:draw-image pane img 10 10)))

(defun draw-turn-image (pane sym)
  (if (eq sym *turn*)
    (gp:draw-image pane
                   (gp:read-and-convert-external-image pane "othello/char/hammy.png")
                   15 150)
    (gp:clear-rectangle pane 0 150 200 200)))

  
(defun number-filename (v)
  (format nil "~A~A.gif" *img-dir* v))

(defun draw-score-image (pane v sym)
  (gp:draw-image pane
                 (gp:read-and-convert-external-image pane (number-filename v))
                 (if (eq sym 'high) 30 50)
                 100))
              
(defun draw-score (pane sym)
  (let ((score (count-score sym)))
    (multiple-value-bind
        (high-value low-value)
        (truncate score 10)
      (progn 
        (draw-score-image pane high-value 'high)
        (draw-score-image pane low-value 'low)))))



(defun draw-char1 (pane x y width height)
  (draw-face pane (char-filename *char-1*))
  (draw-score pane 'white)
  (draw-turn-image pane 'white)
  ) 


(defun draw-char2 (pane x y width height)
  (draw-face pane (char-filename *char-2*))
  (draw-score pane 'black)
  (draw-turn-image pane 'black))



;;; Optin Dialog
(defun option-select-char (str n)
  (make-instance   
   'capi:option-pane
   :items (mapcar #'charactor-name *char-list*)
   :selected-item (charactor-name (nth n *char-list*))
   :title str)) 


; buttons for option dialog
(defun option-dialog-ok-and-cancel-buttons (sel1 sel2)
  (make-instance
   'capi:row-layout
   :description
   (list 
    (make-instance
     'capi:push-button
     :data "OK"
     :callback #'(lambda (&rest args)
                   (progn
                     (setf *char-1* (capi:choice-selection sel1))
                     (setf *char-2* (capi:choice-selection sel2))
                     (capi:quit-interface *option-dialog*)
                     (setf *option-dialog* nil)
                     (gp:invalidate-rectangle (slot-value *top* 'char1))
                     (gp:invalidate-rectangle (slot-value *top* 'char2))
                      )))
    (make-instance
     'capi:push-button
     :data "Cansel"
     :callback #'(lambda (&rest args)
                   (progn
                     (capi:quit-interface *option-dialog*)
                     (setf *option-dialog* nil))))
    )))
  


(defun option-dialog (data interface)
  (let ((sel1 (option-select-char "Player1" *char-1*))
        (sel2 (option-select-char "Player2" *char-2*))) 
    (setf *option-dialog* 
          (capi:contain
           (make-instance 
            'capi:column-layout
                :description (list
                              sel1
                              sel2
                              (option-dialog-ok-and-cancel-buttons sel1 sel2)
                              )
                :title "option"
                )))))



;;; Result
(defun show-result (data interface)
  (capi:contain
   (make-instance 
    'capi:output-pane
    :visible-min-height 100
    :visible-min-width 100
    :background :white
    :display-callback 'draw-result)
    )
   )

(defun draw-result (pane x y width height)
  (let ((string-font (gp:find-best-font
                      pane
                      (gp:make-font-description 
                       :size 32
                       )))
        (white (count-score 'white)) (black (count-score 'black)))
    (if (equal white black)
      (gp:draw-string pane *draw_msg* 70 100 :font string-font)
      (progn
        (gp:draw-string pane *win_msg* 100 100 :font string-font)
        (if (> white black)
          (draw-face pane (char-filename *char-1*))
          (draw-face pane (char-filename *char-2*)))))))



;;; Draw Board
(defun draw-board (pane x y width height)
  (dolist (draw-function (draw-board-functions))
    (funcall draw-function pane x y width height)))

(capi:define-interface othello ()
  ()
  (:menus 
   (file-menu "File"
              ("Open"))
   (option-menu "Option"
              ("Option")
              :selection-callback 'option-dialog
              ))
  (:menu-bar file-menu option-menu)
  (:panes
   (board capi:output-pane
          :visible-min-height (get-view-height)
          :visible-min-width (get-view-width)
          :display-callback 'draw-board
          :background :darkgreen
          :input-model (list (list '(:motion ) 'mouse-move)
                             (list '(:button-1 :press) 'mouse-click)
                             (list '(:button-1 :release) 'mouse-release char1 char2)
                             ))

   (char1 capi:output-pane
         :visible-min-height 120
         :visible-min-width 100
         :display-callback 'draw-char1)
   (char2 capi:output-pane
         :visible-min-height 120
         :visible-min-width 100
         :display-callback 'draw-char2)
   (bstart capi:push-button
           :data *start_msg*
           :visible-min-width 80
           :callback #'(lambda (data interface)
                           (init-game)
                           (gp:invalidate-rectangle board)
                           (gp:invalidate-rectangle char1)
                           (gp:invalidate-rectangle char2)
                           )
           )
   (bpass capi:push-button
           :data *pass_msg*
           :visible-min-width 80
           :callback #'(lambda  (data interface)
                         (pass)
                         (gp:invalidate-rectangle char1)
                         (gp:invalidate-rectangle char2)
                         )
           )
   
   (bend capi:push-button
           :data *end_msg*
           :visible-min-width 80
           :callback #'show-result
           )

   )
  (:layouts
   (all capi:row-layout '(buttons-and-board row-of-char))
   (buttons-and-board capi:column-layout '(buttons board))
   (row-of-char capi:column-layout '(char1 char2))
   (buttons capi:row-layout '(bstart bpass bend))
   )

  (:default-initargs :title "othello")
  )

(defun othello ()
  (init-game)
  (setf *top* (make-instance 'othello))
  (capi:display *top*))
   

(othello)
 









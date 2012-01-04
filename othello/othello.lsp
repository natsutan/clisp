(load "othello/othello_board.lsp")
(load "othello/othello_charactor.lsp")

(defvar *top*)
(defvar *option-dialog*)
(setf *char-1* 0)
(setf *char-2* 1)
(setf *othello-board* (make-board))


(defun draw-char1 (pane x y width height)
  (let  ((img (gp:read-and-convert-external-image
               pane
               (char-filename *char-1*))))
    (gp:draw-image pane img 10 10)))

(defun draw-char2 (pane x y width height)
  (let  ((img (gp:read-and-convert-external-image
               pane
               (char-filename *char-2*))))
    (gp:draw-image pane img 10 10)))


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
          :input-model '(((:motion )
                    mouse-move))
          )

   (char1 capi:output-pane
         :visible-min-height 100
         :visible-min-width 100
         :display-callback 'draw-char1)
   (char2 capi:output-pane
         :visible-min-height 100
         :visible-min-width 100
         :display-callback 'draw-char2)
   )
  (:layouts
   (all capi:row-layout '(board row-of-char))
   (row-of-char capi:column-layout '(char1 char2))
   )

  (:default-initargs :title "othello")
  )

(defun othello ()
  (setf *top* (make-instance 'othello))
  (capi:display *top*))
   

(othello)
 









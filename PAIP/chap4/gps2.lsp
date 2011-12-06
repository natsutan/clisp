(load "c:/home/Dropbox/clisp/mylib/paip.lsp")

(defvar *ops* nil)

(defstruct op "An operation"
	   (action nil)
	   (preconds nil)
	   (add-list nil)
	   (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  (let ((solve (achieve-all (cons '(start) state) goals nil)))
;    (print solve)
    (print (remove-if-not #'executing-p solve))))

(defun executing-p (x)
  (start-with x 'executing))

(defun start-with (list x)
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  (convert-op
   (make-op :action action
            :preconds preconds
            :add-list add-list
            :del-list del-list)))

(defun achieve-all (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
         current-state)))

(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal:~a goal stack:~a" goal goal-stack)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t 
	 (let ((candidates (find-all goal *ops* :test #'appropriate-p)))
	   (format t "~%in achieve find all returns ~a" candidates)
	   (some #'(lambda (op) (apply-op state goal op goal-stack)) candidates)
	       ))))


(defun member-equal (item list)
  (member item list :test #'equal))


(defun apply-op (state goal op goal-stack)
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  (length (setf *ops* oplist)))



                           
;
(defparameter *school-ops*
  (list 
   (make-op 
    :action 'drive-son-to-school
    :preconds '(son-at-home car-works)
    :add-list '(son-at-school)
    :del-list '(son-at-home))
   (make-op
    :action 'shop-installs-battery
    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
    :add-list '(car-works))
   (make-op
    :action 'tell-shop-problem
    :preconds '(in-communication-with-shop)
    :add-list '(shop-knows-problem))
   (make-op
    :action 'telephon-shop
    :preconds '(know-phone-number)
    :add-list '(in-communication-with-shop))
   (make-op
     :action 'look-up-number
     :preconds '(have-phone-book)
     :add-list '(know-phone-number))
    (make-op
     :action 'give-shop-money
     :preconds '(have-money)
     :add-list '(shop-has-money)
     :del-list '(have-money))))

(use 
 (mapc #'convert-op *school-ops*))

;(trace achieve-all)
;(use *school-ops*)

;(gps '(son-at-home car-needs-battery have-money have-phone-book)   '(son-at-school))





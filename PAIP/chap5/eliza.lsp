
(defconstant fail nil "Indicates pat-match faiture")
(defconstant no-binding '((t.t)) "Indicates pat-match success, with novariables")

(defun get-binding (var bindings)
  "Find a (vairable . value) pair in a binding list"
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding"
  (cdr binding))

(defun lookup (var bindings)
  "Get the value aprt (for var) from a binding list"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var. value) pair to a binidng list"
  (cons (cons var val)
        (if (eq bindings no-binding)
          nil
          bindings)))


(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (starts-with (first pattern) '?*)))


(defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))

(defun pat-match (pattern input &optional (bindings no-binding))
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings
                                          ))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      (let ((pos (position (first pat) input :start start :test #'equal)))
        (if (null pos)
          fail
          (let ((b2 (pat-match pat (subseq input pos) bindings)))
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              (match-variable var (subseq input 0 pos) b2))))))))

         

(pat-match '((?* ?p) need (?* ?x)) '(Mr Hulot and I need a vacation))
(pat-match '((?* ?x) is a (?* ?y)) '(what he is a fool))



(pat-match '(?X) '(a))

(pat-match '(I need a ?X) '(I need a vacation))

(pat-match '(a ?X) '(a vacation))

(sublis (pat-match '(i need a ?x) '(i need a vacation))
        '(what would it mean to you if you got a vacation))


(pat-match '(I need a ?X) '(I really need a vacation))

(pat-match '(This is easy) '(This is easy))

(pat-match '(?x is ?x) '((2 + 2) is 4))

(pat-match '(?x is ?x) '((2 + 2) is (2 + 2)))

(pat-match '(?p need . ?x) '(i need a long vacation))







; sublis memo
;(sublis '((?X . vacation))
;        '(what would it mean to you if you got a ?X ?))



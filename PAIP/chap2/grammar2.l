;; PAIPl chapter2 

(defun sentence () (append (noun-phrase) (verb-phrase)))

(defun adj* ()
  (if (= (random 2) 0)
      nil
      (append (adj) (adj*))))

(defun pp* ()
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      nil))


(defun noun-phrase () (append (article) (adj*) (noun) (pp*)))
(defun verb-phrase () (append (verb) (noun-phrase)))
(defun article() (one-of '(the a)))
(defun noun ()   (one-of '(man ball woman table)))
(defun verb ()   (one-of '(hit took saw liked)))
(defun pp () (append (prep) (noun-phrase)))
(defun adj() (one-of '(big little bule green adiabatic)))
(defun prep () (one-of '(to in by with on)))

(defun one-of (set)
  "Pick one element of set. and make a list of it"
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list of random." 
  (elt choices (random (length choices))))

(trace sentence)
(sentence)

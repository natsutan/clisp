;; PAIP 2.3 Rule-Based Solution

(defparameter *simple-grammar* 
  '((sentence -> (noun-pahrse verb-phrase))
    (noun-pahrse -> (Article Noun))
    (verb-phrase -> (Verb noun-pahrse))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English/")

(defvar *grammer*  *simple-grammar* 
  "The grammar used by generate. Initialy, this is *simple-grammar*, but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left-hand side of a rule"
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule"
  (rest (rest rule)))

(defun rewrite (category)
  "return a list of the possible rewrites for this category"
  (rule-rhs (assoc category  *grammer* )))

(defun generate (phrase)
  "Generate a random sentence or pharse"
;  (break)
  (cond ((listp phrase) 
	 (mappend #'generate phrase))
	((rewrite phrase)
	 (generate (random-elt (rewrite phrase))))
	(t (list phrase))))

(defun random-elt (choices)
  "Choose an element from a list of random." 
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))




(generate 'sentence)
(generate 'noun)




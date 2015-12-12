;;; Resolution-based Q&A system.
;;;
;;; Pranav Ravichandran <me@onloop.net>

(defstruct
  (fact
    (:constructor deffact (func &rest params)))
  func params)

(defstruct
  (rule
    (:constructor defrule (facts
                           implications
                           &aux (cnf (cnf-rule facts implications)))))
  facts implications cnf)

(defstruct
  (wff
    (:constructor defwff (operator wffs)))
  operator wffs)

(defun cnf-rule (facts implications)
  (defwff 'or (demorgan (defwff 'not facts)) (demorgan implications)))

(defun demorgan (wff &aux
                     (operator (if (wff-p wff) (wff-operator wff)))
                     (operand (if (wff-p wff) (wff-wffs wff))))
  (if (fact-p wff)
    wff
    (cond ((and (equal operator 'not)
                (notevery #'fact-p operand))
           (cond ((equal (wff-operator (car operand)) 'and)
                  (defwff 'or (mapcar
                                #'(lambda (x)
                                    (demorgan (defwff 'not (list x))))
                                (wff-wffs (car operand)))))
                 ((equal (wff-operator (car operand)) 'or)
                  (defwff 'and (mapcar
                                 #'(lambda (x)
                                     (demorgan (defwff 'not (list x))))
                                 (wff-wffs (car operand)))))
                 ((equal (wff-operator (car operand)) 'not)
                  (wff-wffs (car operand)))))
          ((notevery #'fact-p operand)
           (defwff operator (mapcar #'(lambda (x) (demorgan x)) operand)))
          ('else wff))))

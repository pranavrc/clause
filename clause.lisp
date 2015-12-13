;;; Resolution-based Q&A system.
;;;
;;; Pranav Ravichandran <me@onloop.net>

(defstruct
  (var
    (:constructor var (x))) x)

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

(defun cnf-rule (facts-wff implications-wff)
  (cnf-wff (defwff 'or (list (demorgan (defwff 'not facts-wff))
                             (demorgan implications-wff)))))

(defun cnf-wff (wff)
  (let ((dwff (demorgan wff)))
    (cond
      ((fact-p dwff) dwff)
      ((equal (wff-operator dwff) 'or)
       (defwff (wff-operator dwff)
               (remove-if #'(lambda (x)
                              (some #'(lambda (y)
                                        (isnegation x y)) (wff-wffs dwff)))
                          (wff-wffs dwff))))
      ((equal (wff-operator dwff) 'and)
       (defwff (wff-operator dwff)
               (if (every #'(lambda (x)
                              (notany
                                #'(lambda (y)
                                    (isnegation x y))
                                (wff-wffs dwff))) (wff-wffs dwff))
                 (wff-wffs dwff))))
      ((equal (wff-operator dwff) 'not) dwff))))

(defun fequal (ewff-1 ewff-2)
  (every #'(lambda (pair)
             (if (typep (car pair) 'list)
               (fequal (car pair) (cdr pair))
               (or (equal (car pair) (cdr pair))
                   (or (var-p (car pair)) (var-p (cdr pair))))))
         (pairlis ewff-1 ewff-2)))

(defun isnegation (wff-1 wff-2)
  (cond ((and (fact-p wff-1) (wff-p wff-2))
         (if (fequal (list 'not (evaluate wff-1)) (evaluate wff-2)) t nil))
        ((and (fact-p wff-2) (wff-p wff-1))
         (if (fequal (list 'not (evaluate wff-2)) (evaluate wff-1)) t nil))))

(defun evaluate (wff)
  (cond
    ((fact-p wff) (append (list (fact-func wff)) (fact-params wff)))
    ((wff-p wff)
     (let ((dwff (demorgan wff)))
       (append (list (wff-operator dwff))
               (mapcar #'(lambda (x) (evaluate x)) (wff-wffs dwff)))))))

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

(defun resolve (facts rules goal &aux
                      (goal-negation (cnf-wff (defwff 'not (list goal))))
                      (ans-literal (deffact 'ans (goal-vars goal))))
  ())

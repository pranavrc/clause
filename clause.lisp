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
  (cnf-wff (defwff 'or (list (cnf-wff (defwff 'not (list facts-wff)))
                             (cnf-wff implications-wff)))))

(defun cnf-wff (wff)
  (let ((dwff (flatten (demorgan wff))))
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
  (if (= (length ewff-1) (length ewff-2))
    (every #'(lambda (pair)
               (if (typep (car pair) 'list)
                 (fequal (car pair) (cdr pair))
                 (or (equal (car pair) (cdr pair))
                     (or (var-p (car pair)) (var-p (cdr pair))))))
           (pairlis ewff-1 ewff-2))))

(defun get-substitutions (ewff-1 ewff-2)
  (let* ((resolved (set-difference ewff-1 ewff-2 :test 'equal))
         (resolved-wff (remove-if-not #'(lambda (x) (equal (car x) 'not)) resolved))
         (resolved-fact (remove-if #'(lambda (x) (equal (car x) 'not)) resolved)))
    (if (> (length resolved) 0)
      (get-substitutions-helper (car resolved-wff) (list 'not (car resolved-fact))))))

(defun get-substitutions-helper (ewff-1 ewff-2)
  (if (= (length ewff-1) (length ewff-2))
    (remove nil
            (mapcar #'(lambda (pair)
                        (if (typep (car pair) 'list)
                          (get-substitutions-helper (car pair) (cdr pair))
                          (if
                            (or (var-p (car pair)) (var-p (cdr pair)))
                            (if (var-p (car pair)) (cdr pair) (car pair)))))
                    (pairlis ewff-1 ewff-2)))))

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

(defun flatten (wff &aux
                    (dwff (demorgan wff))
                    (operator (if (wff-p dwff) (wff-operator dwff)))
                    (operand (if (wff-p dwff) (wff-wffs dwff))))
  (if (or (fact-p dwff) (every #'fact-p operand))
    dwff
    (cond ((equal operator 'not)
           (defwff 'not (mapcar #'flatten operand)))
          ((equal operator 'or)
           (defwff 'or (apply #'append
                              (mapcar #'(lambda (x)
                                          (if (wff-p x)
                                            (if (equal (wff-operator x) 'or)
                                              (mapcar #'flatten (wff-wffs x))
                                              (list (flatten x)))
                                            (list x)))
                                      operand))))
          ((equal operator 'and)
           (defwff 'and (apply #'append
                               (mapcar #'(lambda (x)
                                           (if (wff-p x)
                                             (list (flatten x))
                                             (list x)))
                                       operand)))))))

(defun resolve (facts rules goal &optional (negate t) (ans '()) &aux
                      (goal-negation
                        (if negate
                          (cnf-wff (defwff 'not (list goal)))
                          goal)))
  (if (and (wff-p goal-negation) (not (wff-wffs goal-negation)))
    ans
    (loop for wff in (append facts rules)
          for cnf = (demorgan
                      (flatten
                        (defwff
                          'or
                          (cond ((or (fact-p wff) (wff-p wff))
                                 (append (list goal-negation) (list wff)))
                                ((rule-p wff) (append (list goal-negation)
                                                      (list (rule-cnf wff))))))))
          for ecnf = (cnf-wff cnf)
          when (not (fequal (evaluate cnf) (evaluate ecnf)))
          return (resolve facts rules (cnf-wff (flatten ecnf)) nil
                          (append ans (get-substitutions
                                        (evaluate (flatten cnf))
                                        (evaluate (flatten ecnf))))))))


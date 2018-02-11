;;;;;;;;;;;;;;;;;
;; Ejercicio 2 ;;
;;;;;;;;;;;;;;;;;

;; 2.1


;;;;;;;;;;;;;;;;;
;; Ejercicio 3 ;;
;;;;;;;;;;;;;;;;;

;; 3.1
(defun combine-elt-list (elt lst)
	(if (null lst)
		nil
		(cons (list elt (first lst)) (combine-elt-list elt (rest lst)))))

;; 3.2
(defun combine-lst-lst (lst1 lst2)
	(unless (or (null lst1) (null lst2))
		nil
	(append (combine-elt-list (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))

;; 3.3
(defun combine-list-of-lsts (lstolsts)
	(if (null (rest lstolsts))
		(first lstolsts)
		(combine-lst-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts)))))
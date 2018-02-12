;;;;;;;;;;;;;;;;;
;; Ejercicio 2 ;;
;;;;;;;;;;;;;;;;;

;; 2.1
;;
;; Finds a root of f between the points a and b using bisection.
;;
;; If f(a)f(b)>0 there is no guarantee that there will be a root in the
;; interval, and the function will return NIL.
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
(defun bisect (f a b tol)
	;Genera f(x) en en �mbito de la ejecuci�n actual de la funci�n de bisectriz
	(let ((fx (/ (+ a b) 2)))
	(if (> (* a b) 0)
		;Si a y b son ambas positivas o neativas, devuelve NILL
		nil
		(if (< (- b a) tol)
			fx
			(if (> fx 0)
				(bisect f a fx tol)
				(bisect f fx b tol))))))


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
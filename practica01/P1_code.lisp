;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   PRACTICA 1
;;;
;;;   Autores: Celia San Gregorio Moreno
;;;			   Alvaro Martinez Morales
;;;
;;;   Grupo: 2363
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;	Apartado 1.1
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prod-escalar-rec (x y)
;;;
;;; Calcula el producto escalar de dos vectores de forma recursiva.
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;; 	   y: vector, representado como una lista
;;; OUTPUT: producto escalar de x e y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prod-escalar-rec (x y)
	(if (or (null x) (null y))
		0
		(+ (* (first x) (first y)) (prod-escalar-rec (rest x) (rest y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;;
;;; Calcula la similitud coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;;
;;; INPUT: x: vector, representado como una lista
;;; 	   y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sc-rec (x y)
	(if (or (= (prod-escalar-rec x x) 0) (= (prod-escalar-rec y y) 0))
		nil
		(/ (prod-escalar-rec x y) (* (sqrt (prod-escalar-rec x x)) (sqrt (prod-escalar-rec y y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prod-escalar-mapcar (x y)
;;;
;;; Calcula el producto escalar de dos vectores usando mapcar.
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;; 	   y: vector, representado como una lista
;;; OUTPUT: producto escalar de x e y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prod-escalar-mapcar (x y)
	(if (or (null x) (null y))
		nil
		 (reduce #'+ (mapcar #'* x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;;
;;; Calcula la similitud coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;;
;;; INPUT: x: vector, representado como una lista
;;; 	   y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y
(defun sc-mapcar (x y)
	(if (or (= (prod-escalar-mapcar x x) 0) (= (prod-escalar-mapcar y y) 0))
		nil
		(/ (prod-escalar-mapcar x y) (* (sqrt (prod-escalar-mapcar x x)) (sqrt (prod-escalar-mapcar y y))))))

;;;;;;;;;;;;;;;;;;;
;;;	Apartado 1.2
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crea-vectores-conf (cat vs conf)
;;;
;;; Devuelve un lista de vectores cuya similitud respecto 
;;; a una categoria es mayor que conf (nivel de confianza)
;;;
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; 	   vs: vector de vectores
;;; 	   conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud con respecto a la categoría es superior al
;;; nivel de confianza. No están ordenados.
(defun crea-vectores-conf (cat vs conf)
	(if (null vs)
		nil
		; Comprueba que la similitud coseno no es NIL 
		; y que es mayor que el parametro conf
		(if (and (not (null (sc-rec cat (first vs)))) (> (sc-rec cat (first vs)) conf))
			(append (list (first vs)) (crea-vectores-conf cat (rest vs) conf))
		; Si no es mayor que conf, sigue evaluando vs
			(crea-vectores-conf cat (rest vs) conf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (cat vs conf)
;;;
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; 	   vs: vector de vectores
;;; 	   conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud con respecto a la categoría es superior al
;;; nivel de confianza, ordenados
(defun sc-conf (cat vs conf)
	(sort (copy-list (crea-vectores-conf cat vs conf)) #'> :key (lambda (x) (sc-rec x cat))))

;;;;;;;;;;;;;;;;;;;
;;;	Apartado 1.3
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crea-vectores-class (cats texts func)
;;;
;;; Devuelve un vector de vectores con el identificador 
;;; de la categoria y el resultado de la similitud coseno
;;; entre la categoria y cada vector de texts.
;;; Sin ordenar.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; 	   texts: vector de vectores, representado como una lista de listas
;;; 	   func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno, sin ordenar
;;;
(defun crea-vectores-class (cats texts func)
	(if (null texts)
		nil
		; Crea pares (id_categoria similitud_coseno) con la categoria
		; pasada como argumento y cada elemento del vector de vectores texts
		(append (list (list (first cats) (funcall func (rest cats) (rest (first texts))))) (crea-vectores-class cats (rest texts) func))))

;;;;;;;;;;;;;;;;;;;;;;;;sc;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vector-class (cats texts func)
;;;
;;; A partir de un vector de vectores compuesto por el identificador
;;; de la categoria y el resultado de la similitud coseno, selecciona
;;; el par (id sc) con mayor similitud coseno.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; 	   texts: vector de vectores, representado como una lista de listas
;;; 	   func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno, sin ordenar
;;;
(defun select-vector-class (cats texts func)
	(first (sort (copy-list (crea-vectores-class cats texts func)) #'> :key #'second)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;;;
;;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; 	   texts: vector de vectores, representado como una lista de listas
;;; 	   func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
	(if (or (null cats) (null texts))
		nil
		; Comprueba la similitud coseno de cada vector categoria con
		; todos los elementos del vector de vectores texts
		(list (select-vector-class (first cats) texts func) (select-vector-class (first (rest cats)) texts func))))

;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;	Apartado 2.1 (Mi modificación)
;;;;;;;;;;;;;;;;;;;
(defun bisect (f a b tol)
	;Genera x en en ámbito de la ejecución actual de la función de bisectriz
	(let ((x (/ (+ a b) 2)))
	(if (>= (* (funcall f a) (funcall f b)) 0)
		;Si f(a) y f(b) son ambas positivas o negativas, devuelve NIL
		nil
		(if (< (- b a) tol)
			;Si b - a < tol, la función devuelve x como resultado
			x
			; Si f(a) * f(x) < 0, la función busca en [a, x]
			(if (< (* (funcall f a) (funcall f x)) 0)
				;Si no, reposicionamos uno de los puntos como x y continuamos.
				(bisect f a x tol)
				(bisect f x b tol))))))

;;;;;;;;;;;;;;;;;;;
;;;	Apartado 2.1 (Funcion de Amartinez-h)
;;;;;;;;;;;;;;;;;;;
(defun bisect (f a b tol)
	;Genera f(x) en en ámbito de la ejecución actual de la función de bisectriz
	(let ((fx (/ (+ a b) 2)))
	(if (> (* (funcall f a) (funcall f b)) 0)
		;Si f(a) y f(b) son ambas positivas o neativas, devuelve NILL
		nil
		(if (< (- (funcall f b) (funcall f a)) tol)
			;Si f(b) - f(a) < tol, la función devuelve f(x) como resultado
			fx
			(if (> fx 0)
				;Si no, reposicionamos uno de los puntos como f(x) y continuamos.
				(bisect f a fx tol)
				(bisect f fx b tol))))))

;;;;;;;;;;;;;;;;;;;
;;;	Apartado 2.2 (Mi modificación)
;;;;;;;;;;;;;;;;;;;
(defun allroot (f lst tol)
	(if (null (rest lst))
		;Si no quedan al menos dos números en la lista, la recursividad termina
		nil
		;Comprueba si el signo de f(lst[i]) y f(lst[i+1]) es distinto. 
		;Es decir, que  f(lst[i]) * f(lst[i+1]) < 0
		(if (< (* (funcall f (first lst)) (funcall f (second lst))) 0)
			;Calcula la bisectriz de los dos primeros elementos de la lista y pasa 
			;la lista a la llamada recursiva, quitando el primer elemento 
			(cons (bisect f (first lst) (second lst) tol) (allroot f (rest lst) tol))
			;Si no se cumple la condición, continúa evaluando los
			;elementos de la lista
			(allroot f (rest lst) tol))))

;;;;;;;;;;;;;;;;;;;
;;;	Apartado 2.2 (Funcion de Amartinez-h)
;;;;;;;;;;;;;;;;;;;
(defun allroot (f lst tol)
	(if (null (rest lst))
		;Si no quedan, al menos, dos números en la lista, la recursividad termina
		nil
		;Calcula la bisectriz de los dos primeros elementos de la lista y pasa a 
		;llamada recursiva la lista quitando el primer elemento 
		(cons (bisect f (first lst) (rest lst) tol) (allroot f (rest lst) tol))))
	
;;;;;;;;;;;;;;;;;;;
;;;	Apartado 2.3 (EN CONSTRUCCION)
;;;;;;;;;;;;;;;;;;;
(defun allind (f a b N tol)
	;Calcula el delimitador de cada seccion del intervalo
	(let ((i (/ (- b a) N)))
	(find-roots f a (+ a i) a i N tol)))
	

(defun find-roots (f a b st i N tol)
	;Comprueba si se ha alcanzado la ultima seccion del intervalo
	(if (= b (+ st (* N i) 1))
		nil
		(cons (bisect f a b tol) (find-roots f b (+ b i) st i N tol))))


(defvar fseno)
(setf fseno (lambda(x) (sin (* 6.28 x))))

;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;

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
	
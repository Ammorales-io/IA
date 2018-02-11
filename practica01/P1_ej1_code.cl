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
		(if (and (not (null (sc-rec (first vs) cat))) (> (sc-rec (first vs) cat) conf))
			(append (list (first vs)) (crea-vectores-conf cat (rest vs) conf))
		; Si no es mayor que conf, sigue evaluando vs
			(crea-vectores-conf cat (rest vs) conf))))

;;;;;;;;;;;;;;;;;;;
;;;	Apartado 1.3
;;;;;;;;;;;;;;;;;;;

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
(defun sc-classifier (cats texts)
	(if (or (null cats) (null texts))
		nil
		; Comprueba la similitud coseno de cada vector categoria con
		; todos los elementos del vector de vectores texts
		(list (select-vector-class (first cats) texts) (select-vector-class (first (rest cats)) texts))))

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
(defun crea-vectores-class (cats texts)
	(if (null texts)
		nil
		; Crea pares (id_categoria similitud_coseno) con la categoria
		; pasada como argumento y cada elemento del vector de vectores texts
		(append (list (list (first cats) (sc-rec (rest cats) (rest (first texts))))) (crea-vectores-class cats (rest texts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun select-vector-class (cats texts)
	(first (sort (copy-list (crea-vectores-class cats texts)) #'> :key #'second)))




	
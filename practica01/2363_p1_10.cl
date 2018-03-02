;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   PRACTICA 1
;;;
;;;   Autores: Celia San Gregorio Moreno
;;;            Alvaro Martinez Morales
;;;
;;;   Grupo: 2363
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  EJERCICIO 1  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 1.1 ;;;
;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;
;;; Apartado 1.2 ;;;
;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;
;;; Apartado 1.3 ;;;
;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  EJERCICIO 2  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 2.1 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bisect (f a b tol)
;;;
;;; Calcula la raiz de una función dada en un intervalo dado mediante 
;;; la técnica de la bisección a partir de un punto de tolerancia
;;; determinado.
;;;
;;; INPUT:	f: Función de la que calcular la raiz
;;; 		a: Mínimo del intervalo
;;;			b: Máximo del intervalo
;;;			tol: Resultado mínimo para aceptar un resultado 
;;; OUTPUT: Raiz de la función en el intervalo establecido
;;;
(defun bisect (f a b tol)
  ;Genera x en en ámbito de la ejecución actual de la función de bisectriz
  (let ((x (/ (+ a b) 2)))
    (if (> (* (funcall f a) (funcall f b)) 0)
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


;;;;;;;;;;;;;;;;;;;;
;;; Apartado 2.2 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alrrot (f lst tol)
;;;
;;; Calcula la raiz de una función dada en cada par de intervalos
;;; consecutivos de una lista ordenada
;;;
;;; INPUT:	f: Función de la que calcular la raiz
;;; 		lst: Listado de intervalos
;;;			tol: Resultado mínimo para aceptar un resultado 
;;; OUTPUT: Listado de las raices de la función en los intervalos establecidos
;;;
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


;;;;;;;;;;;;;;;;;;;;
;;; Apartado 2.3 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-interval-list (a b i)
;;;
;;; Crea la lista de números entre a y b a distancia i
;;;
;;; INPUT:	a: Mínimo del intervalo
;;; 		b: Máximo del intervalo
;;;			i: Distancia entre elementos
;;; OUTPUT: lista de números entre a y b a distancia i
;;;
(defun make-interval-list (a b i)
  ;En la comparación usamos b+i/2 como un sobrepaso a errores por decimales
  ;que hacía que a, al llegar a b, fuera ligeramente mayor que b y no
  ;registrase el último número de la lista
  (if (> a (+ b (/ i 2)))
      nil
    (cons a (make-interval-list (+ a i) b i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; allind (f a b N tol)
;;;
;;; Calcula todas las raices de una función en los valores establecidos
;;; entre los subintervalos de un intervalo dado.
;;;
;;; INPUT:	f: Función de la que calcular las raices
;;; 		a: Mínimo del intervalo principal
;;;			b: Máximo del intervalo principal
;;;			N: Valor que, siendo exponente de 2, se usará para divir el inervalo
;;;				principal en sub-intervalos
;;;			tol: Resultado mínimo para aceptar un resultado 
;;; OUTPUT: Listado de las raices de la función en los intervalos establecidos
;;;
(defun allind (f a b N tol)
  ; Aunque se podría itroducir directamente el contenido de interval-lst en allroot,
  ; hemos optado por crear una variable local para aumentar la claridad del código.
  ; A.K.A.: #NoALosChurros
  (let ((interval-lst (make-interval-list a b (/ (+ (abs b) (abs a)) (expt 2 N)))))
		(allroot f interval-lst tol)))
	


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  EJERCICIO 3  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 3.1 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;;
;;; Genera un listado con la combinación de un átomo con todos los
;;; átomos de un listado.
;;;
;;; INPUT:	elt: Átomo a combinar
;;; 		lst: Listado a combinar
;;; OUTPUT: Listado con todas las combinaciones del átomo y los elementos del listado
;;;
(defun combine-elt-lst (elt lst)
  (if (null lst)
      nil
    (cons (list elt (first lst)) (combine-elt-lst elt (rest lst)))))

(combine-elt-lst 'a nil) ;; --> NIL
(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 3.2 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst (lst1 lst2)
;;;
;;; Genera un listado con el producto cartesiano de elementos de dos
;;; listas dadas.
;;;
;;; INPUT:	lst1: Lista a combinar
;;; 		lst2: Lista a combinar
;;; OUTPUT: Listado con el producto cartesiano de los elementos de ambos listados
;;;
(defun combine-lst-lst (lst1 lst2)
  (unless (or (null lst1) (null lst2))
    nil
    (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))

(combine-lst-lst nil nil) ;; --> NIL
(combine-lst-lst '(a b c) nil) ;; --> NIL
(combine-lst-lst NIL '(a b c)) ;; --> NIL
(combine-lst-lst '(a b c) '(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 3.3 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lst (lstolsts) 
;;;	!Warning: Incompleta, el resultado conseguido no es correcto aun
;;;
;;; Genera el producto cartesiano de los elementos de los sublistados
;;; facilitados mediante un listado.
;;;
;;; INPUT:	lstolsts: listado con los sublistados
;;; OUTPUT: Listado con el producto cartesiano de los elementos de los sublistados
;;;
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
      nil
    (mapcar #'combine-elt-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts)))))

(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL
(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))



;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  EJERCICIO 5  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 5.4 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
;;;
;;; INPUT:	end: nodo destino de nuestra búsqueda
;;;		queue: cola con los caminos de búsqueda
;;;		net: lista representativa de nuestra red de nodos
;;; OUTPUT:	lista con el camino más corto al nodo destino
(defun bfs (end queue net)
  (if (null queue)
      ;;Establecemos '() como caso base
      '()
    ;Cogemos el primer camino de la cola
    (let* ((path (first queue)) 
           ;Reconocemos el primer elemento del camino como el nodo
           (node (first path))) 
      (if (eql node end)
          ;Si hemos contrado el nodo destino, devolvemos el la inversa del camino evaluado actual
          (reverse path) 	
        ;Si no, metemos todos los caminos a los que se puede llegar desde el nodo identificado en la cola
        (bfs end
             (append (rest queue)
                     (new-paths path node net))
             net))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-paths
;;
;; INPUT: 	path: camino actual
;;		node: nodo actual
;;		net: lista representativa de nuestra red de nodos
;; OUTPUT:	lista de nodos a los que se puede llegar desde el nodo actual
(defun new-paths (path node net)
  (mapcar #'(lambda(n)
              (cons n path))
    (rest (assoc node net)))) 	;; Genera caminos nuevos juntando el camino previo a todos aquellos
				;; con todos aquellos nodos accesibles desde el nodo actual
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 5.5 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shortest-path through Breadth-first-search in graphs
;;;
;;; Encuentra el menor camino dado que introduce el primer nodo 
;;; en el grafo y ejecuta una búsqueda de anchura hasta el nodo
;;; destino, la cual, por defecto, encontrará el camino uno de
;;; (o el) caminos con menor longitud.
;;;
;;;
;;; INPUT:	start: nodo origen de nuestra búsqueda
;;;		end: nodo destino de nuestra búsqueda
;;;		net: lista representativa de nuestra red de nodos
;;; OUTPUT:	lista con el camino más corto al nodo destino
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 5.6 ;;;
;;;;;;;;;;;;;;;;;;;;

(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
;; --> (A D F)
;;  
;; Este resultado sucede porque


;;;;;;;;;;;;;;;;;;;;
;;; Apartado 5.7 ;;;
;;;;;;;;;;;;;;;;;;;;

(shortest-path 'f 'c '((a b c d e) (b a d e f) (c a g) (d a b g h) (e a b g h) (f b h) (g c d e h) (h d e f g)))

;;;;;;;;;;;;;;;;;;;;
;;; Apartado 5.8 ;;;
;;;;;;;;;;;;;;;;;;;;


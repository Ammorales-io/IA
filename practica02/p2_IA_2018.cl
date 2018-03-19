;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;    LAB GROUP: 2363
;;    Couple: 10
;;    Author 1: Celia San Gregorio Moreno 
;;    Author 2: Álvaro Martínez Morales
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-goal-test          ; Reference to a function that determines whether 
                       ; a state fulfills the goal 
  f-h                  ; Reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-search-state-equal ; Reference to a predicate that determines whether
                       ; two nodes are equal, in terms of their search state
  operators)           ; list of operators (references to functions) to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

(defparameter *white-holes*  
  '((Avalon Mallory 6.4) (Avalon Proserpina 8.6) 
    (Davion Proserpina 5) (Davion Sirtis 6) 
    (Katril Mallory 10) (Katril Davion 9)
    (Kentares Avalon 3) (Kentares Katril 10) (Kentares Proserpina 7)
    (Mallory Katril 10) (Mallory Proserpina 15) 
    (Proserpina Avalon 8.6) (Proserpina Davion 5) (Proserpina Mallory 15) (Proserpina Sirtis 12)
    (Sirtis Proserpina 12) (Sirtis Davion 6)))

(defparameter *worm-holes*  
  '((Avalon Kentares 4) (Avalon Mallory 9)
    (Davion Katril 5) (Davion Sirtis 8)  
    (Katril Mallory 5) (Katril Davion 5) (Katril Sirtis 10)
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Mallory Avalon 9) (Mallory Katril 5) (Mallory Proserpina 11)
    (Proserpina Kentares 12) (Proserpina Mallory 11) (Proserpina Sirtis 9)
    (Sirtis Proserpina 9) (Sirtis Davion 8) (Sirtis Katril 10)))
 
(defparameter *sensors* 
  '((Avalon 15) (Mallory 12) (Kentares 14) (Davion 5) (Proserpina 7) (Katril 9) (Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Katril Proserpina))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state cost)
;;             where the first element is the name of a state and the second
;;             a number estimating the cost to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
(defun f-h-galaxy (state sensors)
  (second (assoc state sensors)))

;;;
;;; EJEMPLOS
;;;
(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth  *sensors*) ;-> NIL


;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;

;Crea una lista de tripletes con state como planeta origen
;y cada uno de los planetas en el agujero blanco o de gusano
;a los que puede acceder.
(defun make-colindant-list (state hole-map)
  ;Si hemos llegado al final de la lista
  ;asociativa, la función termina.
  (if (null hole-map)
      nil
    ;Sino, comprueba si el planeta de origen (state)
    ;coincide con el planeta de origen del primer triplete.
    (if (equal state (first (first hole-map)))
        ;Si coincide, crea una lista de tripletes.
        (cons (first hole-map) 
              (make-colindant-list state (rest hole-map)))
      ;Sino, avanza en la lista asociativa hole-map.
      (make-colindant-list state (rest hole-map)))))

(make-colindant-list 'Avalon *white-holes*)
;;; ((AVALON MALLORY 6.4) (AVALON PROSERPINA 8.6))

;Elimina los planetas prohibidos de una lista de tripletes
;que tienen como origen un planeta y como destino los
;planetas directamente accesibles desde el origen.
(defun filter-forbidden-planets (colindant-map planets-forbidden)
  ;Si hemos llegado al final de la lista
  ;de planetas prohibidos o no hay planetas
  ;prohibidos, la función devuelve el mapa resultante.
  (if (null planets-forbidden)
      colindant-map
    (let ((bad-planet (first planets-forbidden)))
      ;En caso contrario comprueba si, por cada triplete, existe al 
      ;menos uno que tiene como segundo elemento (es decir, como destino) un planeta prohibido.
      (if (> (count bad-planet colindant-map :test #'equal :key #'second) 0)
          ;Si se cumple la condición, elimina todos tripletes que
          ;contengan un planeta prohibido como destino.
          (filter-forbidden-planets
           (remove bad-planet colindant-map :key #'second) (rest planets-forbidden))
        ;Si no, deja la lista de tripletes como está y busca más
        ;planetas prohibidos para filtrar.
        (filter-forbidden-planets colindant-map (rest planets-forbidden))))))

;A partir de una lista de tripletes que representa un conjunto de
;agujeros negros o blancos, devuelve una lista de tripletes
;con los planetas a los que se puede acceder a partir del planeta
;de origen state.
(defun allowed-planets (state hole-map planets-forbidden)
  (let ((colindant-map (make-colindant-list state hole-map)))
    ;Filtra los planetas prohibidos de la lista de tripletes
    ;donde cada triplete tiene a state como origen.
    (filter-forbidden-planets colindant-map planets-forbidden)))

;Crea una lista de acciones a partir de una lista de tripletes. Esta
;lista de tripletes contiene todos los viajes que se pueden hacer
;desde el planeta origen hasta los sucesores.
;Los planetas prohibidos han sido filtrados de la lista previamente
;por la función allowed-planets.
(defun make-action-list (hole-map hole-type)
  ;Si hemos llegado al final de la lista de tripletes,
  ;la función termina.
  (if (null hole-map)
      nil
    (let ((triplet (first hole-map)))
      ;Si el grafo corresponde a uno con agujeros blancos,
      ;crea una lista de acciones permitidas en este grafo.
      (if (equal hole-type "white")
          (cons
           (make-action :name 'navigate-white-hole :origin (first triplet) :final (second triplet) :cost (third triplet))
           (make-action-list (rest hole-map) hole-type))
        ;Si el grafo corresponde a uno con agujeros de gusano,
        ;crea una lista de acciones permitidas en este grafo.
        (cons
         (make-action :name 'navigate-worm-hole :origin (first triplet) :final (second triplet) :cost (third triplet))
         (make-action-list (rest hole-map) hole-type))))))

;Obtiene todas las acciones que se pueden realizar desde
;el planeta de origen state hasta sus sucesores inmediatos,
;diferenciando entre agujeros blancos y de gusano.
;
;Todos los planetas prohibidos se filtran mediante la
;función allowed-planets.
(defun navigate (state hole-map planets-forbidden)
  (cond
   ;CASO 1: El planeta no pertenece a la lista de planetas.
   ((null (member state *planets* :test #'equal))
    nil)
   ;CASO 2: El grafo tiene agujeros blancos.
   ;Se crea una lista de acciones permitidas para el planeta
   ;origen state en dicho grafo.
   ((equal hole-map *white-holes*)
    (make-action-list (allowed-planets state hole-map planets-forbidden) "white"))
   ;CASO 3: El grafo tiene agujeros de gusano.
   ;Se crea una lista de acciones permitidas para el planeta
   ;origen state en dicho grafo.
   ((equal hole-map *worm-holes*)
    (make-action-list (allowed-planets state hole-map planets-forbidden) "worm"))
   (t NIL)))
	 
;Operador que devuelve una lista de acciones que se
;pueden hacer a partir del estado state, sobre un
;grafo con agujeros negros.	
(defun navigate-white-hole (state white-holes)
  (navigate state white-holes nil))

;Operador que devuelve una lista de acciones que se
;pueden hacer a partir del estado state, sobre un
;grafo con agujeros de gusano.	
(defun navigate-worm-hole (state worm-holes planets-forbidden)
  (navigate state worm-holes planets-forbidden))


                 
;(defun navigate-white-hole (state white-holes)
  ;(navigate (make-colindant-list state white-holes)))

;(defun navigate-worm-hole (state worm-holes planets-forbidden)
  ;(navigate (member (first planets-forbidden 
                     ;(make-colindant-list state worm-holes) 
                     ;:test-not #'equal
                     ;:key #'second))))
;; Habría que hacer que comprobase, creo, cada uno de los elementos del planets-forbidden, 
;; por eso hay un union. ¿Quizás re-implementar 1.3.3? 

;;;
;;; EJEMPLOS
;;;
(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))

(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*)  ;-> NIL


;;
;; END: Exercise 2 -- Navigation operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3A -- Goal test
;;

;Comprueba si el nodo pasado como argumento es un estado objetivo
(defun f-goal-test-galaxy (node planets-destination planets-mandatory)
  ;Si el nodo está entre la lista de planetas destino,
  ;comprueba que los nodos antecesores hayan pasado por
  ;los planetas obligatorios.
  (if (member (node-state node) planets-destination)
      (f-mandatory-test node planets-mandatory)
    nil))

;Comprueba si en el camino del nodo raíz al nodo actual
;se ha pasado por los planetas obligatorios.
(defun f-mandatory-test (node planets-mandatory)
  ;Si hemos llegado al nodo raíz...
  (if (null node)
      ;Y la lista de planetas obligatorios está vacía,
      ;hemos pasado por todos los planetas obligatorios.
      (if (null planets-mandatory)
          T
        nil)
    ;Si aún no hemos llegado al nodo raíz, comprueba si el nodo actual
    ;es un planeta obligatorio.
    (if (member (node-state node) planets-mandatory)
        ;Si lo es, elimina el nodo de la lista de planetas obligatorios.
        (f-mandatory-test (node-parent node) (remove (node-state node) planets-mandatory))
      ;Sino, comprueba si el nodo padre es obligatorio.
      (f-mandatory-test (node-parent node) planets-mandatory))))

;;;
;;; EJEMPLOS
;;;
(defparameter node-01
   (make-node :state 'Avalon))
(defparameter node-02
   (make-node :state 'Kentares :parent node-01))
(defparameter node-03
   (make-node :state 'Katril :parent node-02))
(defparameter node-04
   (make-node :state 'Kentares :parent node-03))

(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)); -> T


;;
;; END: Exercise 3A -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun f-search-state-equal-galaxy (node-1 node-2 &optional planets-mandatory)
 ...)

;;;
;;; EJEMPLOS
;;;
(f-search-state-equal-galaxy node-01 node-01) ;-> T
(f-search-state-equal-galaxy node-01 node-02) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3B -- Node equality
;;

;;
;; END: Exercise 3B -- Node equality
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;
;;
(defparameter *galaxy-M35* 
  (make-problem 
   :states            *planets*          
   :initial-state     *planet-origin*
   :f-goal-test       #'(lambda (node) 
                          (f-goal-test-galaxy node *planets-destination*
                                                   *planets-mandatory*))
   :f-h               #'(lambda (state)
                          (f-h-galaxy state *sensors*))
   :operators         (list #'(lambda (node)
                                (navigate-white-hole (node-state node) *white-holes*))
                            #'(lambda (node)
                                (navigate-worm-hole (node-state node) *worm-holes* *planets-forbidden*))))) 

;;
;;  END: Exercise 4 -- Define the galaxy structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 5: Expand node
;;

;Obtiene la lista de nodos a los que se puede acceder
;desde el nodo actual, utilizando todos los operadores
;(agujeros blancos y de gusano).
(defun expand-node (node problem)
  (expand-node-aux node (problem-operators problem) problem))

(defun expand-node-aux (node op-list problem)
  ;Si llega al final de la lista de operadores, termina.
  (if (null op-list)
      nil
    ;Sino, crea una lista con cada uno de los nodos
    ;obtenidos a partir de la información de cada acción.
    (append (create-node-list-from-action-list (funcall (first op-list) node) node problem) 
            (expand-node-aux node (rest op-list) problem))))
    
;Crea una lista de nodos a partir de una lista de acciones
(defun create-node-list-from-action-list (a-list parent-node problem)
  ;Fin de la lista de acciones: termina.
  (if (null a-list)
      nil
    ;Crea una lista de nodos a partir de la información
    ;de cada acción.
    (cons (let* ((nstate (action-final (first a-list)))
                ;(ng (action-cost (first a-list)))
				(ng (+ (action-cost (first a-list)) (node-g parent-node))) ;Coste desde la raíz hasta el nodo actual.
                (nh (funcall (problem-f-h problem) nstate)))
           (make-node 
            :state nstate
            :parent parent-node
            :action (first a-list)
            :depth (+ 1 (node-depth parent-node))
            :g ng
            :h nh
            :f (+ ng nh)))
          (create-node-list-from-action-list (rest a-list) parent-node problem))))
           
;;;
;;; EJEMPLOS
;;;
(expand-node (make-node :state 'Kentares :depth 0 :g 0 :f 0) *galaxy-M35*)
;;;(#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL AVALON
;;;                           :COST 3)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE KATRIL
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL KATRIL
;;;                           :COST 10)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL PROSERPINA
;;;                           :COST 7)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL PROSERPINA
;;;                           :COST 12)
;;;         :DEPTH 1
;;;         :G ...))

(expand-node (make-node :state 'Proserpina :depth 12 :g 10 :f 20) *galaxy-M35*)
;;;(#S(NODE
;;;    :STATE AVALON
;;;    :PARENT #S(NODE
;;;               :STATE PROSERPINA
;;;               :PARENT NIL
;;;               :ACTION NIL
;;;               :DEPTH 12
;;;               :G 10
;;;               :H 0
;;;               :F 20)
;;;    :ACTION #S(ACTION
;;;               :NAME NAVIGATE-WHITE-HOLE
;;;               :ORIGIN PROSERPINA
;;;               :FINAL AVALON
;;;               :COST 8.6)
;;;    :DEPTH 13
;;;    :G 18.6
;;;    :H 15
;;;    :F 33.6)
;;; #S(NODE
;;;    :STATE DAVION
;;;    :PARENT #S(NODE
;;;               :STATE PROSERPINA
;;;               :PARENT NIL
;;;               :ACTION NIL
;;;               :DEPTH 12
;;;               :G ...)
;;;    :ACTION #S(ACTION
;;;               :NAME NAVIGATE-WHITE-HOLE
;;;               :ORIGIN PROSERPINA
;;;               :FINAL DAVION
;;;               :COST 5)
;;;    :DEPTH 13
;;;    :G 15
;;;    :H 5
;;;    :F 20)
;;; #S(NODE
;;;    :STATE MALLORY
;;;    :PARENT #S(NODE
;;;               :STATE PROSERPINA
;;;               :PARENT NIL
;;;               :ACTION NIL
;;;               :DEPTH 12
;;;               :G ...)
;;;   :ACTION #S(ACTION
;;;              :NAME NAVIGATE-WHITE-HOLE
;;;              :ORIGIN PROSERPINA
;;;              :FINAL MALLORY
;;;              :COST 15)
;;;   :DEPTH 13
;;;   :G 25
;;;   :H 12
;;;   :F 37)
;;; #S(NODE
;;;   :STATE SIRTIS
;;;   :PARENT #S(NODE
;;;              :STATE PROSERPINA
;;;              :PARENT NIL
;;;              :ACTION NIL
;;;              :DEPTH 12
;;;              :G ...)
;;;    :ACTION #S(ACTION
;;;               :NAME NAVIGATE-WHITE-HOLE
;;;               :ORIGIN PROSERPINA
;;;               :FINAL SIRTIS
;;;               :COST 12)
;;;    :DEPTH 13
;;;    :G 22
;;;    :H 0
;;;    :F 22)
;;; #S(NODE
;;;    :STATE KENTARES
;;;    :PARENT #S(NODE
;;;               :STATE PROSERPINA
;;;               :PARENT NIL
;;;               :ACTION NIL
;;;               :DEPTH 12
;;;               :G ...)
;;;    :ACTION #S(ACTION
;;;               :NAME NAVIGATE-WORM-HOLE
;;;               :ORIGIN PROSERPINA
;;;               :FINAL KENTARES
;;;               :COST 12)
;;;    :DEPTH 13
;;;    :G 22
;;;    :H 14
;;;    :F 36)
;;; #S(NODE
;;;    :STATE MALLORY
;;;    :PARENT #S(NODE
;;;               :STATE PROSERPINA
;;;               :PARENT NIL
;;;               :ACTION NIL
;;;               :DEPTH 12
;;;               :G ...)
;;;    :ACTION #S(ACTION
;;;               :NAME NAVIGATE-WORM-HOLE
;;;               :ORIGIN PROSERPINA
;;;               :FINAL MALLORY
;;;               :COST 11)
;;;    :DEPTH 13
;;;    :G 21
;;;    :H 12
;;;    :F 33)
;;; #S(NODE
;;;    :STATE SIRTIS
;;;    :PARENT #S(NODE
;;;               :STATE PROSERPINA
;;;               :PARENT NIL
;;;               :ACTION NIL
;;;               :DEPTH 12
;;;               :G ...)
;;;    :ACTION #S(ACTION
;;;               :NAME NAVIGATE-WORM-HOLE
;;;               :ORIGIN PROSERPINA
;;;               :FINAL SIRTIS
;;;               :COST 9)
;;;    :DEPTH 13
;;;    :G 19
;;;    :H 0
;;;    :F 19))

;;
;; END Exercise 5: Expand node
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 6 -- Node list management
;;; 

;Obtiene una lista de nodos ordenada según el criterio
;de comparación especificado en la estrategia strategy.
(defun insert-nodes-strategy (nodes lst-nodes strategy)
  ;Si la lista de nodos está vacía, termina.
  (if (null nodes)
      lst-nodes
    ;Sino, va añadiendo cada nodo de nodes a la lista ordenada 
    ;lst-nodes mediante sucesivas llamadas a insert-node-strategy.
    (insert-nodes-strategy (rest nodes)
                           (insert-node-strategy (first nodes)
                                                 lst-nodes
                                                 strategy)
                           strategy)))

;Inserta un nodo en la lista ordenada de nodos de acuerdo
;al orden sobre el que se rige lst-nodes (indicado por strategy).
(defun insert-node-strategy (node lst-nodes strategy)
  ;Si la lista de nodos ordenada por g está vacía, termina.
  (if (null lst-nodes)
      (list node)
    ;Si la función de comparación de strategy indica que el
    ;parámetro a comparar de node es menor que el primer
    ;elemento de lst-nodes...
    (if (funcall (strategy-node-compare-p strategy) 
                 node 
                 (first lst-nodes))
	    ;Node pasa a ser el primer elemento de la lista ordenada.
        (append (list node) lst-nodes)
      ;Sino, sigue mirando en qué posición insertar el nodo de acuerdo al orden.
      (append (list (first lst-nodes)) (insert-node-strategy node (rest lst-nodes) strategy)))))
 

;;;
;;; EJEMPLOS
;;;
(defparameter node-00
   (make-node :state 'Proserpina :depth 12 :g 10 :f 20) )
(defparameter node-01
   (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-02
   (make-node :state 'Kentares :depth 2 :g 50 :f 50) )

(defparameter lst-nodes-00 (expand-node node-00 *galaxy-M35*))

;Función de coste uniforme
(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

;Estrategia de coste uniforme
(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))

(print (insert-nodes-strategy (list node-00 node-01 node-02) 
                        lst-nodes-00 
                        *uniform-cost*));->
;;;
;;;(#S(NODE :STATE AVALON 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13 :G 18.6 :H 15	:F 33.6)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)        
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13 :G 15 :H 5 :F 20)
;;; #S(NODE :STATE MALLORY 
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)                
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)      
;;;         :DEPTH 13 :G 25 :H 12 :F 37)     
;;; #S(NODE :STATE SIRTIS    
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)          
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)      
;;;         :DEPTH 13 :G 22 :H 0 :F 22)
;;; #S(NODE :STATE KENTARES   
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0  :F 20)      
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)          
;;;         :DEPTH 13 :G 22 :H 14 :F 36)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)    
;;;         :DEPTH 13 :G 21 :H 12 :F 33)  
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)        
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)       
;;;         :DEPTH 13 :G 19 :H 0 :F 19)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50))


(print 
 (insert-nodes-strategy (list node-00 node-01 node-02) 
                        (sort (copy-list lst-nodes-00) #'<= :key #'node-g) 
                        *uniform-cost*));->
;;;
;;;(#S(NODE :STATE AVALON
;;;         :PARENT NIL
;;;         :ACTION NIL
;;;         :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT NIL
;;;         :ACTION NIL
;;;         :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13 :G 15 :H 5 :F 20)
;;; #S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13 :G 18.6 :H 15 :F 33.6)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13 :G 19 :H 0 :F 19)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13 :G 21 :H 12 :F 33)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13 :G 22 :H 14 :F 36)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13 :G 22 :H 0 :F 22)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13 :G 25 :H 12 :F 37)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT NIL
;;;         :ACTION NIL
;;;         :DEPTH 2 :G 50 :H 0 :F 50))

;;
;;    END: Exercise 6 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;
(defun lower-g+h (node1 node2)
  (<= (node-f node1)
      (node-f node2)))

(defparameter *A-star*
  (make-strategy
   :name 'A-star
   :node-compare-p #'lower-g+h))

;;
;; END: Exercise 7 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 8: Search algorithm
;;;
(defun graph-search (problem strategy)
  (let* ((initial-planet (problem-initial-state problem))	;Nombre del planeta inicial.
		 (ng 0)												;Valor de g del nodo raíz.
		 (nh (funcall (problem-f-h problem) initial-planet));Valor de h del nodo raíz.
		 (root-node (make-node :state initial-planet		;Nodo raíz del problema con el planeta inicial.
							   :parent nil
							   :action nil
							   :depth 0
							   :g ng
							   :h nh
							   :f (+ ng nh)))
		(open-nodes (list root-node))						;Lista abierta con el nodo raíz.
        (closed-nodes nil))									;Lista cerrada vacía.
	(graph-search-rec open-nodes closed-nodes problem strategy)))

(defun graph-search-rec (open-nodes closed-nodes problem strategy)
  (if (null open-nodes)
    nil
    (let ((current-node (first open-nodes)))
	    (if (f-goal-test-galaxy current-node *planets-destination* *planets-mandatory*) ;El nodo a expandir es el objetivo
			;Evaluar a la solución.
			(first (get-solution current-node))
			;Comprueba si el nodo no está en la lista cerrada o,
			;si está en ella, si tiene un valor de g inferior al primer nodo de closed-nodes.
			(if (or (null (member current-node closed-nodes))
					 (< (node-g current-node) (node-g (first closed-nodes))))
				;Expande el nodo actual e inserta los hijos en open-nodes, ordenados de
				;acuerdo al criterio de comparación de strategy.
				;
				;También inserta el nodo actual en la lista cerrada closed-nodes.
				(let ((new-open-nodes (insert-nodes-strategy (expand-node current-node problem) open-nodes strategy))
					  (new-closed-nodes (append (list current-node) closed-nodes)))
					;Continúa la búsqueda eliminando el nodo expandido actual
					;de la lista abierta.
					(graph-search-rec (remove current-node new-open-nodes) new-closed-nodes problem strategy))
				;Si el nodo a expandir no cumple las condiciones, se elimina directamente
				;de la lista abierta.
				(graph-search-rec (remove current-node open-nodes) closed-nodes problem strategy))))))
		
;Obtiene el camino desde el nodo objetivo hasta la raíz.				
(defun get-solution (node)
  (if (null node)
	nil
	(append (list node) (get-solution (node-parent node)))))

;
;  Solve a problem using the A* strategy
;
(defun a-star-search (problem)
  (graph-search problem *A-star*))


(graph-search *galaxy-M35* *A-star*);->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


(print (a-star-search *galaxy-M35*));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


;;; 
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Solution path / action sequence
;;;

;Obtiene una lista de estados (nombres de planetas) desde
;la raíz hasta el nodo objetivo.
(defun solution-path (node)
  (reverse (get-solution-path node)))

;Obtiene una lista de estados (nombres de planetas) desde
;el nodo objetivo hasta la raíz.
(defun get-solution-path (node)
  (if (null node)
	nil
	(cons (node-state node) (get-solution-path (node-parent node)))))

;;;
;;; EJEMPLOS
;;;
(solution-path nil) ;;; -> NIL 
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...)

;Obtiene una lista de acciones desde
;la raíz hasta el nodo objetivo.
(defun action-sequence (node)
  (reverse (get-action-sequence node)))

;Obtiene una lista de acciones desde
;el nodo objetivo hasta la raíz.
(defun get-action-sequence (node)
  (if (null node)
    nil
    (if (null (node-parent node))
	  (node-action node)
	  (cons (node-action node) (get-action-sequence (node-parent node))))))

;;;
;;; EJEMPLOs
;;;
(action-sequence nil) ;;; -> NIL
(action-sequence (a-star-search *galaxy-M35*)) ;;; -> (#S(ACTION :NAME ...)) 

;;; 
;;;    END Exercise 9: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: depth-first / breadth-first
;;;

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))

;Comprueba si la profundidad de node-1 es 
;mayor o igual que la de node-2.
(defun depth-first-node-compare-p (node-1 node-2)
  (>= (node-depth node-1)
      (node-depth node-2)))

;;;
;;; EJEMPLOS
;;;
(solution-path (graph-search *galaxy-M35* *depth-first*))
(action-sequence (graph-search *galaxy-M35* *depth-first*))
;;; -> (MALLORY ... )

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

;Comprueba si la profundidad de node-1 es 
;menor o igual que la de node-2.
(defun breadth-first-node-compare-p (node-1 node-2)
  (<= (node-depth node-1)
      (node-depth node-2)))

;;;
;;; EJEMPLOS
;;;
(solution-path (graph-search *galaxy-M35* *breadth-first*))
(action-sequence (graph-search *galaxy-M35* *breadth-first*))
;; -> (MALLORY ... )

;;; 
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

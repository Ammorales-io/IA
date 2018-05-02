(defpackage :grupo63pareja1021F0305 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1021F0305)

(defun heuristica (estado) ;Preparamos la información del tablero  
	(let ((puntuacion-propia (suma-fila (estado-tablero estado) (estado-lado-sgte-jugador estado)))
		(puntuacion-contrario (suma-fila (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado))))
		(kalaha-propio (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
        (kalaha-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6))
        (hoyo-0 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0))
        (hoyo-1 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 1))
        (hoyo-2 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 2))
        (hoyo-3 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 3))
        (hoyo-4 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4))
        (hoyo-5 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5))
		(hoyo-0-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
        (hoyo-1-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 1))
        (hoyo-2-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 2))
        (hoyo-3-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 3))
        (hoyo-4-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 4))
        (hoyo-5-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 5)))
    (+
     ;Valoramos si se termina el juego
     (if (juego-terminado-p estado)
         (if (< puntuacion-propia puntuacion-contrario)
             -1000000
           1000000)
       ;Si no se termina, valoramos si repetimos turno
       (+ (if (estado-debe-pasar-turno estado)
              (* (maxium 	
                  (list (tufunsionsita hoyo-0 hoyo-5-contrario)
                        (tufunsionsita hoyo-1 hoyo-4-contrario)
                        (tufunsionsita hoyo-2 hoyo-3-contrario)
                        (tufunsionsita hoyo-3 hoyo-4-contrario)
                        (tufunsionsita hoyo-4 hoyo-1-contrario)
                        (tufunsionsita hoyo-5 hoyo-0-contrario)))
                 100)
            0)
          ;Si no se pasa turno, valoramos la diferencia de fichas en Kalahas...
          (* (- kalaha-propio kalaha-contrario) 1000)
          ;Y la cantidad de fichas en cada hoyos (cuantos menos semillas, mejor)
          (-
           0
           (* hoyo-0 hoyo-0 )
           (* hoyo-1 hoyo-1 )
           (* hoyo-2 hoyo-2 5)
           (* hoyo-3 hoyo-3 10)
           (* hoyo-4 hoyo-4 20)
           (* hoyo-5 hoyo-5 30)))))))

(defvar *alias* '|Lyssa-v2|)

(defun tufunsionsita (hoyo-propio hoyo-contrario)
  (if (not (= hoyo-propio 0))
      0
    hoyo-contrario))

(defun maxium (list)
  (reduce #'max list))
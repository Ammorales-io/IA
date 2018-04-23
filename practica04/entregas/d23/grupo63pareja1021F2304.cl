(defpackage :grupo63pareja1021F2304 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1021F2304)

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
        (hoyo-6 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6)))
    (+
     ;Valoramos si se termina el juego
     (if (juego-terminado-p estado)
         (if (< puntuacion-propia puntuacion-contrario)
             -10000
           10000)
       ;Si no se termina, valoramos si repetimos turno
       (if (estado-debe-pasar-turno estado)
           9000 
         ;Si no se pasa turno, valoramos la diferencia de fichas en Kathalas...
         (+ (* (- kalaha-propio kalaha-contrario) 200)
         ;Y la cantidad de fichas en cada hoyos (cuantos menos semillas, mejor)
         (-
          0
          (* hoyo-0 hoyo-0 )
          (* hoyo-1 hoyo-1 )
          (* hoyo-2 hoyo-2 )
          (* hoyo-3 hoyo-3 12)
          (* hoyo-4 hoyo-4 25)
          (* hoyo-5 hoyo-5 35))))))))


(defvar *alias* '|Petacabras_v1.4e|)
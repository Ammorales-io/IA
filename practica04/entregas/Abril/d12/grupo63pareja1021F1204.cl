(defpackage :grupo63pareja1021F1204 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1021F1204)

(defun heuristica (estado) ;Preparamos la información del tablero
  (let ((kalaha-propio (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
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
         (if (< kalaha-propio kalaha-contrario)
             -20000
           20000)
       ;Si no se termina, valoramos si repetimos turno
       (if (estado-debe-pasar-turno estado)
           (+(* (- kalaha-propio kalaha-contrario) 400) 15000) 
         ;Si no se pasa turno, valoramos la diferencia de fichas en Kathalas...
         (+(* (- kalaha-propio kalaha-contrario) 400)
         ;Y la cantidad de fichas en cada hoyos (cuantos menos semillas, mejor)
         (-
          0
          (* hoyo-0 hoyo-0 hoyo-0)
          (* hoyo-1 hoyo-1 hoyo-1)
          (* hoyo-2 hoyo-2 hoyo-2)
          (* hoyo-3 hoyo-3 hoyo-3)
          (* hoyo-4 hoyo-4 hoyo-4)
          (* hoyo-5 hoyo-5 hoyo-5)
          (* hoyo-6 hoyo-6 hoyo-6))))))))



(defvar *alias* '|Debuguear_mediante_printfs|)

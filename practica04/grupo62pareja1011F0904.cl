(defpackage :grupo62pareja1011F0904 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo62pareja1011F0904)

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
         (if (< kahala-propio kahala-contrario)
             -10000
           10000)
       0)
     ;Valoramos la diferencia de fichas en Kathalas
     (* (- kahala-propio kahala-contrario) 10)
     ;Valoramos los hoyos (cuantos menos semillas, mejor)
     (- (* hoyo-0 hoyo-0))
     (- (* hoyo-1 hoyo-1))
     (- (* hoyo-2 hoyo-2))
     (- (* hoyo-3 hoyo-3))
     (- (* hoyo-4 hoyo-4))
     (- (* hoyo-5 hoyo-5))
     (- (* hoyo-6 hoyo-6))
     ;Por último, valoramos si repetimos turno
     (if (estado-debe-pasar-turno estado)
         1000
       -1000))))
)

(defvar *alias* 'PetaCabras)

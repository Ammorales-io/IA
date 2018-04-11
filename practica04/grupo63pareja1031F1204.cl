(defpackage :grupo63pareja1031F1204 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1031F1204)

(defun heuristica (estado) ;Preparamos la información del tablero
  (let ((kalaha-propio (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
        (kalaha-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6)))
    (if (juego-terminado-p estado)
        (if (< kalaha-propio kalaha-contrario)
            -1
          1002)
      (if (estado-debe-pasar-turno estado)
          1001
        (random 1000)))))


(defvar *alias* '|Debuguear_con_printfs|)

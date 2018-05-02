(defpackage :grupo63pareja1012F1004 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1012F1004)

(defun heuristica (estado) ;Preparamos la información del tablero
  (let ((kalaha-propio (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
        (kalaha-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6)))
    (- kalaha-propio kalaha-contrario)))



(defvar *alias* 'Montoro)

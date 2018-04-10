(defpackage :grupo63pareja1012F1004 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la funci�n de evaluaci�n
  (:export :heuristica :*alias*)) 	; heur�stica y un alias para el torneo

(in-package grupo63pareja1012F1004)

(defun heuristica (estado) ;Preparamos la informaci�n del tablero
  (let ((kalaha-propio (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
        (kalaha-contrario (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6)))
    (- kalaha-propio kalaha-contrario)))



(defvar *alias* 'Montoro)

(defpackage :grupo63pareja1011F2304	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1011F2304)

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
    
    ; Penaliza si el jugador pierde o empata con el contrario
    (if (juego-terminado-p estado)
        (if (<= puntuacion-propia puntuacion-contrario)
            -10000
          10000)
      ;Si no se termina, valoramos si repetimos turno
      (if (estado-debe-pasar-turno estado)
          -3000
        (+ (* (- kalaha-propio kalaha-contrario) 600)
           ; Pondera los hoyos en función de lo que tiene en el propio y en el contrario
           (+ 0
              (- (* hoyo-0 (/ 120 (+ hoyo-5-contrario 1))))
              (- (* hoyo-1 (/ 120 (+ hoyo-4-contrario 1))))
              (- (* hoyo-2 (/ 120 (+ hoyo-3-contrario 1))))
              (- (* hoyo-3 (/ 120 (+ hoyo-2-contrario 1))))
              (- (* hoyo-4 (/ 120 (+ hoyo-1-contrario 1))))
              (- (* hoyo-5 (/ 120 (+ hoyo-0-contrario 1))))))))))

(defvar *alias* '|Jetstream-Sam|)
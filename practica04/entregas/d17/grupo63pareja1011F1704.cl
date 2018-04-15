(defpackage :grupo63pareja1031F1604 	; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) 		; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) 	; heurística y un alias para el torneo

(in-package grupo63pareja1031F1604)

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
        (hoyo-5 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5)))
    
    ; Termina el juego y el jugador pierde o empata
    (if (juego-terminado-p estado)
        (if (<= puntuacion-propia puntuacion-contrario)
            -10000
          10000)
      ; Valora si puede jugar otra vez
      (+ (if (estado-debe-pasar-turno estado)
             5000
           ; Sino, valora las fichas en los kalahas
           (+ (* (- kalaha-propio kalaha-contrario) 60)
              ;Comprueba las fichas del contrario
              (* (- puntuacion-propia puntuacion-contrario) 100)
              (-
               (* (- 0 hoyo-0) 1000)
               (* (- 0 hoyo-1) 800)
               (* (- 0 hoyo-2) 500)
               (* (- 0 hoyo-3) 100)
               (* (- 0 hoyo-4) 50)
               (* (- 0 hoyo-5) 30))))))))

(defvar *alias* '|714|)
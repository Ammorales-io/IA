;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '~)

(defun truth-value-p (x) 
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x) 
  (eql x +not+))

(defun binary-connector-p (x) 
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x) 
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x) 
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo 
;;
;; RECIBE   : expresion 
;; EVALUA A : T si la expresion es un literal positivo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun positive-literal-p (x)
  ;Comprueba si x es un atomo y no se ha declarado
  ;como conector, o como NIL o T directamente
  (and (atom x) (not (equal x nil)) (not (equal x t)) (not (connector-p x))))

;; EJEMPLOS:
(positive-literal-p 'p)
;; evalua a T
(positive-literal-p T)
(positive-literal-p NIL)
(positive-literal-p '~)
(positive-literal-p '=>)
(positive-literal-p '(p))
(positive-literal-p '(~ p))
(positive-literal-p '(~ (v p q)))
;; evaluan a NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; Predicado para determinar si una expresion
;; es un literal negativo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si la expresion es un literal negativo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negative-literal-p (x)
  ;Comprueba si x es una lista de 2 elementos, 
  ;siendo el primero el conector NOT 
  ;y el segundo un literal positivo
  (and (listp x) (unary-connector-p (first x)) (positive-literal-p (second x))))

;; EJEMPLOS:
(negative-literal-p '(~ p))        ; T
(negative-literal-p NIL)           ; NIL
(negative-literal-p '~)            ; NIL
(negative-literal-p '=>)           ; NIL
(negative-literal-p '(p))          ; NIL
(negative-literal-p '((~ p)))      ; NIL
(negative-literal-p '(~ T))        ; NIL
(negative-literal-p '(~ NIL))      ; NIL
(negative-literal-p '(~ =>))       ; NIL
(negative-literal-p 'p)            ; NIL
(negative-literal-p '((~ p)))      ; NIL
(negative-literal-p '(~ (v p q)))  ; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x) 
  ;Comprueba si x es un literal positivo o negativo
  (or (positive-literal-p x) (negative-literal-p x)))

;; EJEMPLOS:
(literal-p 'p)             
(literal-p '(~ p))      
;;; evaluan a T
(literal-p '(p))
(literal-p '(~ (v p q)))
;;; evaluan a  NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
        (and (listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((connector (first x))
                   (rest_1    (rest  x)))
               (cond
                ((unary-connector-p connector)  ;; Si el primer elemento es un connector unario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
                      (wff-prefix-p (first rest_1)))) 
                ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
                   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
                        (wff-prefix-p (first rest_1))
                        (wff-prefix-p (first rest_2)))))               
                ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
                 (or (null rest_1)              ;; conjuncion o disyuncion vacias
                     (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos 
                          (let ((rest_2 (rest rest_1)))
                            (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
                                (wff-prefix-p (cons connector rest_2)))))))	
                (t NIL)))))))                   ;; No es FBF en formato prefijo 
;;
;; EJEMPLOS:
(wff-prefix-p '(v))
(wff-prefix-p '(^))
(wff-prefix-p '(v A))
(wff-prefix-p '(v A B C))
(wff-prefix-p '(^ (~ B)))
(wff-prefix-p '(v A (~ B)))
(wff-prefix-p '(v (~ B) A ))
(wff-prefix-p '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;;; evaluan a T
(wff-prefix-p 'NIL)
(wff-prefix-p '(~))
(wff-prefix-p '(=>))
(wff-prefix-p '(<=>))
(wff-prefix-p '(^ (V P (=> A ( B ^ (~ C) ^ D))) (^ (<=> P (~ Q)) P) E))
;;; evaluan a NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; Predicado para determinar si una expresion esta en formato prefijo,
;; bien (~ FBF) o (FBF1 <conector> FBF2)
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-p (x)
  (unless (null x)	;NIL no es FBF en formato infijo (por convencion)
    (or (literal-p x)	;Un literal es FBF en formato infijo
        (and (listp x)	;En caso de que no sea un literal debe ser una lista
             (cond
              ;Si el primer elemento es un connector unario ~
              ;debería tener la estructura (<conector> FBF)
              ((unary-connector-p (first x))
               (and (null (rest (rest x)))	;Después de FBF no hay "nada" (NIL)
                    (wff-infix-p (second x))))
              ;Si el elemento es un conector binario =>, <=>
              ;deberia tener la estructura (FBF1 <conector> FBF2)
              ((binary-connector-p (second x))
               (and (null (fourth x))		;Después de FBF2 no hay "nada" (NIL)
                    (wff-infix-p (first x))
                    (wff-infix-p (third x))))
              ;Si el elemento es una conjuncion o disyuncion vacía,
              ;(^) (v), no tienen ninguna FBF detrás (NIL)
              ((n-ary-connector-p (first x))
               (null (rest x)))
              ;Si el elemento es un conector enario ^ ó v, debería
              ;tener la estructura (FBF1 <conector> FBF2 <conector> ... FBFn)
              ((n-ary-connector-p (second x))
               ;Si llegamos a una estructura de tipo (FBFn-1 <conector> FBFn)
               (if (null (fourth x))
                   (and (wff-infix-p (first x))	;Comprueba si FBF1 es válida
                        (wff-infix-p (third x)))	;Comprueba si FBF2 es válida
                 ;En caso de tener una estructura de tipo (FBFi <conector> FBFi+1 <conector> ... FBFn)
                 (and (eql (second x) (fourth x))	;Comprueba si en la estructura no hay dos conectores distintos como: (A ^ B v C)
                      (wff-infix-p (first x))		;Comprueba si FBF1 es válida
                      (wff-infix-p (rest (rest x))))))	;Comprueba si (FBFi+1 <conector> ... FBFn) es válida
              ;No es FBF en formato infijo
              (t NIL))))))

;;
;; EJEMPLOS:
;;
(wff-infix-p 'a) 					; T
(wff-infix-p '(^)) 					; T  ;; por convencion
(wff-infix-p '(v)) 					; T  ;; por convencion
(wff-infix-p '(A ^ (v))) 			      	; T  
(wff-infix-p '( a ^ b ^ (p v q) ^ (~ r) ^ s))  		; T 
(wff-infix-p '(A => B)) 				; T
(wff-infix-p '(A => (B <=> C))) 			; T
(wff-infix-p '( B => (A ^ C ^ D))) 			; T   
(wff-infix-p '( B => (A ^ C))) 				; T 
(wff-infix-p '( B ^ (A ^ C))) 				; T 
(wff-infix-p '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p ) ^ e))  ; T 
(wff-infix-p nil) 					; NIL
(wff-infix-p '(a ^)) 					; NIL
(wff-infix-p '(^ a)) 					; NIL
(wff-infix-p '(a)) 					; NIL
(wff-infix-p '((a))) 				      	; NIL
(wff-infix-p '((a) b))   			      	; NIL
(wff-infix-p '(^ a b q (~ r) s))  		      	; NIL 
(wff-infix-p '( B => A C)) 			      	; NIL   
(wff-infix-p '( => A)) 				      	; NIL   
(wff-infix-p '(A =>)) 				      	; NIL   
(wff-infix-p '(A => B <=> C)) 		      		; NIL
(wff-infix-p '( B => (A ^ C v D))) 		      	; NIL  
(wff-infix-p '( B ^ C v D )) 			      	; NIL 
(wff-infix-p '((p v (a => e (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p ) ^ e)); NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF en formato infijo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
        wff
      (let ((connector      (first wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p connector) 
          (list connector (prefix-to-infix (second wff))))
         ((binary-connector-p connector) 
          (list (prefix-to-infix (second wff))
                connector
                (prefix-to-infix (third wff))))
         ((n-ary-connector-p connector) 
          (cond 
           ((null elements-wff)        ;;; conjuncion o disyuncion vacias. 
            wff)                       ;;; por convencion, se acepta como fbf en formato infijo
           ((null (cdr elements-wff))  ;;; conjuncion o disyuncion con un unico elemento
            (prefix-to-infix (car elements-wff)))  
           (t (cons (prefix-to-infix (first elements-wff)) 
                    (mapcan #'(lambda(x) (list connector (prefix-to-infix x))) 
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca

;;
;;  EJEMPLOS:
;;
(prefix-to-infix '(v))          ; (V)
(prefix-to-infix '(^))          ; (^)
(prefix-to-infix '(v a))        ; A
(prefix-to-infix '(^ a))        ; A
(prefix-to-infix '(^ (~ a)))    ; (~ a)
(prefix-to-infix '(v a b))      ; (A v B)
(prefix-to-infix '(v a b c))    ; (A V B V C)
(prefix-to-infix '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;;; ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)
(prefix-to-infix '(^ (v p (=> a (^ b (~ c) d))))) ; (P V (A => (B ^ (~ C) ^ D)))
(prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))     ; (((P <=> (~ Q)) ^ P) ^ E)  
(prefix-to-infix '( v (~ p) q (~ r) (~ s)))       ; ((~ P) V Q V (~ R) V (~ S))
(prefix-to-infix '(^ a b (~ b) (=> a c) (~ d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)		;Mientras wff sea una FBF
    (if (literal-p wff)
        wff					;Caso base: wff es un literal
      (cond   
       ;Si el primer elemento es un connector unario ~
       ;debería tener la estructura (<conector> FBF)
       ((unary-connector-p (first wff))
        (list (first wff) (infix-to-prefix (second wff))))
       ;Si el elemento es un conector binario =>, <=>
       ;deberia tener la estructura (FBF1 <conector> FBF2)
       ((binary-connector-p (second wff))
        (list (second wff)
              (infix-to-prefix (first wff))
              (infix-to-prefix (third wff))))
       ; Si el elemento es una conjuncion o disyuncion 
       ; vacía, se evalúa direcamente.
       ((n-ary-connector-p (first wff))
        (when (null (rest wff))
          wff))
       ;Si el elemento es un conector enario ^ v, debería
       ;tener la estructura (FBF1 <conector> FBF2 <conector> ... FBFn)
       ((n-ary-connector-p (second wff))
        ;Crea una lista de tipo (<conector> FBF1 FBF2 ... FBFn)
        ;sobre una copia de wff en la que todos los conectores
        ;se han eliminado.
        ;Por tanto, mapcar evalua cada FBF de la lista y 
        ;devuelve su forma prefijo.
        (append (list (second wff))
                (mapcar #'infix-to-prefix (remove (second wff) wff))))
       (t NIL)))))

;;
;; EJEMPLOS
;;
(infix-to-prefix nil)      ;; NIL
(infix-to-prefix 'a)       ;; a
(infix-to-prefix '((a)))   ;; NIL
(infix-to-prefix '(a))     ;; NIL
(infix-to-prefix '(((a)))) ;; NIL
(prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p) ^ e)) ) 
;;-> ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)


(infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^  ((p <=> (~ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)

(infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; (~ (V (~ P) Q (~ R) (~ S)))


(infix-to-prefix
 (prefix-to-infix
  '(V (~ P) Q (~ R) (~ S))))
;;-> (V (~ P) Q (~ R) (~ S))

(infix-to-prefix
 (prefix-to-infix
  '(~ (V (~ P) Q (~ R) (~ S)))))
;;-> (~ (V (~ P) Q (~ R) (~ S)))


(infix-to-prefix 'a)  ; A
(infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^  ((p <=> (~ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)

(infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; (~ (V (~ P) Q (~ R) (~ S)))

(infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (~ c) d)))))) ; '(v p (=> a (^ b (~ c) d))))
(infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))) ; '(^ (^ (<=> p (~ q)) p ) e))  
(infix-to-prefix (prefix-to-infix '( v (~ p) q (~ r) (~ s))))  ; '( v (~ p) q (~ r) (~ s)))
;;;

(infix-to-prefix '(p v (a => (b ^ (~ c) ^ d)))) ; (V P (=> A (^ B (~ C) D)))
(infix-to-prefix '(((P <=> (~ Q)) ^ P) ^ E))  ; (^ (^ (<=> P (~ Q)) P) E)
(infix-to-prefix '((~ P) V Q V (~ R) V (~ S))); (V (~ P) Q (~ R) (~ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-p (wff)
  (when (listp wff)
    (let ((conector (first wff))
          (elems (rest wff)))
      (if (eql +or+ conector)
          (or (null elems)			;Disyunción vacía (v)
              (and (null (rest elems))		;Disyunción con un elemento (v lit)
                   (literal-p (second wff)))
              (and (literal-p (second wff))	;Disyunción con más de un elemento
                   (clause-p (cons conector (rest elems)))))))))

;;
;; EJEMPLOS:
;;
(clause-p '(v))             ; T
(clause-p '(v p))           ; T
(clause-p '(v (~ r)))       ; T
(clause-p '(v p q (~ r) s)) ; T
(clause-p NIL)                    ; NIL
(clause-p 'p)                     ; NIL
(clause-p '(~ p))                 ; NIL
(clause-p NIL)                    ; NIL
(clause-p '(p))                   ; NIL
(clause-p '((~ p)))               ; NIL
(clause-p '(^ a b q (~ r) s))     ; NIL
(clause-p '(v (^ a b) q (~ r) s)) ; NIL
(clause-p '(~ (v p q)))           ; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-p (wff)
  (when (listp wff)
    (let ((conector (first wff))
          (elems (rest wff)))
      (if (eql +and+ conector)
          (or (null elems)			;Conjunción vacía (^)
              (and (null (rest elems))		;Conjunción de tipo (^ (v)) ó (^ (v clause))
                   (clause-p (second wff)))
              (and (clause-p (second wff))	;Conjunción con más de una cláusula
                   (cnf-p (cons conector (rest elems)))))))))

;;
;; EJEMPLOS:
;;
(cnf-p '(^ (v a  b c) (v q r) (v (~ r) s) (v a b))) ; T
(cnf-p '(^ (v a  b (~ c)) ))                        ; T
(cnf-p '(^ ))                                       ; T
(cnf-p '(^(v )))                                    ; T
(cnf-p '(~ p))                                      ; NIL
(cnf-p '(^ a b q (~ r) s))                          ; NIL
(cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
(cnf-p '(v p q (~ r) s))                            ; NIL
(cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
(cnf-p '(^ p))                                      ; NIL
(cnf-p '(v ))                                       ; NIL
(cnf-p NIL)                                         ; NIL
(cnf-p '((~ p)))                                    ; NIL
(cnf-p '(p))                                        ; NIL
(cnf-p '(^ (p)))                                    ; NIL
(cnf-p '((p)))                                      ; NIL
(cnf-p '(^ a b q (r) s))                            ; NIL
(cnf-p '(^ (v a  (v b c)) (v q r) (v (~ r) s) a b)) ; NIL
(cnf-p '(^ (v a (^ b c)) (^ q r) (v (~ r) s) a b))  ; NIL
(cnf-p '(~ (v p q)))                                ; NIL
(cnf-p '(v p q (r) s))                              ; NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;;
;; Dada una FBF, evalua a una FBF equivalente 
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF equivalente en formato prefijo 
;;            sin connector <=>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-biconditional (wff)
  ;Si la FBF es NIL o un literal, devuelve su valor.
  (if (or (null wff) (literal-p wff))
      wff
	;Sino, tenemos una estructura de tipo (<conector> FBFs).
    (let ((connector (first wff)))
      (if (eq connector +bicond+)
		  ;Si el conector es <=>, extrae FBF1 y FBF2 y crea
		  ;una lista de tipo (^ (=> FBF1 FBF2) (=> FBF2 FBF1)).
          (let ((wff1 (eliminate-biconditional (second wff)))
                (wff2 (eliminate-biconditional (third  wff))))
            (list +and+ 
                  (list +cond+ wff1 wff2)
                  (list +cond+ wff2 wff1)))
		  ;En caso contrario, devuelve una lista de tipo
		  ;(<conector> <FBFs sin <=>>)
          (cons connector 
              (mapcar #'eliminate-biconditional (rest wff)))))))		

;;
;; EJEMPLOS:
;;
(eliminate-biconditional '(<=> p  (v q s p) ))
;;   (^ (=> P (v Q S P)) (=> (v Q S P) P))
(eliminate-biconditional '(<=>  (<=> p  q) (^ s (~ q))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (~ Q)))
;;      (=> (^ S (~ Q)) (^ (=> P Q) (=> Q P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)  
  ;Si la FBF es NIL o un literal, devuelve su valor.
  (if (or (null wff) (literal-p wff))
	  wff
	;Sino, tenemos una estructura de tipo (<conector> FBFs).
	(let ((connector (first wff)))
	  (if (eql connector +cond+)
		  ;Si el conector es =>, extrae FBF1 y FBF2 y crea
		  ;una lista de tipo (v (~ FBF1) FBF2).
	      (let ((wff1 (eliminate-conditional (second wff)))
				(wff2 (eliminate-conditional (third wff))))
			(list +or+
				  (list +not+ wff1)
				  wff2))
		  ;En caso contrario, devuelve una lista de tipo
		  ;(<conector> <FBFs sin =>>).
		  (cons connector
			  (mapcar #'eliminate-conditional (rest wff)))))))

;;
;; EJEMPLOS:
;;
(eliminate-conditional '(=> p q))                      	;;; (V (~ P) Q)
(eliminate-conditional '(=> p (v q s p)))        	;;; (V (~ P) (V Q S P))
(eliminate-conditional '(=> (=> (~ p) q) (^ s (~ q)))) 	;;; (V (~ (V (~ (~ P)) Q)) (^ S (~ Q)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; exchange-and-or
;;
;; Función auxiliar que sustituye el conector +and+ (^)
;; por not (v) y viceversa.
;;
;; RECIBE   : Conector lógico 
;; EVALUA A : Conector or (si recibió and), conector and
;; (si recibió or), o cualquier otro conector pasado como
;; parámetro.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exchange-and-or (connector)
  (cond
   ((eq connector +and+) +or+)    
   ((eq connector +or+) +and+)
   (t connector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; Dada una FBF, que no contiene los conectores <=>, => 
;; evalua a una FNC equivalente en la que la negacion  
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, => 
;; EVALUA A : FBF equivalente en formato prefijo en la que 
;;            la negacion  aparece unicamente en literales 
;;            negativos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduce-scope-of-negation (wff)
  ;Si la FBF es NIL o un literal, devuelve su valor.
  (if (or (null wff) (literal-p wff))
      wff
    ;Sino, tenemos una estructura de tipo (<conector> FBFs).
    (let ((connector (first wff))
          (elems (second wff)))
      (if (eql connector +not+)
          ;Si recibe una estructura de tipo (~ (<conector> FBFs)):
          (let ((connector_2 (first (second wff))))
            (cond
             ;CASO 1: FBF de tipo (~ (<conector n-ario> FBF1 FBF2 ... FBFn)).
             ;Primero añade un conector ~ a cada FBF en (FBF1 FBF2 ... FBFn)
             ;y después crea una lista de la forma (<conector n-ario cambiado> <FBFs reducidas en ~>)
             ((n-ary-connector-p connector_2)
              (let ((negated_wffs (mapcar #'(lambda(x) (list +not+ x)) (rest elems)))) ;Niega todas las FBFs.
                (cons (exchange-and-or connector_2)
                      (mapcar #'reduce-scope-of-negation negated_wffs))))	;Por cada FBF negada, reduce el ámbito del conector ~.
             ;CASO 2: FBF de tipo (~ (~ FBF))
             ;Evalúa y reduce el ámbito de ~ en FBF.
             ((eql connector_2 +not+)
              (reduce-scope-of-negation (second elems)))
             (t NIL)))
        ;Si la estructura es de tipo (<conector n-ario> FBF1 FBF2 ... FBFn),
        ;evalúa cada una de las FBFs en caso de que se necesite reducir
        ;el ámbito de ~.
        (cons connector
              (mapcar #'reduce-scope-of-negation (rest wff)))))))  

;;
;;  EJEMPLOS:
;;
(reduce-scope-of-negation '(~ (v p (~ q) r))) 
;;; (^ (~ P) Q (~ R))
(reduce-scope-of-negation '(~ (^ p (~ q) (v  r s (~ a))))) 
;;;  (V (~ P) Q (^ (~ R) (~ S) A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto 
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la 
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v  
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>, 
;;            en la que la negacion aparece unicamente 
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC 
;;            con conectores ^, v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función que crea listas de listas, siendo
;; cada una de tipo (elt <elemento-lst>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst)
  ;Si la lista es NIL, crea una lista
  ;de tipo ((elem))
  (if (null lst)
      (list (list elt))
	;Si no es NIL, combina elt con cada elemento
	;lst de esta forma: 
	;(elt elem-lst-1) (elt-elem-lst-2)... (elt elem-lst-n)
    (mapcar #'(lambda (x) (cons elt x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función que añade conectores a una FBF
;; con elementos combinados entre sí
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exchange-NF (nf)
  ;Si la FBF es NIL o un literal, devuelve su valor.
  (if (or (null nf) (literal-p nf)) 
      nf
	;Sino, construye una lista con el conector n-ario 
	;cambiado y las sublistas (<conector n-ario normal> FBF);
	;todo ello utilizando la FBF combinada.
    (let ((connector (first nf)))
      (cons (exchange-and-or connector)
            (mapcar #'(lambda (x)
                          (cons connector x))
                (exchange-NF-aux (rest nf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función que combina los elementos de una FBF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exchange-NF-aux (nf)
  ;Si recibe NIL como argumento, devuelve NIL.
  (if (null nf) 
      NIL
	;Sino, obtiene el primer elemento de la FBF.
	;
	;Si el primer elemento es un literal, construirá
	;una lista combinándolo con el resto de la FBF.
	;Si no es un literal, combinará el resto de la FBF
	;con sus propios elementos.
    (let ((lst (first nf)))
      (mapcan #'(lambda (x) 
                  (combine-elt-lst 
                   x 
                   (exchange-NF-aux (rest nf)))) 
        (if (literal-p lst) (list lst) (rest lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función que simplifica una FBF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify (connector lst-wffs )
  ;Si la FBF pasada como argumento es un literal,
  ;devuelve su valor
  (if (literal-p lst-wffs)
      lst-wffs
	;Sino, crea una lista simplificada sobre lst-wffs
	;dependiendo de ciertos criterios.
    (mapcan #'(lambda (x) 
                (cond
				 ;Si el elemento es un literal, lo añade
				 ;a una lista.
                 ((literal-p x) (list x))
				 ;Si el primer elemento de la FBF coincide
				 ;con el conector pasado como argumento,
				 ;simplifica el resto de la lista.
                 ((equal connector (first x))
                  (mapcan 
                      #'(lambda (y) (simplify connector (list y))) 
                    (rest x)))
				 ;Si el elemento no es un literal (con lo que 
				 ;sería un conector) y no coincide con el conector
				 ;pasado como argumento, mete el elemento en una lista.
                 (t (list x))))               
      lst-wffs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función que traduce una FBF a FNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf (wff)
  (cond
   ;Si wff es una FNC, devuelve su contenido.
   ((cnf-p wff) wff)
   ;Si wff es un literal, crea una lista
   ;de tipo (^ (v lit)) (FNC con 1 elemento).
   ((literal-p wff)
    (list +and+ (list +or+ wff)))
   ((let ((connector (first wff)))	;Evalúa el conector de wff.
      (cond
	   ;CASO 1: Conector ^
	   ;Simplifica y evalúa el resto de la FBF.
       ((equal +and+ connector) 
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))
	   ;CASO 2: Conector v
	   ;<Intercambia> la FBF simplificada previamente.
       ((equal +or+ connector) 
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff)))))))))))


(cnf 'a)

(cnf '(v (~ a) b c))
(cnf '(v (~ a) c (^ m n)))
(print (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (~ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s ))
(cnf '(^ (v a b (^ y r s) (v k l)) c (~ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (v a b (^ y r s)) c (~ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (^ y r s (^ p q (v c d))) (v a b)))
(print (cnf '(^ (v (~ a) b c) (~ e) r s 
                (v e f (~ g) h) k (v m n) d)))
;;
(cnf '(^ (v p (~ q)) (v k r (^ m  n))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))))
(print (cnf '(^ (^ (~ y) (v r (^ s (~ x)) (^ (~ p) m (v c d))) (v (~ a) (~ b))) g)))
;;
;; EJEMPLOS:
;;
(cnf NIL)              ; NIL
(cnf 'a)               ; (^ (V A))
(cnf '(~ a))           ; (^ (V (~ A)))
(cnf '(V (~ P) (~ P))) ; (^ (V (~ P) (~ P)))
(cnf '(V A))           ; (^ (V A))
(cnf '(^ (v p (~ q)) (v k r (^ m  n))))
;;;   (^ (V P (~ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (~ B) D)  (V P Q E F R N (~ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (~ B) D)  (V P Q E F M N (~ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
 (cnf '(^ (^ (~ y) (v r (^ s (~ x)) 
                      (^ (~ p) m (v c d)))(v (~ a) (~ b))) g)))
;;;(^ (V (~ Y)) (V R S (~ P)) (V R S M) 
;;;   (V R S C D) (V R (~ X) (~ P)) 
;;;   (V R (~ X) M) (V R (~ X) C D)
;;;   (V (~ A) (~ B)) (V G))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-connectors (cnf)
  ;Si cnf es un literal, devuelve su valor.
  (if (literal-p cnf)
      cnf
    ;Sino, comprueba si el primer elemento es el
    ;conector ^ ó v.
    (let ((connector (first cnf)))
      (when (n-ary-connector-p connector)
        ;Siempre que se cumpla la condición, evaluará
        ;cada elemento de cnf y devolverá una lista de
        ;listas sin conectores.
        (mapcar #'eliminate-connectors (rest cnf))))))
		  

(eliminate-connectors 'nil)
;; NIL
(eliminate-connectors (cnf '(^ (v p  (~ q))  (v k  r  (^ m  n)))))
;; ((P (~ Q)) (K R M) (K R N))
(eliminate-connectors
 (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
;; (((~ A) B C) ((~ E)) (E) (F) ((~ G)) (H) (M N) (R) (S) (Q) (U Q) (X) (Y))
(eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
;; ((P Q R N S) (P Q R Q S) (P Q M N S) (P Q M Q S))
(eliminate-connectors (print (cnf '(^ (v p  (~ q)) (~ a) (v k  r  (^ m  n))))))
;; (^ (V P (~ Q)) (V (~ A)) (V K R M) (V K R N)) --print de cnf
;; ((P (~ Q)) ((~ A)) (K R M) (K R N))

(eliminate-connectors '(^))
;; NIL
(eliminate-connectors '(v))
;; NIL
(eliminate-connectors '(^ (v p (~ q)) (v) (v k r)))
;; ((P (~ Q)) NIL (K R))
(eliminate-connectors '(^ (v a b)))
;; ((A B))

;;
;;   EJEMPLOS:
;;
(eliminate-connectors '(^ (v p (~ q)) (v k r)))
;; ((P (~ Q)) (K R))
(eliminate-connectors '(^ (v p (~ q)) (v q (~ a)) (v s e f) (v b)))
;; ((P (~ Q)) (Q (~ A)) (S E F) (B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Función que devuelve la FBF
;; convertida a formato prefijo
(defun transformar-prefijo (wff)
  (infix-to-prefix wff))

;; Función que devuelve la FBF en 
;; formato prefijo sin conectores <=>
;; (Paso 1)
(defun eliminar-bicond (wff)
  (let ((wff-paso-1 (transformar-prefijo wff)))
    (eliminate-biconditional wff-paso-1)))

;; Función que devuelve la FBF en 
;; formato prefijo sin conectores =>
;; (Paso 2)
(defun eliminar-cond (wff)
  (let ((wff-paso-2 (eliminar-bicond wff)))
    (eliminate-conditional wff-paso-2)))

;; Función que devuelve la FBF en 
;; formato prefijo con el ámbito 
;; de negación reducido
;; (Paso 3)
(defun reducir-negacion (wff)
  (let ((wff-paso-3 (eliminar-cond wff)))
    (reduce-scope-of-negation wff-paso-3)))

;; Función que devuelve la FBF en
;; formato prefijo convertida a FNC
;; (Paso 4)
(defun get-cnf (wff)
  (let ((wff-paso-4 (reducir-negacion wff)))
    (cnf wff-paso-4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF 
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-to-cnf (wff)
  ;Siempre que wff sea FNC, obtendrá
  ;su forma FBF sin conectores.
  (when (wff-infix-p wff)
    (let ((cnf (get-cnf wff)))
      (eliminate-connectors cnf))))

;;
;; EJEMPLOS:
;; 
(wff-infix-to-cnf 'a)
;; ((A))
(wff-infix-to-cnf '(~ a))
;; (((~ A)))
(wff-infix-to-cnf  '( (~ p) v q v (~ r) v (~ s)))
;; (((~ P) Q (~ R) (~ S)))
(wff-infix-to-cnf  '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p) ^ e))
;; ((P (~ A) B) (P (~ A) (~ C)) (P (~ A) D) ((~ P) (~ Q)) (Q P) (P) (E))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-repeated-literals (k)
  (let ((lit (first k))
        (elems (rest k)))
    (cond
     ;CASO1: Hemos llegado al final.
     ((null elems) k)
     ;CASO 2: Un elemento tiene sólo una ocurrencia. 
     ;Para comparar, utilizamos su representación textual (equal).
     ((= (count lit k :test #'equal) 1)
      (cons lit (eliminate-repeated-literals (rest k))))
     ;CASO 3: Un elemento tiene más de una ocurrencia.
     (t (eliminate-repeated-literals (rest (member lit k :test #'equal)))))))

;;
;; EJEMPLO:
;;
(eliminate-repeated-literals '(a b (~ c) (~ a) a c (~ c) c a))
;;;   (B (~ A) (~ C) C A)
(eliminate-repeated-literals '(m (~ n) a b (~ a) (~ a) n m (~ m) (~ a) a))
;;;	  ((~ N) B N M (~ M) (~ A) A)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-if-equal (cls target-cls)
;;;
;;; Comprueba si una cláusula es igual que otra que está en 
;;; la FNC, se representen de manera idéntica o diferente. 
;;; Por ejemplo, '(a b c) = '(b a c)
;;;
;;; INPUT: cls: cláusula 1 de la comprobación.
;;; 	   target-cls: cláusula 2 de la comprobación.
;;; OUTPUT: T si son iguales, NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-if-equal (cls target-cls)
  ;O las cláusulas se representan textualmente 
  ;de igual forma (por ejemplo, '(a b c) '(a b c))
  (or (equal cls target-cls)
	  ;O, pese a representarse de forma distinta, 
	  ;ambas contienen los mismos elementos
	  ;(por ejemplo, '(a b c) '(b a c)).
	  (and (null (set-difference cls target-cls :test #'equal))
		   (null (set-difference target-cls cls :test #'equal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-if-repeated-in-cnf (clause cnf)
;;;
;;; Comprueba si la cláusula clause está repetida en
;;; la FNC pasada como argumento.
;;;
;;; INPUT: clause: cláusula que se va a comprobar.
;;; 	   cnf: FNC que puede o no contener cláusulas repetidas.
;;; OUTPUT: T si hay coincidencias, NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-if-repeated-in-cnf (clause cnf)
  ;Si hemos llegado al final de la FNC,
  ;clause no está repetida.
  (if (null cnf)
	nil
	;Si aún no hemos llegado al final:
	(or (check-if-equal clause (first cnf))		;O coinciden clause y el primer elemento de cnf.
	    (check-if-repeated-in-cnf clause (rest cnf))))) ;O hay coincidencias con alguno del resto
														;de elementos de cnf.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-cnf-without-repeated-clauses (cnf)
;;;
;;; Devuelve una FNC sin cláusulas repetidas.
;;;
;;; INPUT: cnf: FNC que puede o no contener cláusulas repetidas.
;;; OUTPUT: FNC sin cláusulas repetidas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-cnf-without-repeated-clauses (cnf)
	(cond
	  ;Si hemos llegado al final de FNC, 
	  ;no hay más cláusulas repetidas que filtrar.
	  ((null cnf)
		nil)
	  ;Comprueba si la cláusula coincide con alguno de los
	  ;elementos de la FNC. Si coincide, avanza en la FNC.
	  ((check-if-repeated-in-cnf (first cnf) (rest cnf))
		(get-cnf-without-repeated-clauses (rest cnf)))
	  ;Si no hay coincidencias, crea una lista con el 
	  ;elemento único y el resto de la FNC filtrada.
	  (t (cons (first cnf)
			   (get-cnf-without-repeated-clauses (rest cnf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminacion de clausulas repetidas en una FNC 
;; 
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-repeated-clauses (cnf)
  ;Obtiene la FNC sin literales repetidos en sus cláusulas.
  (let ((no-rep-cnf (mapcar #'eliminate-repeated-literals cnf)))
	;Obtiene la FNC sin cláusulas repetidas.
	(get-cnf-without-repeated-clauses no-rep-cnf)))

;;
;; EJEMPLO:
;;
(eliminate-repeated-clauses '(((~ a) c) (c (~ a)) ((~ a) (~ a) b c b) (a a b) (c (~ a) b  b) (a b)))
;;; ((C (~ A)) (C (~ A) B) (A B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.3
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : (list K1) si K1 subsume a K2
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsume (K1 K2)
  ;Mientras K1 sea subconjunto de K2, la función
  ;devolverá una lista con K1.
  (when (subsetp K1 K2 :test #'equal)
	(list K1)))
  
;;
;;  EJEMPLOS:
;;
(subsume '(a) '(a b (~ c)))
;; ((a))
(subsume NIL '(a b (~ c)))
;; (NIL)
(subsume '(a b (~ c)) '(a) )
;; NIL
(subsume '( b (~ c)) '(a b (~ c)) )
;; (( b (~ c)))
(subsume '(a b (~ c)) '( b (~ c)))
;; NIL
(subsume '(a b (~ c)) '(d  b (~ c)))
;; nil
(subsume '(a b (~ c)) '((~ a) b (~ c) a))
;; ((A B (~ C)))
(subsume '((~ a) b (~ c) a) '(a b (~ c)) )
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-subsumed-clauses (cnf) 
  (eliminate-subsumed-clauses-rec cnf cnf))

(defun eliminate-subsumed-clauses-rec (cnf cnf-original)
  (if (null cnf)
      cnf-original
    (if (null (first-needs-removal? (first cnf) (rest cnf)))
        (append 
         (list (first cnf)) 
         (intersection 
          (remove-subsumed-clauses (first cnf) (rest cnf)) 
          (eliminate-subsumed-clauses-rec (rest cnf) cnf-original) 
          :test #'equal))
      (intersection 
          (remove-subsumed-clauses (first cnf) (rest cnf)) 
          (eliminate-subsumed-clauses-rec (rest cnf) cnf-original) 
          :test #'equal))))

(defun first-needs-removal? (first-fbf rest-cnf)
  (if (null (first rest-cnf))
      nil
    (if (null (subsume (first rest-cnf) first-fbf))
        (or nil (first-needs-removal? first-fbf (rest rest-cnf)))
      T)))

(defun remove-subsumed-clauses (first-fbf rest-cnf)
  (if (null (first rest-cnf))
      '()
    (if (null (subsume first-fbf (first rest-cnf)))
        (append (list (first rest-cnf)) (remove-subsumed-clauses first-fbf (rest rest-cnf)))
      (remove-subsumed-clauses first-fbf (rest rest-cnf)))))

(first-needs-removal? '(c) '((b c) (a (~ c) b)  ((~ a) b) (a b (~ a)) (c b a)))
(remove-subsumed-clauses '((~ c)) '((b c) (a (~ c) b)  ((~ a) b) (a b (~ a)) (c b a)))
;;
;;  EJEMPLOS:
;;
(eliminate-subsumed-clauses 
 '((a b c) (b c) (a (~ c) b)  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((A (~ C) B) ((~ A) B) (B C)) ;; el orden no es importante
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (~ c) b) (b)  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((B))
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (~ c) b) ((~ a))  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((A (~ C) B) ((~ A)) (B C))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; literals-with-same-component lit1 lit2)
;;;
;;; Comprueba si 2 literales en forma positiva y negativa
;;; tienen el mismo componente (por ejemplo, p y (~ p))
;;;
;;; INPUT: lit1: literal 1 de la comprobación.
;;;		   lit2: literal 2 de la comprobación.
;;; OUTPUT: t si tienen el mismo componente, NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literals-with-same-component (lit1 lit2)
  (cond
	;CASO 1: El literal es positivo.
	;Comprueba si (~ lit1) tiene el mismo componente
	;que el literal negativo (~ lit2).
	((positive-literal-p lit1)
		(equal (list +not+ lit1) lit2))
	;CASO 2: El literal es negativo.
	;Comprueba si lit1 tiene el mismo componente
	;que el literal positivo lit2.
	((negative-literal-p lit1)
		(equal (second lit1) lit2))
	;Los literales no tienen el mismo componente.
	(t NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-if-pos-and-neg (lit1 K)
;;;
;;; Comprueba si en una cláusula K hay un literal positivo lit1
;;; y otro negativo (~ lit1) o vice versa.
;;;
;;; INPUT: lit1: literal 1 de la comprobación.
;;;		   lit2: literal 2 de la comprobación.
;;; OUTPUT: t si tienen el mismo componente, NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-if-pos-and-neg (lit K)
  ;Si hemos llegado al final de K,
  ;no hay un literal positivo y otro negativo.
  (if (null K)
	nil
	;Sino, comprueba que el literal tiene el mismo componente
	;que el primer elemento de K o que alguno de los elementos 
	;del resto de K.
	(or (literals-with-same-component lit (first K))
		(check-if-pos-and-neg lit (rest K)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tautology-p (K)
  ;Si la cláusula es vacía o tiene sólo un elemento,
  ;su valor de verdad es F.
  (if (or (null K) (null (rest K)))
	nil
	;Sino, comprueba si hay algún literal positivo
	;y negativo con el mismo componente en K.
	(or (check-if-pos-and-neg (first K) (rest K))
		(tautology-p (rest K)))))


;;
;;  EJEMPLOS:
;;
(tautology-p '((~ B) A C (~ A) D)) ;;; T 
(tautology-p '((~ B) A C D))       ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies (cnf) 
  ;Si hemos llegado al final de la FNC, 
  ;no hay más cláusulas que evaluar.
  (if (null cnf)
	nil
	;Sino, comprueba si cada cláusula de la FNC es una tautología.
	(if (tautology-p (first cnf))
	   ;Si lo es, se elimina.
	   (eliminate-tautologies (rest cnf))
	   ;En caso contrario, construye una lista con la cláusula 
	   ;que no es tautología y el resto de la FNC.
	   (cons (first cnf)				  			
			 (eliminate-tautologies (rest cnf))))))

;;
;;  EJEMPLOS:
;;
(eliminate-tautologies 
 '(((~ b) a) (a (~ a) b c) ( a (~ b)) (s d (~ s) (~ s)) (a)))
;; (((~ B) A) (A (~ B)) (A))

(eliminate-tautologies '((a (~ a) b c)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.7
;; FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Función que devuelve la FNC
;; sin literales repetidos.
(defun fnc-sin-literales-repetidos (cnf)
  (eliminate-repeated-literals cnf))

;; Función que devuelve la FNC
;; sin claúsulas repetidas.
(defun fnc-sin-clausulas-repetidas (cnf)
  (let ((cnf-1 (fnc-sin-literales-repetidos cnf)))
    (eliminate-repeated-clauses cnf-1)))

;; Función que devuelve la FNC 
;; sin tautologías.
(defun fnc-sin-tautologias (cnf)
  (let ((cnf-2 (fnc-sin-clausulas-repetidas cnf)))
    (eliminate-tautologies cnf-2)))

;; Función que devuelve la FNC
;; simplificada y sin cláusulas
;; subsumidas.
(defun get-fnc-simplificada (cnf)
  (let ((cnf-3 (fnc-sin-tautologias cnf)))
    (eliminate-subsumed-clauses cnf-3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplifica FBF en FNC 
;;        * elimina literales repetidos en cada una de las clausulas 
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;  
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas, 
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-cnf (cnf) 
  ;Obtiene la FNC simplificada aplicando 
  ;las operaciones especificadas en
  ;las funciones auxiliares.
  (get-fnc-simplificada cnf))

;;
;;  EJEMPLOS:
;;
(simplify-cnf '((a a) (b) (a) ((~ b)) ((~ b)) (a b c a)  (s s d) (b b c a b)))
;; ((B) ((~ B)) (S D) (A)) ;; en cualquier orden

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; FUNCIÓN AUXILIAR  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; contains-pos-or-neg-literal (lit clause)
;;;
;;; Comprueba si el literal lit se encuentra en la
;;; cláusula clause, ya sea en forma positiva o negativa.
;;;
;;; INPUT: lit: literal de la comprobación.
;;;		   clause: cláusula donde se va a buscar.
;;; OUTPUT: t si el literal se encuentra en clause, NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun contains-pos-or-neg-literal (lit clause)
  ;Si clause es NIL, no hay literal que buscar.
  (if (null clause)
	nil
	;O encuentra el literal lit en forma positiva dentro de clause.
    (or (not (null (member lit clause :test #'equal)))
		;O encuentra el literal lit en forma negativa dentro de clause.
		(not (null (member (list +not+ lit) clause :test #'equal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni ~lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-neutral-clauses (lambda cnf) 
  ;Si hemos llegado al final de la FNC, no
  ;hay más cláusulas que evaluar.
  (if (null cnf)
	nil
    ;Sino, comprueba si el literal (positivo o negativo)
	;está en la primera cláusula de la FNC.
	(if (contains-pos-or-neg-literal lambda (first cnf))
		;Si está, elimina la cláusula.
	    (extract-neutral-clauses lambda (rest cnf))
		;Si no está, construye una lista con la cláusula
		;y el resto de elementos de la FNC.
		(cons (first cnf)									  
			  (extract-neutral-clauses lambda (rest cnf))))))

;;
;;  EJEMPLOS:
;;
(extract-neutral-clauses 'p
                           '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((R (~ S) Q) ((~ R) S))


(extract-neutral-clauses 'r NIL)
;; NIL

(extract-neutral-clauses 'r '(NIL))
;; (NIL)

(extract-neutral-clauses 'r
                           '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((P Q) (A B P) (A (~ P) C))

(extract-neutral-clauses 'p
                           '((p (~ q) r) (p q) (r (~ s) p q) (a b p) (a (~ p) c) ((~ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; FUNCIÓN AUXILIAR  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; contains-positive-literal (lit clause)
;;;
;;; Comprueba si el literal positivo lit 
;;; se encuentra en la cláusula clause.
;;;
;;; INPUT: lit: literal de la comprobación.
;;;		   clause: cláusula donde se va a buscar.
;;; OUTPUT: t si el literal positivo se encuentra en clause, 
;;; NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun contains-positive-literal (lit clause)
  ;Si clause es NIL, no hay literal que buscar.
  (if (null clause)
	nil
	;Sino, comprueba si lit se encuentra entre alguno
	;de los elementos de clause.
	(not (null (member lit clause :test #'equal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; Construye el conjunto de clausulas lambda-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(+) subconjunto de clausulas de cnf 
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-positive-clauses (lambda cnf)
  ;Si hemos llegado al final de la FNC,
  ;no hay más cláusulas que analizar.
  (if (null cnf)
	nil
	;Si aún quedan cláusulas, comprueba si no contienen 
	;literales positivos lambda.
	;En ese caso ignorará la cláusula y avanzará en la FNC.
	(if (null (contains-positive-literal lambda (first cnf)))
		(extract-positive-clauses lambda (rest cnf))
		;Si hay literales positivos lambda en la FNC,
		;construye una lista con la cláusula y el resto de
		;elementos de la FNC.
		(cons (first cnf)
			  (extract-positive-clauses lambda (rest cnf))))))
;;
;;  EJEMPLOS:
;;
(extract-positive-clauses 'p
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))

;; ((P (~ Q) R) (P Q) (A B P))


(extract-positive-clauses 'r NIL)
;; NIL
(extract-positive-clauses 'r '(NIL))
;; NIL
(extract-positive-clauses 'r
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((P (~ Q) R) (R (~ S) Q))
(extract-positive-clauses 'p
                             '(((~ p) (~ q) r) ((~ p) q) (r (~ s) (~ p) q) (a b (~ p)) ((~ r) (~ p) s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; FUNCIÓN AUXILIAR  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; contains-negative-literal (lit clause)
;;;
;;; Comprueba si el literal lit se encuentra
;;; en forma negativa (~ lit) en la cláusula clause.
;;;
;;; INPUT: lit: literal de la comprobación.
;;;		   clause: cláusula donde se va a buscar.
;;; OUTPUT: t si el literal negativo se encuentra en clause, 
;;; NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun contains-negative-literal (lit clause)
  ;Si clause es NIL, no hay literal que buscar.
  (if (null clause)
	nil
	;Sino, comprueba si (~ lit) se encuentra entre alguno
	;de los elementos de clause.
    (not (null (member (list +not+ lit) clause :test #'equal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal ~lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lambda cnf)
  ;Si hemos llegado al final de la FNC,
  ;no hay más cláusulas que analizar.
  (if (null cnf)
	nil
	;Si aún quedan cláusulas, comprueba si no contienen 
	;literales negativos (~ lambda).
	;En ese caso ignorará la cláusula y avanzará en la FNC.
	(if (null (contains-negative-literal lambda (first cnf)))
		(extract-negative-clauses lambda (rest cnf))
		;Si hay literales negativos (~ lambda) en la FNC,
		;construye una lista con la cláusula y el resto de
		;elementos de la FNC.
		(cons (first cnf)
			  (extract-negative-clauses lambda (rest cnf))))))

;;
;;  EJEMPLOS:
;;
(extract-negative-clauses 'p
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((A (~ P) C))

(extract-negative-clauses 'r NIL)
;; NIL
(extract-negative-clauses 'r '(NIL))
;; NIL
(extract-negative-clauses 'r
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; (((~ R) S))
(extract-negative-clauses 'p
                             '(( p (~ q) r) ( p q) (r (~ s) p q) (a b p) ((~ r) p s)))
;; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; resolvable-clauses-p (lambda K1 K2)
;;;
;;; Comprueba si dos cláusulas K1 y K2 se pueden resolver.
;;;
;;; INPUT: lambda: literal de la comprobación.
;;;		   K1: cláusula simplificada 1 de la comprobación.
;;;		   K2: cláusula simplificada 2 de la comprobación.
;;; OUTPUT: t si K1 y K2 se pueden resolver, NIL si no.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun resolvable-clauses-p (lambda K1 K2)
  ;O K1 tiene el literal lambda positivo y K2
  ;tiene el literal lambda negativo.
  (or (and (contains-positive-literal lambda K1)
		   (contains-negative-literal lambda K2))
      ;O viceversa.
	  (and (contains-positive-literal lambda K2)
		   (contains-negative-literal lambda K1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-resolved-clause (lambda K)
;;;
;;; Devuelve una cláusula K resuelta sobre lambda.
;;;
;;; INPUT: lambda: literal sobre el que se va a 
;;;        resolver la cláusula.
;;;		   K: cláusula que se va a resolver.
;;; OUTPUT: Cláusula resuelta o NIL si K no contiene a lambda.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-resolved-clause (lambda K)
  (cond
	;CASO 1: K contiene al literal positivo lambda.
	;Todas las ocurrencias de lambda se eliminan de K.
	((contains-positive-literal lambda K)
	 (remove lambda K :test #'equal))
	;CASO 2: K contiene al literal negativo (~ lambda).
	;Todas las ocurrencias de (~ lambda) se eliminan de K.
    ((contains-negative-literal lambda K)
	 (remove (list +not+ lambda) K :test #'equal))
    (t NIL))) ;No debería llegar a este caso nunca.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lambda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lambda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun resolve-on (lambda K1 K2)
  ;Si K1 o K2 son NIL, no hay nada que resolver.
  (if (or (null K1)
          (null K2))
	nil
	;Siempre que K1 o K2 tengan el literal lambda
	;en forma positiva o negativa:
    (when (resolvable-clauses-p lambda K1 K2)
	  ;Obtiene las cláusulas resueltas.
	  (let ((resolved-K1 (get-resolved-clause lambda K1))
			(resolved-K2 (get-resolved-clause lambda K2)))
		;Si al resolver K1 y K2 ambas devuelven NIL, 
		;crea una lista de tipo (NIL).
		;Sino, devolverá la unión entre las cláusulas resueltas.
		(if (and (null resolved-K1) (null resolved-K2))
			(list NIL)
			(union resolved-K1 resolved-K2))))))

;;
;;  EJEMPLOS:
;;
(resolve-on 'p '(a b (~ c) p) '((~ p) b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(a b (~ c) (~ p)) '( p b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(p) '((~ p)))
;; (NIL)


(resolve-on 'p NIL '(p b a q r s))
;; NIL

(resolve-on 'p NIL NIL)
;; NIL

(resolve-on 'p '(a b (~ c) (~ p)) '(p b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(a b (~ c)) '(p b a q r s))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-list-of-resolved-clauses (lambda K clause-set)
;;;
;;; Devuelve una lista de cláusulas resueltas entre K y cada
;;; uno de los elementos de clause-set (conjunto/lista de cláusulas).
;;;
;;; INPUT: lambda: literal sobre el que se va a 
;;;        		   resolver la cláusula.
;;;		   K: cláusula que se va a resolver.
;;;		   clause-set: Conjunto de cláusulas; cada una se resolverá
;;;		   			   con K sobre lambda.
;;; OUTPUT: Lista de cláusulas resueltas sobre lambda.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-list-of-resolved-clauses (lambda K clause-set)
  ;Si hemos llegado al final del conjunto de cláusulas,
  ;no hay más resoluciones que hacer.
  (if (null clause-set)
	nil
	;Resuelve K y la primera cláusula del conjunto sobre lambda.
	(let ((resolved-clause (resolve-on lambda K (first clause-set))))
	  ;Si la cláusula resuelta tiene el valor '(NIL), 
	  ;no hay más cláusulas que resolver.
	  (if (equal resolved-clause '(NIL))
		resolved-clause
		;Sino, construye una lista de cláusulas resueltas, sin 
	    ;literales repetidos.
	    (cons (eliminate-repeated-literals resolved-clause)
		      (get-list-of-resolved-clauses lambda K (rest clause-set)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-all-resolved-clauses (lambda positive-clauses negative-clauses)
;;;
;;; Devuelve una lista de resoluciones entre cada cláusula
;;; del conjunto lambda-positivo y cada cláusula del conjunto
;;; lambda-negativo.
;;;
;;; INPUT: lambda: literal sobre el que se van a 
;;;        		   resolver las cláusulas.
;;;		   positive-clauses: conjunto de cláusulas lambda-positivas.
;;;		   negative-clauses: conjunto de cláusulas lambda-negativas.
;;; OUTPUT: Lista de todas las cláusulas resueltas entre ambos
;;;         conjuntos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-all-resolved-clauses (lambda positive-clauses negative-clauses)
  ;Si hemos llegado al final del conjunto de cláusulas
  ;positivas, no hay más resoluciones que hacer.
  (if (null positive-clauses)
	nil
	  ;Sino, hace la unión entre: 

	  ;    * Las resoluciones entre la primera cláusula 
      ;      positiva y el conjunto de cláusulas negativas.
	  ;    * Las resoluciones del resto de cláusulas positivas
      ;      con el conjunto de cláusulas negativas.
	  (eliminate-repeated-clauses
		(union
		  (get-list-of-resolved-clauses lambda (first positive-clauses) negative-clauses)
		  (get-all-resolved-clauses lambda (rest positive-clauses) negative-clauses)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-RES (lambda cnf)
  ;Si hemos llegado al final de la FNC,
  ;no hay más resoluciones que hacer.
  (if (null cnf)
	nil
	;Obtiene los conjuntos de cláusulas 
	;positivas, negativas y neutras.
	(let ((positive-clauses (extract-positive-clauses lambda cnf))
		  (negative-clauses (extract-negative-clauses lambda cnf))
		  (neutral-clauses (extract-neutral-clauses lambda cnf)))
	  ;Une las cláusulas neutras con el resultado de resolver entre 
      ;el conjunto de cláusulas positivas y el conjunto de cláusulas
      ;negativas sobre lambda.
	  (eliminate-repeated-clauses
	    (union neutral-clauses
			   (get-all-resolved-clauses lambda positive-clauses negative-clauses))))))

;;
;;  EJEMPLOS:
;;
(build-RES 'p NIL)
;; NIL
(build-RES 'P '((A  (~ P) B) (A P) (A B)));; ((A B))
(build-RES 'P '((B  (~ P) A) (A P) (A B)));; ((B A))

(build-RES 'p '(NIL))
;; (NIL)

(build-RES 'p '((p) ((~ p))))
;; (NIL)

(build-RES 'q '((p q) ((~ p) q) (a b q) (p (~ q)) ((~ p) (~ q))))
;; ((P) ((~ P) P) ((~ P)) (B A P) (B A (~ P)))

(build-RES 'p '((p q) (c q) (a b q) (p (~ q)) (p (~ q))))
;; ((A B Q) (C Q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :	T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun  RES-SAT-p (cnf) 
  ;;
  ;; 4.5 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
;;
;; SAT Examples
;;
(RES-SAT-p nil)  ;;; T
(RES-SAT-p '((p) ((~ q)))) ;;; T 
(RES-SAT-p
 '((a b d) ((~ p) q) ((~ c) a b) ((~ b) (~ p) d) (c d (~ a)))) ;;; T 
(RES-SAT-p
 '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) ((~ q)) ((~ p) (~ q) r))) ;;;T
(RES-SAT-p '((P (~ Q)) (K R))) ;;; T
;;
;; UNSAT Examples
;;
(RES-SAT-p '((P (~ Q)) NIL (K R))) ;;; NIL
(RES-SAT-p '(nil))         ;;; NIL
(RES-SAT-p '((S) nil))     ;;; NIL 
(RES-SAT-p '((p) ((~ p)))) ;;; NIL
(RES-SAT-p
 '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) (p) (q) ((~ r)) ((~ p) (~ q) r))) ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical-consequence-RES-SAT-p (wff w)
  ;;
  ;; 4.6 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
(logical-consequence-RES-SAT-p NIL 'a) ;;; NIL
(logical-consequence-RES-SAT-p NIL NIL) ;;; NIL
(logical-consequence-RES-SAT-p '(q ^ (~ q)) 'a) ;;; T 
(logical-consequence-RES-SAT-p '(q ^ (~ q)) '(~ a)) ;;; T 

(logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) '(~ q))
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) '(~q))
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => (a v (~ b))) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => (a v (~ b))) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'a)
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'a)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'q)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ q))
;; NIL

(or 
 (logical-consequence-RES-SAT-p '((p => q) ^ p) '(~q))      ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  'a) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  'q) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  '(~ q)))

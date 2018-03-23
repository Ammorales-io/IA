;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BATERÍA DE EJEMPLOS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    1. Galaxia KW-426
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *planets* '(Trevaris Tantalak Limium Rilooid Nudjor))

(defparameter *white-holes*  
  '((Trevaris Tantalak 2) (Trevaris Limium 7)
    (Tantalak Trevaris 2) (Tantalak Limium 5.1) (Tantalak Rilooid 3)
    (Limium Tantalak 5.1) (Limium Nudjor 10)
    (Rilooid Limium 8)
    (Nudjor Limium 10)))

(defparameter *worm-holes*  
  '((Trevaris Tantalak 1) (Trevaris Limium 9)
    (Tantalak Trevaris 1) (Tantalak Limium 1)
    (Limium Trevaris 9) (Limium Tantalak 1) (Limium Rilooid 6) (Limium Nudjor 12)
    (Rilooid Limium 6) (Rilooid Nudjor 5)
    (Nudjor Limium 12) (Nudjor Rilooid 5)))
 
(defparameter *sensors* 
  '((Trevaris 10) (Tantalak 7) (Limium 9) (Rilooid 5) (Nudjor 0)))

(defparameter *planet-origin* 'Tantalak)
(defparameter *planets-destination* '(Nudjor))
(defparameter *planets-forbidden*   nil)
(defparameter *planets-mandatory*   '(Limium))

(defparameter *galaxy-KW-426* 
  (make-problem 
   :states            *planets*          
   :initial-state     *planet-origin*
   :f-goal-test       #'(lambda (node) 
                          (f-goal-test-galaxy node *planets-destination*
                                              *planets-mandatory*))
   :f-h               #'(lambda (state)
                          (f-h-galaxy state *sensors*))
   :f-search-state-equal #'(lambda (node-1 node-2) 
                             (f-search-state-equal-galaxy node-1 node-2))
   :operators         (list #'(lambda (node)
                                (navigate-white-hole (node-state node) *white-holes*))
                            #'(lambda (node)
                                (navigate-worm-hole (node-state node) *worm-holes* *planets-forbidden*)))))

;;;
;;; EJEMPLOS
;;;

;;;
;;; Ejercicio 1
;;;
(f-h-galaxy 'Tantalak *sensors*) ;-> 7
(f-h-galaxy 'Rilooid *sensors*) ;-> 5
(f-h-galaxy 'Uranus *sensors*) ;-> NIl

;;;
;;; Ejercicio 2
;;;
(navigate-worm-hole 'Trevaris *worm-holes* *planets-forbidden*)  ;->
;;; (#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN TREVARIS :FINAL TANTALAK :COST 1)
;;;  #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN TREVARIS :FINAL LIMIUM :COST 9))
(navigate-white-hole 'Tantalak *white-holes*) ;->
;;; (#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN TANTALAK :FINAL TREVARIS :COST 2)
;;;  #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN TANTALAK :FINAL LIMIUM :COST 5.1)
;;;  #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN TANTALAK :FINAL RILOOID :COST 3))

;;;
;;; Ejercicio 3A
;;;
(defparameter node-01
   (make-node :state 'Tantalak))
(defparameter node-02
   (make-node :state 'Limium :parent node-01))
(defparameter node-03
   (make-node :state 'Rilooid :parent node-02))
(defparameter node-04
   (make-node :state 'Tantalak :parent node-03))
(defparameter node-05
   (make-node :state 'Nudjor :parent node-04))

(f-goal-test-galaxy node-01 '(Nudjor Uranus) '(Limium Rilooid)); -> NIL
(f-goal-test-galaxy node-02 '(Nudjor Uranus) '(Limium Rilooid)); -> NIL
(f-goal-test-galaxy node-03 '(Nudjor Uranus) '(Limium Rilooid)); -> NIL
(f-goal-test-galaxy node-04 '(Nudjor Uranus) '(Limium Rilooid)); -> NIL
(f-goal-test-galaxy node-05 '(Nudjor Uranus) '(Limium Rilooid)); -> T

;;;
;;; Ejercicio 3B
;;;

;;; Under construction
(f-search-state-equal-galaxy node-01 node-01) ;-> T
(f-search-state-equal-galaxy node-01 node-02) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL

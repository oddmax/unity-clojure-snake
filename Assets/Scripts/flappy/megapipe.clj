(ns flappy.megapipe
	(:use arcadia.core)
	(:import [UnityEngine Application Time Debug Vector3]))

(declare do-pipe-update)

(defcomponent MovingMegaPipe [^float speed]
	(Update [this]
		(do-pipe-update this)))

(defn reset-world []
	(Application/LoadLevel Application/loadedLevel))

(defn do-pipe-update [^MovingMegaPipe this]
	(.. this transform (Translate (* (.speed this) Time/deltaTime)
										0
										0))
	;;(Debug/Log (.. this transform position x))
	(if (< (.. this transform position x)
		-2)
	(set! (.. this transform position)
		(Vector3. 2 
			(+ 
				(.. this transform position y)
				(* 1 (rand)))
			 	0))))

(defn calc [a b] 
	(+ a b))
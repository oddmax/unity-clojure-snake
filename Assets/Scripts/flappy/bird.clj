(ns flappy.bird
	(:use arcadia.core)
	(:import [UnityEngine Debug Vector2 Input Rigidbody2D]))

(defcomponent Bird [^Vector2 force]
	(Update [this]
		(if (Input/anyKeyDown)
			(. (get-component this Rigidbody2D)
				(AddForce force ForceMode2D/Impulse)))))
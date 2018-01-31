(ns parade.menu
	(:use arcadia.core hard.input hard.corre)
	(:import [UnityEngine Application Debug Vector2 Input Rigidbody2D]))


(defcomponent Menu []
	(Update [this] 
		(if (key? "space")
			(do 
				(log "text")
				(set! (.text (.GetComponent (find-name "message") "GUIText")) "loading...")
				(load-scene "parade")
			))))
		
			
			;;(Application/LoadLevel "parade"))))

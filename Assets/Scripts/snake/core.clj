(ns snake.core
	(:use arcadia.core hard.input hard.corre)
	(:require [clojure.set :as set])
	(:import [UnityEngine Application Vector2]))

(declare gamestate)

(def world { :world-pos Vector2/zero :width 100 :height 100 :food-amount 100 })
(def pool (atom []))
(def food-go (atom {}))

(def fabs 
  {
    :empty-tile (resource "parade/empty-tile")
    :food (resource "crystals/food")
    :cube (resource "crystals/cube")})

(defn set-text! [n s]
	(set! (.text (.GetComponent (find-name n) "GUIText")) (str s)))

(defn create-snake-game-object []
	(clone! (:cube fabs) [0 1 0]))

(defn create-food-game-object [x z]
	(let [g (clone! (:food fabs) [x 1 z])]
	(swap! food-go #(assoc % [x z] g))
	g))

(defn remove-food [piece]
	(let [g (:g piece)]
		;;(log piece)
		(destroy! g)))

(defn remove-food-vec [food]
	(doseq [piece food
    :let [
          g (:g piece)
          pos (:pos piece)]]
    (do 	
    	(remove-food piece))))

(defn get-pool []
	(if-let [g (first @pool)]
		(do
		(swap! pool #(subvec % 1))
		(.SetActive g true)
		g)
		(create-snake-game-object)))

(defn in-pool [g]
	(do 
		(swap! pool conj g)))

(defn gen-food [] 
	(let [x (rand-int (world :width))
		  z (rand-int (world :width))]
		{:g (create-food-game-object x z) :pos [x z]}))

(defn rand-n []
	(let [r (rand-int 2)]
		(if (= r 0) -1 1)))

(defn rand-move []
	(let [r (rand-int 2)]
		(if (= r 0)
			[(rand-n) 0]
			[0 (rand-n)]
		)))

(defn update-food-offset [piece]
	(update piece :pos #(mapv + % (rand-move))))

(defn move-food [{:keys [food] :as state}]
	(-> state (update :food #(mapv update-food-offset %))))

(defn replenish-food [initial amount]
  (loop [food initial] (if (>= (count food) amount) food (recur (conj food (gen-food))))))

(defn wrap [i m]
  (loop [x i] (cond (< x 0) (recur (+ x m)) (>= x m) (recur (- x m)) :else x)))

(defn create-snake-part [x z]
	(let [g (get-pool)]
		(position! g [x 1 z])
		{:g g :pos [x z]}))

(defn remove-snake-part [part]
	(if-let [gob (:g part)]
		(do
			(.SetActive gob false)
			(in-pool gob))))

(defn grow-snake [{:keys [snake velocity] :as state}]
  (let [[px py] (map + (:pos (peek snake)) velocity)]
    (assoc state :snake (conj snake (create-snake-part (wrap px (world :width)) (wrap py (world :height)))))))

(defn score [snake]
	(dec (count snake)))

(defn check-food [piece pos]
	(= 0 (compare (:pos piece) pos)))

(defn check-not-food [piece pos]
	(not= 0 (compare (:pos piece) pos)))

(defn find-food-in-pos [food pos]
	(let [r (filter #(check-food % pos) food)]
		(if (= 0 (count r)) 
			nil
			r)))

(defn find-food-not-in-pos [food pos]
	(filterv #(check-not-food % pos) food))

(defn eat [{:keys [snake food] :as state}]
	(let [snake-head (:pos (peek snake))]
	  (if-let [food-in-place (find-food-in-pos food snake-head)]
	    (do
	    	(log food-in-place)
	    	;;(map remove-food food-in-place)
	    	(remove-food-vec food-in-place)
	    	;;(remove-food (first food-in-place))
	    	(set-text! "time" (int (score snake)))
	    	(-> state (update :food #(find-food-not-in-pos % snake-head))))
	    (do 
	    	(remove-snake-part (first snake))
	 		(-> state (update :snake subvec 1))))))
 
(defn reset? [{:keys [snake] :as state}]
  (if (apply distinct? snake)
    state
    (do 
    	(assoc state :snake [(peek snake)]))))

(defn setup-state []
  (do 
      {:snake [{:g nil :pos [50 50]}] :velocity [1 0] :score 0 :food (replenish-food [] (world :food-amount))}))

(defn input-update [{:keys [velocity] :as state}]
      (if (key-down? (symbol "LeftArrow"))
  		(if (not= [0 1] velocity) (assoc state :velocity [0 -1]) state)
  		(if (key-down? (symbol "RightArrow")) 
  			(if (not= [0 -1] velocity) (assoc state :velocity [0 1]) state)
  			(if (key-down? (symbol "UpArrow")) 
  				(if (not= [1 0] velocity) (assoc state :velocity [-1 0]) state)
  				(if (key-down? (symbol "DownArrow")) 
  					(if (not= [-1 0] velocity) (assoc state :velocity [1 0]) state)
  					state)))))

(defn food-update [{:keys [food] :as state}]
	(doseq [piece food
    :let [
          g (:g piece)
          pos (:pos piece)
          v3pos [(get pos 0) 1 (get pos 1)]]]
    (do 	
    	(if g
    		(do 
    			(position! g v3pos))))))

(defn repopulate-food [{:keys [food] :as state}]
	(update state :food replenish-food (world :food-amount)))

(defn update-state [state]
	(-> state
		grow-snake 
		eat
		;;move-food
		;;repopulate-food
		reset?))

(defcomponent SnakeBrain [state]
  (Start [this]
  	(set! state world)
  	(set-text! "time" 0)
  	(def gamestate (atom (setup-state)))
  )
  (Update [this]
  	(if (any-key-down?)
    	(swap! gamestate input-update))
    (swap! gamestate update-state)
    (food-update @gamestate)
  )
)
 
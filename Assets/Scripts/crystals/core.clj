(ns crystals.core
	(:use arcadia.core hard.input hard.corre)
	(:require [clojure.set :as set])
	(:import [UnityEngine Application]))

(declare amount board)

(def cur-peg-pos (atom {:pos 0}))

(def fabs 
  {
    :empty-tile (resource "parade/empty-tile")
    :crystal (resource "crystals/crystall")
    })

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil
  otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(defn coor-for-row [board row-num]
  (loop [positions (row-positions row-num)
        result {}
        coor [1 row-num] 
    ] 
    (if (empty? positions)
      result
      (let [[position & remaining] positions]
        (recur remaining
                (assoc result position coor)
                (apply (fn [x z] [(inc x) (dec z)]) coor)
        ))
    ))
  )

(defn create-coordinates-map [board]
  (let [rows (:rows board)
        rows-range (range 1 (inc rows))]
    (reduce conj (map (partial coor-for-row board) rows-range))
  ))

(defn create-board [coordinates]
  (def boardtiles (atom (into {} (for [i (range (count coordinates))
              :let [coor (get coordinates (inc i))
              x (get coor 0)
              z (get coor 1)
              t (clone! (:empty-tile fabs) [x 0.1 z])]]
              (do 
                (set! (.name t) (str x "-" z))
                {[x z] {:empty false :go t :pos (inc i)}} ))))) 
  )

(defn create-crystals [coordinates]
  (def crystals (atom (into {} (for [i (range (count coordinates))
              :let [coor (get coordinates (inc i))
              x (get coor 0)
              z (get coor 1)
              t (clone! (:crystal fabs) [x -0.5 z])]]
              (do 
                (set! (.name t) (str x "-" z))
                {(inc i) {:empty false :go t :pos [x z]}} ))))) 
  )

(defn update-crystals [board crystalmap]
  (into {} 
    (for [i (range amount)
    :let [
          indx (inc i)
          element (get board indx)
          crystal (get crystalmap indx)
          coor (:pos crystal)
          is-pegged (:pegged element)
          go (:go crystal)
          is-empty (:empty crystal)
          ]]
    (do 
        ;;(log indx)
        (when (and is-pegged is-empty)
          (do
            (log "add")
            (let [
              x (get coor 0)
              z (get coor 1)
              t (clone! (:crystal fabs) [x -0.5 z])]
              (swap! crystals #(assoc-in % [indx :go] t))
              (swap! crystals #(assoc-in % [indx :empty] false))
            )
          )
        )
        (when (and (not is-pegged) (not is-empty))
          (do
            (log "destroy")
            (destroy! go)
            (swap! crystals #(assoc-in % [indx :go] nil))
            (swap! crystals #(assoc-in % [indx :empty] true))
          )
        )
        {indx element}
    )))
)

(defn handle-mouse-down []
  (when-let [hit (first (vec (ray-hits (mouse-ray) 5000)))]
    (let [[x y z] (-v + (->vec (.point hit)) 0.0)
        tile (mapv int (-v + [x z] [0.5 0.5]))]
        (when-let [target (get @boardtiles tile)]
        (do
          ;;(log target)
          ;;(swap! board #(remove-peg % (:pos target)))
          ;;(log (get @board (:pos target)))
          ;;(log amount)
          ;;(update-crystals @board @crystals)
          (let [position (:pos target)
                pebble (get @board position)
                is-pegged (:pegged pebble)]
            (do 
              ;;(log position)
              (if is-pegged 
                (do 
                  (swap! cur-peg-pos #(assoc % :pos position))
                  ;;(log position)
                )
                (let [start-pos (:pos @cur-peg-pos)]
                  (log (str "--->" start-pos " " position))
                  (if (not (= start-pos 0)) 
                    (if (valid-move? @board start-pos position)
                      (do
                        (log (str "--->" start-pos " " position))
                        (swap! board #(make-move % start-pos position))
                        (update-crystals @board @crystals)
                      )
                    )
                  )
                )
              )
            )
          )
        )
))))

(defcomponent CrystalBrain []
  (Start [this]
    (def board (atom (new-board 5)))
    (def board-coordinates (create-coordinates-map @board))
    (def amount (row-tri (:rows @board)))
    (create-board board-coordinates)
    (create-crystals board-coordinates)
    (swap! board #(remove-peg % (inc (rand-int 5))))
    (update-crystals @board @crystals)
  )
  (Update [this]
    (.Rotate (.transform (find-name "background")) (vec3 [0 0.2 0]))
    (when (mouse-down?) 
      (do
        (handle-mouse-down)
    ))
  )
)

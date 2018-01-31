(ns hard.corre
  (:require arcadia.core clojure.string)
  (:import
    [UnityEngine Debug Resources GameObject PrimitiveType Application Color Input Screen Gizmos]))

(declare position!)

(defn log 
  ([x] (UnityEngine.Debug/Log x))
  ([x & more] (UnityEngine.Debug/Log (apply str (cons x more)))))

(def delta-time UnityEngine.Time/deltaTime)

(defn vector2? [x] (instance? UnityEngine.Vector2 x))
(defn vector3? [x] (instance? UnityEngine.Vector3 x))
(defn vector4? [x] (instance? UnityEngine.Vector4 x))
(defn transform? [x] (instance? UnityEngine.Transform x))
(defn quaternion? [x] (instance? UnityEngine.Quaternion x))
(defn color? [x] (instance? UnityEngine.Color x))
(defn gameobject? [x] (instance? UnityEngine.GameObject x))
(defn component? [x] (instance? UnityEngine.Component x))

(defn- get-or [col idx nf] (or (get col idx) nf))
    
(defn- -count [o]
  (cond (number? o) 1
        (sequential? o) (count o)
        (vector3? o) 3
        (vector2? o) 2
        (vector4? o) 4
        (color? o) 4))

(defn- -vec [o]
  (cond (number? o) [o]
    (vector2? o) [(.x o)(.y o)]
    (vector3? o) [(.x o)(.y o)(.z o)]
    (vector4? o) [(.x o)(.y o)(.z o)(.w o)]
    (color? o) [(.r o)(.g o)(.b o)(.a o)] 
  :else
  (try (vec o) (catch Exception e (str e)))))

(defn- operate [op -a -b]
  (let [c (max (-count -a)(-count -b))
        a (if (number? -a) (vec (take c (repeat -a)))
              (-vec -a))
        b (if (number? -b) (vec (take c (repeat -b)))
              (-vec -b))]
    (map #(op (get-or a % 0) (get-or b % 0)) (range c))))

(defn- reduce-operate [op col]
  (vec (reduce #(operate op %1 %2) col)))

(defn v+ [& more] (reduce-operate + more))
(defn v- [& more] (reduce-operate - more))
(defn v* [& more] (reduce-operate * more))
(defn vdiv [& more] (reduce-operate / more))
(defn -v [op & more] (reduce-operate op more)) 

(defn V+ [^Vector3 a ^Vector3 b] (Vector3/op_Addition a b))
(defn V- [^Vector3 a ^Vector3 b] (Vector3/op_Subtraction a b))
(defn V* [a b] (Vector3/op_Multiply a b))
(defn V÷ [a b] (Vector3/op_Division a b))
(defn Vx [^Vector3 a ^Vector3 b] (Vector3/Cross a b))


(defn find-name [str] (. GameObject (Find str)))

(defn ->go [v]
  (cond (gameobject? v) v   
      (sequential? v) nil
      (string? v) (try (find-name v) (catch Exception e nil))
      :else (try (.gameObject v) (catch Exception e nil))))

(defn ->transform [v]
  (cond (transform? v) v
      :else (if-let [o (->go v)] (.transform o) nil)))

(defn ->v3 
  ([] (Vector3. 0 0 0))
  ([a b] (Vector3. a b 0))
  ([a b c] (Vector3. a b c))
  ([o] 
  (cond 
    (vector3? o) o
    (gameobject? o) (.position (.transform o))
    (number? o) (Vector3. o o o)
    (sequential? o) (Vector3. (get-or o 0 0) (get-or o 1 0) (get-or o 2 0))
    
    (vector2? o) (Vector3. (.x o) (.y o) 0)
    (vector4? o) (Vector3. (.x o) (.y o) (.z o))
    (quaternion? o) (Vector3. (.x o)(.y o)(.z o))
    (color? o) (Vector3. (.r o)(.g o)(.b o))
    (transform? o) (.position o)
    :else
    (try (.position (.transform (.gameObject o)))
        (catch Exception e (type o))))))

(defn vec3 [o]
	(cond (vector3? o) o
		(sequential? o) (let [[x y z] o] (Vector3. (or x 0) (or y 0) (or z 0)))
		(transform? o)  (.position o)
		(gameobject? o) (.position (.transform o))))

(defn ->vec [o]
  (cond 
    (vector3? o) [(.x o)(.y o)(.z o)]
    (vector2? o) [(.x o)(.y o)]
    (vector4? o) [(.x o)(.y o)(.z o)(.w o)]
    (quaternion? o) [(.x o)(.y o)(.z o)(.w o)]
    (color? o) [(.r o)(.g o)(.b o)(.a o)]
    :else (vec o)))

(defn X [o] (.x (->v3 o)))
(defn Y [o] (.y (->v3 o)))
(defn Z [o] (.z (->v3 o)))

;uh.. so this is not really saving much typing
(defn color-normalized-number [n] (if (> (max n 0) 1) (* n 0.003921569) n))
(defn color 
  ([col] (if (> (count col) 2) (apply color (take 4 col)) (color 0 0 0 0)))
  ([r g b] (color r g b 1.0))
  ([r g b a] (Color. (color-normalized-number r) (color-normalized-number g) (color-normalized-number b) (color-normalized-number a))))



(defn- clamp-v3 [v3 min max]
  (let [v (->vec (->v3 v3))
      res (mapv #(Mathf/Clamp % min max) v)]
      (->v3 res)))

(defn -editor? []
  (. Application isEditor)) 

(defn playing? []
  (. Application isPlaying))

(defn load-scene [sn]
  (log (str "loading scene " sn "..."))
  (Application/LoadLevel sn))

(defn loaded-scene [] Application/loadedLevel)

(defn quit []
  (Application/Quit))

(defn screen-size []
  [(Screen/width)(Screen/height)])

(defn main-camera [] (UnityEngine.Camera/main))

(defn resource [s] (UnityEngine.Resources/Load s))

(defn destroy! [o]
  (if (sequential? o)
    (mapv destroy! o)
    (let [o (if (component? o) o (->go o))]
      (if (-editor?)
        (. GameObject (DestroyImmediate o))
        (. GameObject (Destroy o))))))

(defonce CLONED (atom []))
(def _DATA_ (atom {}))
(def _DEFERRED_ (atom []))
(defn clear-cloned! [] 
  (mapv destroy! @CLONED) 
  (reset! CLONED [])
  (reset! _DATA_ {}))

(defn clone!
  ([ref] (clone! ref nil))
  ([ref pos]
    (let [source (cond (string? ref) (find-name ref)
                       (keyword? ref) (resource (clojure.string/replace (subs (str ref) 1) #"[:]" "/")) 
                       :else ref)
        pos  (if pos (->v3 pos) (->v3 source))
        quat  (.rotation (.transform source))
        gob (. GameObject (Instantiate source pos quat))]
      (set! (.name gob) (.name source))
      (swap! CLONED #(cons gob %))
      gob)))

(defn parent [o] (.parent (->transform o)))

(defn parent! [a b]
  (set! (.parent (.transform a)) (.transform b)) a)

(defn unparent! ^GameObject [^GameObject child]
  (set! (.parent (.transform child)) nil) child)

(defn world-position [o]
  (when-let [o (->go o)] (.TransformPoint (.transform o) (->v3 o))))

(defn position! [o pos]
  (set! (.position (.transform o)) (->v3 pos)))

(defn local-position [o] (.localPosition (.transform o)))

(defn get-position [o] (.position (.transform o)))

(defn local-position! [o pos]
  (set! (.localPosition (.transform o)) (->v3 pos)))

(defn local-direction [o v]
  (when-let [o (->go o)]
    (let [[x y z] (->vec v)]
      (.TransformDirection (.transform o) x y z))))

(defn transform-point [o v]
  (when-let [o (->go o)]
    (.TransformPoint (.transform o) (->v3 v))))

(defn inverse-transform-point [o v]
  (when-let [o (->go o)]
    (.InverseTransformPoint (.transform o) (->v3 v))))

(defn inverse-transform-direction [o v]
  (when-let [o (->go o)]
    (.InverseTransformDirection (.transform o) (->v3 v))))

(defn move-towards [v1 v2 step]
  (Vector3/MoveTowards v1 v2 step))

(defn lerp [v1 v2 ratio]
  (Vector3/Lerp (->v3 v1) (->v3 v2) ratio))


(defn local-scale [o]
  (when-let [o (->go o)] (.localScale (.transform o) )))

(defn local-scale! [o v]
  (when-let [o (->go o)] (set! (.localScale (.transform o)) (->v3 v))))

(defn rotate-around! [o point axis angle]
  (when-let [o (->go o)]
  (. (.transform o) (RotateAround (->v3 point) (->v3 axis) angle))))

(defn rotation [o]
  (when-let [o (->go o)] (.eulerAngles (.rotation (.transform o) ))))

(defn rotate! [o rot]
  (when-let [o (->go o)]
    (.Rotate (.transform o) (->v3 rot)))
  o)

(defn rotation! [o rot]
  (when-let [o (->go o)]
    (set! (.eulerAngles (.transform o)) (clamp-v3 rot 0 360))))

  
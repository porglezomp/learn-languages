(ns nbody.core
  (:gen-class))

(defn except 
  "Returns all the items of the list except the nth item."
  [n list]
  (keep-indexed (fn [index item] 
                  (if (= n index)
                    nil
                    item))
                list))

(defn others [list]
  (for [n (range (count list))]
    (cons (nth list n) (except n list))))

(defn vec-sub [a b]
  (map - a b))

(defn step
  "This function steps the simulation forward, using one step of
  verlet integration. The state vector should be a seq with the
  previous state in the first element, and the current state in the
  second. The dt represents the timestep in seconds."
  [state dt]
  (let [prev (first state)
        curr (second state)
        delta (map vec-sub prev curr)]
    ;(println delta)
    (others curr)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "2d")
  (println (others (range 10)))
  (println (step '(((1 2) (3 4))
                   ((5 6) (7 8)))
                 0.1))
  (println "done"))

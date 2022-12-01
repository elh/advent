; Unfinished

(require '[clojure.string :as str])

(when (not= (count *command-line-args*) 1)
  (do
    (println "expects exactly one command line arg. EXIT")
    (. System exit 1)))

(def inputs (clojure.string/split-lines (slurp (first *command-line-args*))))

(defn to-node [orbitspec]
  (let [parts (str/split orbitspec #"\)")]
    {:id (first parts)
     :children [(second parts)]}))

(defn orbit-tree [orbitspecs]
  (reduce (fn [nodes spec]
            (let [newnode (to-node spec)]
              (if (contains? nodes (get newnode :id))
                (let [prior (get nodes (get newnode :id))]
                   (assoc nodes (get newnode :id) (assoc prior :children (conj (get prior :children) (first (get newnode :children))))))
                (assoc nodes (get newnode :id) newnode))))
          {}
          orbitspecs))

; assumes only 1 immediate parent
(defn depth [orbittree node-id]
  (let [parents (filter (fn [x] (and (some #{node-id} (get x :children)))) (vals orbittree))] 

    ; (println (get (get orbittree "COM") :children))
    ; (println (some #{node-id} (get (get orbittree "COM") :children)))

    (println node-id)
    ; (println (vals orbittree))
    ; (println parents)
    (if (= (count parents) 1)
      (+ 1 (depth orbittree (get (first parents) :id)))
      0)))

; direct and indirect
(defn total-orbit-counts [orbittree]
  (reduce (fn [sum node]
            (+ sum (depth orbittree (get node :id))))
          0
          (vals orbittree)))

; (println inputs)

; (println (orbit-tree inputs))

(println (total-orbit-counts (orbit-tree inputs)))

; (println (depth (orbit-tree inputs) "B"))

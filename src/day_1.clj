(when (not= (count *command-line-args*) 1)
  (do
    (println "expects exactly one command line arg. EXIT")
    (. System exit 1)))

(defn fuel [mass] 
  (- (int (Math/floor (/ mass 3))) 2))

(defn fuel-with-loss
  ([mass] (fuel-with-loss mass 0))
  ([mass sum]
   (let [massfuel (max (- (int (Math/floor (/ mass 3))) 2) 0)]
     (if (= massfuel 0)
       sum
       (recur massfuel (+ sum massfuel))))))

(def inputs (clojure.string/split-lines (slurp (first *command-line-args*))))

(println (time (reduce + (map fuel (map read-string inputs)))))
(println (time (reduce + (map fuel-with-loss (map read-string inputs)))))

(when (not= (count *command-line-args*) 1)
  (do
    (println "expects exactly one command line arg. EXIT")
    (. System exit 1)))

; Fuel required to launch a given module is based on its mass. Specifically, to find the fuel
; required for a module, take its mass, divide by three, round down, and subtract 2.
(defn fuel [mass] 
  (- (int (Math/floor (/ mass 3))) 2))

(def inputs (clojure.string/split-lines (slurp (first *command-line-args*))))

(println (time (reduce + (map fuel (map read-string inputs)))))

; CORRECT: 3479429

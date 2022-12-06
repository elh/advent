(when (not= (count *command-line-args*) 1)
  (do
    (println "expects exactly one command line arg. EXIT")
    (. System exit 1)))

; Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2.
; However, that fuel also requires fuel, and that fuel requires fuel, and so on. Any mass that would require
; negative fuel should instead be treated as if it requires zero fuel; the remaining mass, if any, is instead
; handled by wishing really hard, which has no mass and is outside the scope of this calculation.
(defn fuel
  ([mass] (fuel mass 0))
  ([mass sum]
   (let [massfuel (max (- (int (Math/floor (/ mass 3))) 2) 0)]
     (if (= massfuel 0)
       sum
       (recur massfuel (+ sum massfuel))))))

(def inputs (clojure.string/split-lines (slurp (first *command-line-args*))))

(println (time (reduce + (map fuel (map read-string inputs)))))

; CORRECT: 5216273

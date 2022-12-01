(require '[clojure.string :as str])

(when (not= (count *command-line-args*) 1)
  (throw (Exception. (format "FAIL: expects 1 cmdline arg. got: %d" (count *command-line-args*)))))

(def inputs
  (map str/trim-newline
       (str/split (slurp (first *command-line-args*)) #"\n")))

(defn elves [calorie-list]
  (letfn
   [(agg [vs v]
         (if (str/blank? v)
           (conj vs 0)
           (conj (vec (butlast vs)) (+ (last vs) (Integer/parseInt v)))))]
    (reduce agg [0] (vec calorie-list))))

(defn top-n-elves [n elves]
  (take n (sort (fn [a b] (compare b a)) elves)))

; part 1
(println (apply max (elves inputs)))

; part 2
(println (reduce + (top-n-elves 3 (elves inputs))))

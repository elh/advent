(require '[clojure.string :as str])

(when (not= (count *command-line-args*) 1)
  (throw (Exception. (format "FAIL: expects 1 cmdline arg. got: %d" (count *command-line-args*)))))

; as vector
(def input
  (vec (map str/trim-newline (str/split (slurp (first *command-line-args*)) #"\n"))))

(defn full-overlap? [line]
  (let [parts (map #(Integer/parseInt %) (str/split line #",|-"))]
    (or
     (and (<= (first parts) (nth parts 2)) (>= (second parts) (nth parts 3)))
     (and (>= (first parts) (nth parts 2)) (<= (second parts) (nth parts 3))))))

; 2 ranges overlap if they they start before the other ends
(defn partial-overlap? [line]
  (let [parts (map #(Integer/parseInt %) (str/split line #",|-"))]
    (and (<= (first parts) (nth parts 3)) (>= (second parts) (nth parts 2)))))

; part 1
(println (->> input
              (map full-overlap?)
              (filter true?)
              (count)))

; part 2
(println (->> input
              (map partial-overlap?)
              (filter true?)
              (count)))

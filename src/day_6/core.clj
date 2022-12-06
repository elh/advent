(ns day-6.core
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (vec (map str/trim-newline (str/split (slurp file-name) #"\n"))))

(defn build-graph [lines]
  (letfn [(agg [s l]
            (let [[in out] (str/split l #"\)")]
              (assoc s out in)))]
    (reduce agg {} lines)))

(defn count-orbits [g]
  (letfn [(count-orbits-for-node [n] (loop [cur n
                                            count 0]
                                       (if (nil? (get g cur))
                                         count
                                         (recur (get g cur) (+ count 1)))))]
    (reduce + (map count-orbits-for-node (keys g)))))

(defn orbit-ancestors [g n]
  (loop [cur n
         acc []]
    (if (nil? (get g cur))
      acc
      (recur (get g cur) (conj acc (get g cur))))))

(defn common-ancestor [g n1 n2]
  (let [a1 (orbit-ancestors g n1)
        a2 (orbit-ancestors g n2)]
    (loop [a1 a1
           a2 a2]
      (if (empty? a1)
        nil
        (if (contains? (set a2) (first a1))
          (first a1)
          (recur (rest a1) a2))))))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn orbital-transfers [g n1 n2]
  (let [a1 (orbit-ancestors g n1)
        a2 (orbit-ancestors g n2)
        ca (common-ancestor g n1 n2)
        d1 (first (indices #(= ca %) a1))
        d2 (first (indices #(= ca %) a2))]
    (+ d1 d2)))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [input (read-input (first args))]
    (println "part 1:" (count-orbits (build-graph input)))
    (println "part 2:" (orbital-transfers (build-graph input) "YOU" "SAN"))))

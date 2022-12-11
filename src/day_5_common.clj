(ns day-5-common
  (:require [intcode]))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [program (intcode/parse-program (slurp (first args)))]
    (println "PART 1:" (time (intcode/run program [1])))
    (println "PART 2:" (time (intcode/run program [5])))))

(ns day-2-common
  (:require [intcode]))

(defn setup [program noun verb]
  (assoc (assoc program 1 noun) 2 verb))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [program (intcode/parse-program (slurp (first args)))]
    (println "PART 1:" (intcode/run (setup program 12 2) 0))))

(ns day-7
  (:require [intcode]))

(defn permutations [vs]
  (if (= 1 (count vs))
    (list vs)
    (for [head vs
          tail (permutations (remove #{head} vs))]
      (cons head tail))))

(defn amp-series [program setting]
  (loop [setting setting out {:outputs [0]}]
    (if (empty? setting)
      out
      (recur (rest setting) (intcode/run program [(first setting), (get-in out [:outputs 0])])))))

(defn amp-loop [program setting]
  (loop [amps (reduce #(assoc %1 %2 {:program program :setting (nth setting %2)}) {} (range 5))
         results (reduce #(assoc %1 %2 {}) {} (range 5))
         amp-idx 0
         out 0]
    (if (every? #(= :halted (:status (second %))) results)
      (results (dec (count results)))
      (let [amp (amps amp-idx)
            res (if (empty? (results amp-idx))
                  (intcode/run (:program amp) [(:setting amp), out])                      ;; first run. input the setting and initial input
                  (intcode/run (:program amp) [out] [] (get-in results [amp-idx :pc])))]  ;; subsequent run. resume at last pc w/ new input
        (if (empty? (:outputs res))
          (throw (Exception. "unexpected empty output"))
          (recur (assoc-in amps [amp-idx :program] (:program res))
                 (assoc results amp-idx res)
                 (mod (inc amp-idx) (count amps))
                 (get-in res [:outputs 0])))))))

(defn max-thruster [program amp-fn settings]
  (loop [best-out -1 best-setting nil settings settings]
    (if (empty? settings)
      {:best-out best-out :best-setting best-setting}
      (let [cur (get-in (amp-fn program (first settings)) [:outputs 0])]
        (if (> cur best-out)
          (recur cur (first settings) (rest settings))
          (recur best-out best-setting (rest settings)))))))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [program (intcode/parse-program (slurp (first args)))]
    (println "PART 1:" (time (:best-out (max-thruster program amp-series (permutations (range 5))))))
    (println "PART 2:" (time (:best-out (max-thruster program amp-loop (permutations (range 5 10))))))))

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

;; TODO: we need to be able to run intcode computer and have it block and resume I/O
;; this is wrong
(defn amp-loop [program setting]
  (loop [amps (reduce #(assoc %1 %2 {:program program :setting (nth setting %2)}) {} (range 5))
         amp-idx 0
         out 0]
    (let [amp (amps amp-idx)
          res (intcode/run (:program amp) [(:setting amp), out])]
      (println "res" res)
      (if (empty? (:outputs res))
        res
        (recur (assoc-in amps [amp-idx :program] (:program res)) (mod (inc amp-idx) (count amps)) (get-in res [:outputs 0]))))))

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
    (println (amp-loop program [9,8,7,6,5]))
    (println "PART 1:" (time (:best-out (max-thruster program amp-series (permutations (range 5))))))
    (println "PART 2:" (time "TODO"))))

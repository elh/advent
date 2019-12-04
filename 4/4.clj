; copied this from SO
(defn digits [n]
  (for [d  (str n)]
    (- (byte d) 48)))

(defn validperms [a b]
  (filter (fn [perm] 
            (second
             (reduce (fn [data digit]
                       [digit (or (second data) (= (first data) digit))])
                     [-1 false]
                     perm)))
          (filter #(= (sort %) %)
                  (map digits
                       (range (+ a 1) b)))))

(defn pt2perms [a b]
  (filter (fn [perm]
            (not= -1 (.indexOf (get
                                (reduce (fn [data digit]
                                          {
                                           :prior digit
                                           :runs (if
                                                  (= (get data :prior) digit)
                                                   (conj (pop (get data :runs)) (+ 1 (last (get data :runs))))
                                                   (conj (get data :runs) 0)
                                                   )
                                           })
                                        {:prior -1 :runs [0]}
                                        perm)
                                :runs)
                               1)))
          (filter #(= (sort %) %)
                  (map digits
                       (range (+ a 1) b)))))

; p1
(println (time (count (validperms 108457 562041))))
; CORRECT: 2779

; p2
(println (time (count (pt2perms 108457 562041))))
; CORRECT: 1972


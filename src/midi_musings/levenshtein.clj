(ns midi-musings.levenshtein)

(defn flip-reduce
  [val f coll]
  (reduce f val coll))

(defn naive
  [s t]
  (-> {[0 0] {:value 0, :parent nil}}
      (flip-reduce (fn [m i]
                     (assoc m [i 0] {:value i, :parent [(dec i) 0]}))
                   (range 1 (inc (count s))))
      (flip-reduce (fn [m j]
                     (assoc m [0 j] {:value j, :parent [0 (dec j)]}))
                   (range 1 (inc (count t))))
      (flip-reduce (fn [m [i j]]
                     (assoc
                      m
                      [i j]
                      (min-key :value
                               (let [p [(dec i) (dec j)]]
                                 {:value (+ (:value (get m p))
                                            (if (= (nth s (dec i)) (nth t (dec j)))
                                              0 1))
                                  :parent p})
                               (let [p [(dec i) j]]
                                 {:value (inc (:value (get m p)))
                                  :parent p})
                               (let [p [i (dec j)]]
                                 {:value (inc (:value (get m p)))
                                  :parent p}))))
                   (for [i (range 1 (inc (count s)))
                         j (range 1 (inc (count t)))]
                     [i j]))))

(defn trace
  [s t m]
  (let [end [(count s) (count t)]]
    (letfn [(f [[i j] path]
              (let [path (cons [i j] path)]
                (if (or (= 0 i j) (= 0 (:value (get m [i j]))))
                  path
                  #(g (:parent (get m [i j]))
                      path))))
            (g [pos path] (f pos path))]
      (trampoline
       (partial f end)
       ()))))

;; TODO: just going to assume for now that if you go diagonally you are substituting
;; Might fix this in trace

(defn string-trace
  [s t trace]
  (reductions
   (fn [st [[i1 j1] [i2 j2]]]
     (cond
       (and (not= i1 i2) (not= j1 j2))
       (assoc st j2 (nth s i2))

       (not= j1 j2)
       (vec (butlast st))

       (not= i1 i2)
       (vec (concat [(nth s i2)] st))

       :else
       st))
   (vec t)
   (partition 2 1 (reverse trace))))


(defn edit-trace
  [s t]
  (->> (naive s t)
       (trace s t)
       (string-trace s t)))

(edit-trace "foobar" "bazoo")

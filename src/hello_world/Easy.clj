(ns hello-world.Easy)

[1 2 3] 2

(apply (repeat [1 2 3] 2))

() #(mapcat repeat (repeat (count %1) %2) %1) [1 2 3] 2

(take 3 (iterate #(+ 3 %) 4))

(interpose 0 [1 2 3])

#(drop 1 (interleave (repeat (count %2) %1) %2))

[1 1 2 3 3 2 2 3]

;; #30 - This is throwing an error at the moment about too few
;; arguments to if, which doesn't make any sense to me
((fn compress [seq]
   (loop [c (first seq)
          r (rest seq)
          result '()]
     (if (empty? r)
       (if (= c (first r))
         (recur (first rest) (rest r) result)
         (recur (first rest) (rest r) (conj result c)))
       result)
     )) [1 1 2 3 3 2 2 3])
;; Above returns empty

[1 1 2 3 3 2 2 3]

((fn [seq]
   (loop [f (first seq)
          r (rest seq)
          ret []]
     (if (empty? r)
       (conj ret f)
       (if (= f (first r))
         (recur (first r) (rest r) ret)
         (recur (first r) (rest r) (conj ret f))
         )))) [1 1 2 3 3 2 2 3])

(partition-by identity [1 1 2 3 3 2 2 3])

((fn [coll] (reduce (fn [a b]
                      (if (= (last a) b) a (conj a b))) [] coll))
  [[1 2] [1 2] [3 4]])

;; #31 Pack a sequence
(fn [s]
  (loop [f (first s)
         r (rest s)
         ret []]
    (if (empty? r)
      ret
      (if (= f (first r))
        (recur (first r) (rest r) (cons ))))))

;; #41 Drop every nth Item - I can't figure this one out either.  I'm really
;; struggling to read where the % stuff goes
[1 2 3 4 5 6 7 8] 3
[:a :b :c :d :e :f] 2

((fn [coll n]
   (->> (partition-all n coll)
        (map #(take (- n 1) %))
        flatten))
  [1 2 3 4 5 6 7 8 9 10 11 12 13] 3)

((fn [coll n]
   (partition 2 (interleave (take (count n) (range)) coll)) ) [:a :b :c :d :e
                                                                :f] 2)

(fn [coll n]
  (partition 2 (interleave (take count n) range)))

((fn [seq n]
    (filter #(not= 0 (mod (.indexOf %) n)) seq)) [1 2 3 4 5 6 7 8] 3)

(flatten (map drop-last (partition-all 2 [:a :b :c :d :e :f])))
(#(->> %1
       (partition-all %2)
       (map (fn [%1 %2] (take (- %2 1) seq)))
       (flatten)) [1 2 3 4 5 6 7 8] 3)

(fn [seq n] (take (- n 1) seq))


((fn [coll n]
   (filter #(if (not= %2 n) %1) coll)) [1 2 3 4 5 6 7 8] 3))

(fn [coll n]
  (keep-indexed #(if (not= %2 n) %1) coll))

;; This one I copied from someone else's solution and it still doesn't work
((fn [s n]
    (loop [i 1
           s s
           ret []]
      (if (empty? s)
        ret
        (recur (inc i) (rest s) (if (= 0 (mod i n)) ret (conj ret (first s)))))))
  3 [1 2 3 4 5 6])

;;#51
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

;;#61 - Map Construction
((fn [set1 set2]
   (into {} (map vector set1 set2)))
  [:a :b :c] [1 2 3])

;;#66 Greatest Common Divisor - Solved
(fn [a b]
  (if (zero? b)
    a
    (recur b (mod a b)))
  )

;; #166 Comparisons
(= :gt (__ < 5 1))
(fn [x y] (< (count x) (count y))) "pear" "plum"

((fn [f x y]
   (if (f x y)
     :lt
     (if (f y x)
       :gt
       :eq)))
  < 1 1)

(exp 3 3)

;; #90 Cartesian Product
#{1 2} #{3 4}

(map vector #{1 2} #{1 2 3 4})
(zipmap #{3 4 5 6} #{1 2} )


(fn [set1 set2]

  )


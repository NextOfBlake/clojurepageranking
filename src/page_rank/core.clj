(ns page-rank.core
  (:require [clojure.java.io :as io])
)

(def pagesfile "pages.txt")
(def pagecount 10000)
(def pagesrank (vec (repeat pagecount 1)))
(def weight 0.85)

(defn Reader []
  (with-open [rdr (clojure.java.io/reader pagesfile)]
    (reduce conj [] (line-seq rdr))
  )  
)

(defn Summation [expression count]
  (loop [x 0, sum 0]
    (if (< x count)
      (recur (+ x 1) (+ sum expression))
      sum
    )
  )
)

(defn PageRank [page] 
  (+ (- 1 weight) (* weight (Summation 1 10))) 
)

(defn -main []
  (println (PageRank 1))
  ; (def pages [])
  ; (def index (get [1, 2, 3] 0))
  ; (print index)
  ; (doseq [line (Reader)]
  ;   (println line)
  ; )
)
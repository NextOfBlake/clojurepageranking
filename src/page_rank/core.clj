(ns page-rank.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
)

(def pagesfile "pages.txt")
(def pagecount 10000)
(def pageranks (vec (repeat pagecount 1)))
(def weight 0.85)

(defn Reader []
  (with-open [rdr (clojure.java.io/reader pagesfile)]
    (reduce conj [] (line-seq rdr))
  )  
)

(defn Contains [vector value]
  (some #(= (str value) %) vector)
)

(defn Summation [expression count]
  (loop [x 0, sum 0]
    (if (< x count)
      (recur (+ x 1) (+ sum expression))
      sum
    )
  )
)

(defn CalculatePageRank [expression count] 
  (+ (- 1 weight) (* weight (Summation expression count))) 
)

(defn GetPageRank [index]
  (get pageranks index)
)

(defn GetOutboundPages [pages from]
  (get pages from)
)
(defn GetInboundPages [pages to]

  (loop [from 0, inbound []]
    (if (< from pagecount)
      (if (Contains (get pages from) to)
        (recur (+ from 1) (conj inbound from))
        (recur (+ from 1) inbound)
      )
      inbound
    )
  )
)

(defn -main []
  (def pages [])

  (doseq [line (Reader)]
    (def pages 
      (conj pages (str/split line #" "))
    )
  )
  
  (println (GetInboundPages pages 0))
  ; (def inbound 
  ;   (conj inbound 
  ;     if(Find ())
  ;   )
  ; )

  ; (println inbound)
  
)
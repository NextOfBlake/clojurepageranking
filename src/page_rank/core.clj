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

(defn GetPageRank [page]
  (get pageranks page)
)

(defn SetPageRank [page value]
  (assoc pageranks page value)
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

(defn CalculatePageRank [pages inbound_pages]
  ; (+ (- 1 weight) 
  ;   (* weight 
  ;     ; summation

  ;   )
  ; )
  (loop [x 0, sum 0]
    (if (< x (count inbound_pages))
      (recur (+ x 1)
        (+ sum 
          (/ GetPageRank (get inbound_pages x) (count (GetOutboundPages pages (get inbound_pages x))))
        )
      )
      sum
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
  
  (println (GetOutboundPages pages 0))
  (println (GetInboundPages pages 0))

  (println (CalculatePageRank pages (GetInboundPages pages 0)))
)
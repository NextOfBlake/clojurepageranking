(ns page-rank.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:import (java.util.concurrent Executors ExecutorService Callable))
)

(def pagesfile "shortpages.txt")
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

(defn CalculatePageRank [pages inbound_pages prev_page_ranking]
  (+ (- 1 weight) 
    (* weight 
      ; summation
      (loop [x 0, sum 0]
        (if (< x (count inbound_pages))
          (recur 
            (+ x 1)
            (+ sum 
              (/ @(get (into [] prev_page_ranking) x) (count (GetOutboundPages pages (get inbound_pages x))))
            )
          )
          sum
        )
      )
    )
  )
)

(defn Hello [pagerank page pages prev_page_ranking]  
  (CalculatePageRank pages (GetInboundPages pages page) prev_page_ranking)
)

(defn ProcessRankings [pages threads iterations]
  (let [
        refs  (map ref (repeat (count pages) 1))
        pool  (Executors/newFixedThreadPool threads)
        tasks (map (fn [t]
                      (fn []
                        (dotimes [n iterations]
                          (dosync
                            (doseq [[index rank] (map-indexed vector refs)]
                              (alter rank Hello index pages refs)
                            )
                          )    
                        )
                      )
                    )
                    (range threads)
                )
        ]
      (doseq [future (.invokeAll pool tasks)]
        (.get future)
      )
      (.shutdown pool)
      (map deref refs)
    )
)

(defn -main []
  ; Import pages from file
  (def pages [])
  (def threads 1)

  (doseq [line (Reader)]
    (def pages 
      (conj pages (nthrest (str/split line #" ") 1))
    )
  )

  (println (ProcessRankings pages threads 10))
)
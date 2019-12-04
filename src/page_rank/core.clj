(ns page-rank.core
  (:require [clojure.java.io :as io])
)

(def Pages "pages.txt") 

(defn Reader []
  (with-open [rdr (clojure.java.io/reader Pages)]
    (reduce conj [] (line-seq rdr))
  )  
)

(defn -main []
  (doseq [line (Reader)]
    (println line)  
  )
)
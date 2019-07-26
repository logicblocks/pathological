(ns support.streams
  (:require
    [support.io :as io]))

(defn new-string-streams
  [& {:keys [input]
      :or   {input ""}}]
  {:input  (io/new-string-reader input)
   :output (io/new-string-writer)
   :error  (io/new-string-writer)})

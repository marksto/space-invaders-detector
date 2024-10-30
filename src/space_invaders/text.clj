(ns space-invaders.text
  "General text manipulations"
  (:require [clojure.string :as str]))

(defn text-dimensions [text-str]
  (let [text-lines (str/split-lines text-str)]
    [(count (first text-lines)) (count text-lines)]))

(defn text-str->char-seqs [text-str]
  (map seq (str/split-lines text-str)))

(defn text-str->char-seq [text-str]
  (remove #{\newline} (seq text-str)))

(defn extract-char-subseq
  [char-seqs [idx idy] [width height]]
  (->> char-seqs
       (drop idy) (take height)
       (mapcat #(->> % (drop idx) (take width)))))

(defn str-vec->text-str [str-vec]
  (str/join \newline str-vec))

(defn str-vec->char-seqs [str-vec]
  (map seq str-vec))

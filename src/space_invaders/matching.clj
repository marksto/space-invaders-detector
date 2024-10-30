(ns space-invaders.matching
  "General pattern matching in a multi-line input string"
  (:require [clj-fuzzy.metrics :as fuzzy-metrics]
            [clojure.string :as str]
            [failjure.core :as f]
            [fipp.edn :refer [pprint]]
            [space-invaders.text :as t]))

;; patterns

(defn validate-pattern-str
  [pattern-str valid-pattern-chars]
  (let [pattern-lines (str/split-lines pattern-str)]
    (cond
      (seq (mapcat #(remove valid-pattern-chars %) pattern-lines))
      (f/fail "Pattern must contain only valid characters\n%s"
              (with-out-str (pprint pattern-lines)))
      ;;
      (not= 1 (count (set (map count pattern-lines))))
      (f/fail "Pattern lines have to be of the same length\n%s"
              (with-out-str (pprint pattern-lines)))
      ;;
      :else pattern-str)))

(defn ->pattern [pattern-str]
  {:pattern/text      pattern-str
   :pattern/dims      (t/text-dimensions pattern-str)
   :pattern/char-seqs (t/text-str->char-seqs pattern-str)})

;; input strings

(defn validate-input-str
  [input-str [min-width min-height :as min-dims]]
  (let [input-lines (str/split-lines input-str)
        [input-width input-height :as input-dims] (t/text-dimensions input-str)]
    (cond
      (not= 1 (count (set (map count input-lines))))
      (f/fail "Input lines have to be of the same length\n%s"
              (with-out-str (pprint input-lines)))
      ;;
      (or (< input-width min-width)
          (< input-height min-height))
      (f/fail "Input dimensions have to fit all patterns\n%s"
              (with-out-str (pprint {:input-dims input-dims
                                     :min-dims   min-dims})))
      ;;
      :else input-str)))

(defn ->input
  [input-str]
  {:input/text      input-str
   :input/dims      (t/text-dimensions input-str)
   :input/char-seqs (t/text-str->char-seqs input-str)})

;; pattern matching

(defn- calc-distance
  [char-seq-1 char-seq-2]
  (assert (= (count char-seq-1) (count char-seq-2))
          "The Hamming distance is only defined for seqs of the same length")
  (fuzzy-metrics/hamming char-seq-1 char-seq-2))

(defn- ->pattern-match
  ([p-char-seq p-dims i-char-seqs i-loc min-accuracy]
   (->pattern-match p-char-seq p-dims i-char-seqs i-loc min-accuracy nil))
  ([p-char-seq p-dims i-char-seqs i-loc min-accuracy edge-kind]
   (let [i-subseq (t/extract-char-subseq i-char-seqs i-loc p-dims)
         distance (calc-distance p-char-seq i-subseq)
         accuracy (* 100.0 (- 1 (/ (double distance) (count p-char-seq))))]
     (when (<= min-accuracy accuracy)
       (cond-> {:match/location  i-loc
                :match/distance  distance
                :match/accuracy  accuracy
                :match/char-seqs (partition (first p-dims) i-subseq)}
               (some? edge-kind) (assoc :match/edge-kind edge-kind))))))

;;

(defmulti edge-base-locs
  (fn [edge-kind _pattern _input]
    edge-kind))

(defmethod edge-base-locs :top
  [_
   {[p-width] :pattern/dims :as _pattern}
   {[i-width] :input/dims :as _input}]
  (map #(vector % 0)
       (range (inc (- i-width p-width)))))

(defmethod edge-base-locs :left
  [_
   {[_ p-height] :pattern/dims :as _pattern}
   {[_ i-height] :input/dims :as _input}]
  (map #(vector 0 %)
       (range (inc (- i-height p-height)))))

(defmethod edge-base-locs :bottom
  [_
   {[p-width p-height] :pattern/dims :as _pattern}
   {[i-width i-height] :input/dims :as _input}]
  (map #(vector % (- i-height p-height))
       (range (inc (- i-width p-width)))))

(defmethod edge-base-locs :right
  [_
   {[p-width p-height] :pattern/dims :as _pattern}
   {[i-width i-height] :input/dims :as _input}]
  (map #(vector (- i-width p-width) %)
       (range (inc (- i-height p-height)))))

;;

(defmulti edge-shifts
  (fn [edge-kind _pattern _min-sub-pattern]
    edge-kind))

(defmethod edge-shifts :top
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-height min-sub-pattern))
       (range 1)
       (map (fn [y-shift]
              {:shift/pattern-loc  [0 y-shift]
               :shift/pattern-dims [p-width (- p-height y-shift)]}))))

(defmethod edge-shifts :left
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-width min-sub-pattern))
       (range 1)
       (map (fn [x-shift]
              {:shift/pattern-loc  [x-shift 0]
               :shift/pattern-dims [(- p-width x-shift) p-height]}))))

(defmethod edge-shifts :bottom
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-height min-sub-pattern))
       (range 1)
       (map (fn [y-shift]
              {:shift/pattern-loc  [0 0]
               :shift/pattern-dims [p-width (- p-height y-shift)]
               :shift/input-loc-fn (fn [[idx idy]] [idx (+ idy y-shift)])}))))

(defmethod edge-shifts :right
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-width min-sub-pattern))
       (range 1)
       (map (fn [x-shift]
              {:shift/pattern-loc  [0 0]
               :shift/pattern-dims [(- p-width x-shift) p-height]
               :shift/input-loc-fn (fn [[idx idy]] [(+ idx x-shift) idy])}))))

;;

(defn- find-edge-matches
  [edge-kind
   {p-char-seqs :pattern/char-seqs :as pattern}
   {i-char-seqs :input/char-seqs :as input}
   excluded-base-locs
   min-sub-pattern
   min-accuracy]
  (let [edge-base-locs (remove excluded-base-locs
                               (edge-base-locs edge-kind pattern input))
        edge-shifts    (edge-shifts edge-kind pattern min-sub-pattern)]
    (reduce
      (fn [matches base-loc]
        (if-some [new-match (reduce
                              (fn [_ {p-loc    :shift/pattern-loc
                                      p-dims   :shift/pattern-dims
                                      i-loc-fn :shift/input-loc-fn}]
                                (let [p-char-subseq (t/extract-char-subseq
                                                      p-char-seqs
                                                      p-loc p-dims)
                                      shifted-i-loc (if (some? i-loc-fn)
                                                      (i-loc-fn base-loc)
                                                      base-loc)]
                                  (when-some [match (->pattern-match
                                                      p-char-subseq p-dims
                                                      i-char-seqs shifted-i-loc
                                                      min-accuracy edge-kind)]
                                    (reduced match))))
                              nil
                              edge-shifts)]
          (conj matches new-match)
          matches))
      []
      edge-base-locs)))

(defn- find-matches-on-edges
  [pattern input full-matches min-sub-pattern min-accuracy]
  (let [fm-locs (set (map :match/location full-matches))]
    (concat
      (find-edge-matches :top
                         pattern input fm-locs min-sub-pattern min-accuracy)
      (find-edge-matches :left
                         pattern input fm-locs min-sub-pattern min-accuracy)
      (find-edge-matches :bottom
                         pattern input fm-locs min-sub-pattern min-accuracy)
      (find-edge-matches :right
                         pattern input fm-locs min-sub-pattern min-accuracy))))

(defn- find-full-matches
  [{pattern-str :pattern/text [p-width p-height :as p-dims] :pattern/dims :as _pattern}
   {i-char-seqs :input/char-seqs [i-width i-height] :input/dims :as _input}
   min-accuracy]
  (let [p-char-seq (t/text-str->char-seq pattern-str)]
    (for [idy (range (inc (- i-height p-height)))
          idx (range (inc (- i-width p-width)))
          :let [match (->pattern-match p-char-seq p-dims
                                       i-char-seqs [idx idy]
                                       min-accuracy)]
          :when (some? match)]
      match)))

(defn find-matches
  ([pattern-str input-str]
   (find-matches pattern-str input-str nil))
  ([pattern-str input-str {:keys [min-accuracy
                                  search-on-edges
                                  min-sub-pattern]
                           :or   {min-accuracy    100.0
                                  min-sub-pattern 1}
                           :as   _opts}]
   (let [pattern      (->pattern pattern-str)
         input        (->input input-str)
         full-matches (find-full-matches pattern input min-accuracy)]
     (if search-on-edges
       (concat full-matches
               (find-matches-on-edges
                 pattern input full-matches min-sub-pattern min-accuracy))
       full-matches))))

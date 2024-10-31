(ns space-invaders.printing
  "Various ways to print matches out"
  (:require [clansi.core :refer [style]]
            [clojure.string :as str]
            [fipp.edn :refer [pprint]]
            [space-invaders.text :as t]))

(defn- ->output-match
  [{:match/keys [location char-seqs distance accuracy edge-kind]
    :as         _match}]
  (cond-> {:location location
           #_#_:distance distance ; for debug purposes
           :accuracy (format "%.2f%%" accuracy)
           :matching (mapv str/join char-seqs)}
          (some? edge-kind) (assoc :edge-kind edge-kind)))

(def matches-comp
  "Compares matches first by the total length (the bigger the better)
   and then by `accuracy` of the match (again, the bigger the better)."
  (juxt #(- (reduce + (map count (:match/char-seqs %))))
        #(- (:match/accuracy %))))

(defn print-results [results]
  (when (some? results)
    (let [total-matches (reduce + 0 (map (comp count second) results))]
      (println (format "Found %s possible invader matches in total.\n"
                       total-matches)))
    (doseq [[invader-type matches] results]
      (println (format "Found %s possible '%s' invader matches:"
                       (count matches) (name invader-type)))
      (doseq [output-match (->> matches
                                (sort-by matches-comp)
                                (map ->output-match))]
        (pprint output-match))
      (println))))

;;

(defn- invader-type->color [invaders]
  (let [predefined-colors [:red :green :blue :yellow :magenta :cyan]
        invader-types     (distinct (map :invader/type invaders))]
    (when (< (count predefined-colors)
             (count invader-types))
      (println
        (style "Not enough predefined colors for all invader types" :yellow)))
    (zipmap invader-types (concat predefined-colors (repeat :black)))))

(defn- accumulate-matches
  [acc matches color]
  (let [{char-seqs :match/char-seqs
         [mx my]   :match/location} (first matches)]
    (reduce
      (fn [acc [idy m-line]]
        (reduce (fn [acc idx]
                  ;; NB: This will overwrite previously added matches
                  ;;     with those coming up later, but this is fine.
                  ;;     Other approaches are way too fancy.
                  (assoc acc [(+ mx idx) (+ my idy)]
                         (style (nth m-line idx) color)))
                acc
                (range (count m-line))))
      acc
      (map-indexed (fn [idy m-line] [idy m-line]) char-seqs))))

(defn- build-loc->match-char
  [results invaders]
  (let [invader-type->color (invader-type->color invaders)]
    (reduce-kv
      (fn [acc invader-type matches]
        (let [color (get invader-type->color invader-type)]
          (loop [acc acc, matches matches]
            (if (seq matches)
              (recur (accumulate-matches acc matches color) (rest matches))
              acc))))
      {}
      results)))

(defn print-radar-sample-with-matches
  [invaders radar-sample results]
  (when (seq results)
    (println "Possible invaders on the radar sample:")
    (let [[width height] (t/text-dimensions radar-sample)
          char-seqs       (t/text-str->char-seqs radar-sample)
          loc->match-char (build-loc->match-char results invaders)]
      (doseq [idy (range height)]
        (println (str/join (map (fn [idx]
                                  (or (get loc->match-char [idx idy])
                                      (nth (nth char-seqs idy) idx)))
                                (range width))))))
    (println)))

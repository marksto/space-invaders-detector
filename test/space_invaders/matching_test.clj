(ns space-invaders.matching-test
  (:require [expectations.clojure.test
             :refer [approximately defexpect expect expecting more-of]]
            [space-invaders.matching :as sut]
            [space-invaders.text :as t]))

(defn ≈ [^double v]
  (approximately v 0.01))

;;

(defexpect validate-pattern-str-test
  (expecting "Valid pattern is returned as is"
    (doseq [valid-pattern [(t/str-vec->text-str ["---oo---"
                                                 "--oooo--"
                                                 "-oooooo-"
                                                 "oo-oo-oo"
                                                 "oooooooo"
                                                 "--o--o--"
                                                 "-o-oo-o-"
                                                 "o-o--o-o"])
                           (t/str-vec->text-str ["--o-----o--"
                                                 "---o---o---"
                                                 "--ooooooo--"
                                                 "-oo-ooo-oo-"
                                                 "ooooooooooo"
                                                 "o-ooooooo-o"
                                                 "o-o-----o-o"
                                                 "---oo-oo---"])]]
      (expect valid-pattern (sut/validate-pattern-str valid-pattern))))
  (expecting "Pattern must contain only valid characters"
    (expect (more-of {msg :message}
                     (expect #"^Pattern must contain only valid characters" msg))
            (sut/validate-pattern-str
              (t/str-vec->text-str ["--o-----o--"
                                    "---o---x---"
                                    "--ooooooo--"
                                    "-oo-ooo-oo-"
                                    "ooooooooooo"
                                    "o-ooooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]))))
  (expecting "Pattern lines have to be of the same length"
    (expect (more-of {msg :message}
                     (expect #"^Pattern lines have to be of the same length" msg))
            (sut/validate-pattern-str
              (t/str-vec->text-str ["--o-----o--"
                                    "---o---o-"
                                    "--ooooooo--"
                                    "-oo-ooo-oo-"
                                    "ooooooooooo"
                                    "o-ooooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"])))))

;;

(defexpect validate-input-str-test
  (let [max-invader-dims [11 8]]
    (expecting "Valid input string is returned as is"
      (doseq [valid-radar-sample [;; min size
                                  (t/str-vec->text-str ["-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"])
                                  ;; bigger width
                                  (t/str-vec->text-str ["-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"])
                                  ;; bigger height
                                  (t/str-vec->text-str ["-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"])]]
        (expect valid-radar-sample
                (sut/validate-input-str valid-radar-sample max-invader-dims))))
    (expecting "Input lines have to be of the same length"
      (expect (more-of {msg :message}
                       (expect #"^Input lines have to be of the same length" msg))
              (sut/validate-input-str
                (t/str-vec->text-str ["-----------"
                                      "----------"
                                      "-----------"
                                      "-----------"
                                      "---------"
                                      "-----------"
                                      "-----------"
                                      "-----------"])
                max-invader-dims)))
    (expecting "Input dimensions have to fit all patterns"
      (expect (more-of {msg :message}
                       (expect #"^Input dimensions have to fit all patterns" msg))
              (sut/validate-input-str
                (t/str-vec->text-str ["--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"])
                max-invader-dims)))))

;;

(defexpect find-matches:no-matches-test
  (expecting "No matches"
    (expect empty?
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "----------"
                                                    "----------"
                                                    "----------"
                                                    "----------"])))))

(defexpect find-matches:single-100%-match-test
  (expecting "A single full match with 100% accuracy"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [3 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---ooo----"
                                                    "---ooo----"
                                                    "---ooo----"
                                                    "----------"]))))
  (expecting "A single match on the TOP edge with 100% accuracy"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [3 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :top :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["---ooo----"
                                                    "---ooo----"
                                                    "----------"
                                                    "----------"
                                                    "----------"])
                              {:search-on-edges true})))
  (expecting "A single match on the LEFT edge with 100% accuracy"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [0 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :left :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["oo"
                                        "oo"
                                        "oo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "oo--------"
                                                    "oo--------"
                                                    "oo--------"
                                                    "----------"])
                              {:search-on-edges true})))
  (expecting "A single match on the TOP edge with 100% accuracy"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [3 4] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :bottom :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "----------"
                                                    "----------"
                                                    "----------"
                                                    "---ooo----"])
                              {:search-on-edges true})))
  (expecting "A single match on the RIGHT edge with 100% accuracy"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [9 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :right :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["o"
                                        "o"
                                        "o"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---------o"
                                                    "---------o"
                                                    "---------o"
                                                    "----------"])
                              {:search-on-edges true}))))

(defexpect find-matches:multiple-100%-matches:only-full-matches-test
  (expecting "Multiple full matches with 100% accuracy — no intersection"
    (expect (more-of [m1 m2 :as matches]
                     (expect 2 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [7 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m2))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooo---ooo"
                                                    "-ooo---ooo"
                                                    "-------ooo"
                                                    "----------"]))))
  (expecting "Multiple full matches with 100% accuracy — some intersection"
    (expect (more-of [m1 m2 :as matches]
                     (expect 2 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [3 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m2))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooooo----"
                                                    "-ooooo----"
                                                    "---ooo----"
                                                    "----------"]))))
  (expecting "Multiple full matches with 100% accuracy — more intersection"
    (expect (more-of [m1 m2 m3 :as matches]
                     (expect 3 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [3 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m2)
                     (expect (more-> [4 2] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m3))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooooo----"
                                                    "-oooooo---"
                                                    "---oooo---"
                                                    "----ooo---"])))))

(defexpect find-matches:multiple-100%-matches:on-edges-test
  (expecting "Multiple matches on the edges with 100% accuracy — no intersection"
    (expect (more-of [m1 m2 m3 :as matches]
                     (expect 3 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :top :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [0 3] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :bottom :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"]) :match/char-seqs)
                             m2)
                     (expect (more-> [8 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :right :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["oo"
                                        "oo"
                                        "oo"]) :match/char-seqs)
                             m3))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "--------oo"
                                                    "--------oo"
                                                    "ooo-----oo"
                                                    "ooo-------"])
                              {:search-on-edges true})))
  (expecting "Multiple matches on the edges with 100% accuracy — some intersection"
    (expect (more-of [m1 m2 m3 m4 m5 :as matches]
                     (expect 5 (count matches))
                     (expect (more-> [0 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :top :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :top :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"]) :match/char-seqs)
                             m2)
                     (expect (more-> [0 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :left :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["o"
                                        "o"
                                        "o"]) :match/char-seqs)
                             m3)
                     (expect (more-> [6 3] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :bottom :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"]) :match/char-seqs)
                             m4)
                     (expect (more-> [8 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     true :match/partial?
                                     :right :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["oo"
                                        "oo"
                                        "oo"]) :match/char-seqs)
                             m5))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["oooo------"
                                                    "o-------oo"
                                                    "o-------oo"
                                                    "------oooo"
                                                    "------ooo-"])
                              {:search-on-edges true})))
  ;; FIXME: A partial match duplicating a full one!
  (expecting "Multiple matches (all) with 100% accuracy — no intersection"
    (expect (more-of [m1 m2 :as matches]
                     (expect 2 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [7 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m2))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooo---ooo"
                                                    "-ooo---ooo"
                                                    "-------ooo"
                                                    "----------"])
                              {:search-on-edges true})))
  ;; FIXME: A partial match duplicating a full one!
  (expecting "Multiple matches (all) with 100% accuracy — some intersection"
    (expect (more-of [m1 m2 :as matches]
                     (expect 2 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [3 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m2))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooooo----"
                                                    "-ooooo----"
                                                    "---ooo----"
                                                    "----------"])
                              {:search-on-edges true})))
  ;; FIXME: A partial match duplicating a full one!
  (expecting "Multiple matches (all) with 100% accuracy — more intersection"
    (expect (more-of [m1 m2 m3 :as matches]
                     (expect 3 (count matches))
                     (expect (more-> [1 0] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [3 1] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m2)
                     (expect (more-> [4 2] :match/location
                                     0 :match/distance
                                     100.0 :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "ooo"
                                        "ooo"]) :match/char-seqs)
                             m3))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooooo----"
                                                    "-oooooo---"
                                                    "---oooo---"
                                                    "----ooo---"])
                              {:search-on-edges true}))))

(defexpect find-matches:partial-matches:lower-accuracy-test
  (expecting "Partial full matches with 80% accuracy"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [3 1] :match/location
                                     1 :match/distance
                                     (≈ 88.88) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "oxo"
                                        "ooo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---ooo----"
                                                    "---oxo----"
                                                    "---ooo----"
                                                    "----------"])
                              {:min-accuracy 80.0})))
  (expecting "Partial full matches with 50% accuracy"
    (expect (more-of [m1 m2 m3 m4 m5 :as matches]
                     (expect 5 (count matches))
                     (expect (more-> [3 0] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["---"
                                        "ooo"
                                        "oxo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [2 1] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["-oo"
                                        "-ox"
                                        "-oo"]) :match/char-seqs)
                             m2)
                     (expect (more-> [3 1] :match/location
                                     1 :match/distance
                                     (≈ 88.88) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "oxo"
                                        "ooo"]) :match/char-seqs)
                             m3)
                     (expect (more-> [4 1] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["oo-"
                                        "xo-"
                                        "oo-"]) :match/char-seqs)
                             m4)
                     (expect (more-> [3 2] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["oxo"
                                        "ooo"
                                        "---"]) :match/char-seqs)
                             m5))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---ooo----"
                                                    "---oxo----"
                                                    "---ooo----"
                                                    "----------"])
                              {:min-accuracy 50.0}))))

(defexpect find-matches:partial-matches:on-edges:lower-accuracy-test
  (expecting "All partial matches with 80% accuracy — almost exact match"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [3 1] :match/location
                                     1 :match/distance
                                     (≈ 88.88) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "oxo"
                                        "ooo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---ooo----"
                                                    "---oxo----"
                                                    "---ooo----"
                                                    "----------"])
                              {:search-on-edges true
                               :min-accuracy    80.0})))
  (expecting "All partial matches with 50% accuracy — almost exact match"
    (expect (more-of [m1 m2 m3 m4 m5 m6 m7 :as matches]
                     (expect 7 (count matches))
                     (expect (more-> [3 0] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["---"
                                        "ooo"
                                        "oxo"]) :match/char-seqs)
                             m1)
                     (expect (more-> [2 1] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["-oo"
                                        "-ox"
                                        "-oo"]) :match/char-seqs)
                             m2)
                     (expect (more-> [3 1] :match/location
                                     1 :match/distance
                                     (≈ 88.88) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "oxo"
                                        "ooo"]) :match/char-seqs)
                             m3)
                     (expect (more-> [4 1] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["oo-"
                                        "xo-"
                                        "oo-"]) :match/char-seqs)
                             m4)
                     (expect (more-> [3 2] :match/location
                                     4 :match/distance
                                     (≈ 55.55) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["oxo"
                                        "ooo"
                                        "---"]) :match/char-seqs)
                             m5)
                     (expect (more-> [3 0] :match/location
                                     3 :match/distance
                                     (≈ 50.0) :match/accuracy
                                     true :match/partial?
                                     :top :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["---"
                                        "ooo"]) :match/char-seqs)
                             m6)
                     (expect (more-> [3 3] :match/location
                                     3 :match/distance
                                     (≈ 50.0) :match/accuracy
                                     true :match/partial?
                                     :bottom :match/edge-kind
                                     (t/str-vec->char-seqs
                                       ["ooo"
                                        "---"]) :match/char-seqs)
                             m7))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---ooo----"
                                                    "---oxo----"
                                                    "---ooo----"
                                                    "----------"])
                              {:search-on-edges true
                               :min-accuracy    50.0})))
  (expecting "All partial matches with 50% accuracy — non-exact (poor) match"
    (expect (more-of [m1 :as matches]
                     (expect 1 (count matches))
                     (expect (more-> [3 1] :match/location
                                     3 :match/distance
                                     (≈ 66.66) :match/accuracy
                                     (t/str-vec->char-seqs
                                       ["oox"
                                        "oxo"
                                        "xoo"]) :match/char-seqs)
                             m1))
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---oox----"
                                                    "---oxo----"
                                                    "---xoo----"
                                                    "----------"])
                              {:search-on-edges true
                               :min-accuracy    50.0}))))

;; TODO: Add tests that check that the `:min-sub-pattern` option works!

;;

(comment
  (expectations.clojure.test/run-tests)
  .)

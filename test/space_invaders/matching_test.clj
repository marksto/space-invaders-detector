(ns space-invaders.matching-test
  (:require [expectations.clojure.test
             :refer [defexpect expect expecting more-of more->]]
            [space-invaders.matching :as sut]
            [space-invaders.text :as t]
            [space-invaders.test-utils :refer [≈]]))

(defexpect validate-pattern-str-test
  (let [valid-pattern-chars #{\- \o}]
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
        (expect valid-pattern (sut/validate-pattern-str valid-pattern
                                                        valid-pattern-chars))))
    (expecting "Pattern must contain only valid characters"
      (expect (more-of {msg :message}
                #"^Pattern must contain only valid characters" msg)
              (sut/validate-pattern-str
                (t/str-vec->text-str ["--o-----o--"
                                      "---o---x---"
                                      "--ooooooo--"
                                      "-oo-ooo-oo-"
                                      "ooooooooooo"
                                      "o-ooooooo-o"
                                      "o-o-----o-o"
                                      "---oo-oo---"])
                valid-pattern-chars)))
    (expecting "Pattern lines have to be of the same length"
      (expect (more-of {msg :message}
                #"^Pattern lines have to be of the same length" msg)
              (sut/validate-pattern-str
                (t/str-vec->text-str ["--o-----o--"
                                      "---o---o-"
                                      "--ooooooo--"
                                      "-oo-ooo-oo-"
                                      "ooooooooooo"
                                      "o-ooooooo-o"
                                      "o-o-----o-o"
                                      "---oo-oo---"])
                valid-pattern-chars)))))

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
                       #"^Input lines have to be of the same length" msg)
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
                       #"^Input dimensions have to fit all patterns" msg)
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
                     1 (count matches)
                     (more-> [3 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1)
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
                     1 (count matches)
                     (more-> [3 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :top :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"]) :match/char-seqs) m1)
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
                     1 (count matches)
                     (more-> [0 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :left :match/edge-kind
                             (t/str-vec->char-seqs
                               ["oo"
                                "oo"
                                "oo"]) :match/char-seqs) m1)
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
                     1 (count matches)
                     (more-> [3 4] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :bottom :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"]) :match/char-seqs) m1)
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
                     1 (count matches)
                     (more-> [9 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :right :match/edge-kind
                             (t/str-vec->char-seqs
                               ["o"
                                "o"
                                "o"]) :match/char-seqs) m1)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["----------"
                                                    "---------o"
                                                    "---------o"
                                                    "---------o"
                                                    "----------"])
                              {:search-on-edges true}))))

(defexpect find-matches:multiple-100%-matches:only-full-test
  (expecting "Multiple full matches with 100% accuracy — no intersection"
    (expect (more-of [m1 m2 :as matches]
                     2 (count matches)
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1
                     (more-> [7 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m2)
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
                     2 (count matches)
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1
                     (more-> [3 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m2)
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
                     3 (count matches)
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1
                     (more-> [3 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m2
                     (more-> [4 2] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m3)
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
                     3 (count matches)
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :top :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"]) :match/char-seqs) m1
                     (more-> [0 3] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :bottom :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"]) :match/char-seqs) m2
                     (more-> [8 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :right :match/edge-kind
                             (t/str-vec->char-seqs
                               ["oo"
                                "oo"
                                "oo"]) :match/char-seqs) m3)
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
                     5 (count matches)
                     (more-> [0 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :top :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"]) :match/char-seqs) m1
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :top :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"]) :match/char-seqs) m2
                     (more-> [0 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :left :match/edge-kind
                             (t/str-vec->char-seqs
                               ["o"
                                "o"
                                "o"]) :match/char-seqs) m3
                     (more-> [6 3] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :bottom :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"]) :match/char-seqs) m4
                     (more-> [8 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :right :match/edge-kind
                             (t/str-vec->char-seqs
                               ["oo"
                                "oo"
                                "oo"]) :match/char-seqs) m5)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["oooo------"
                                                    "o-------oo"
                                                    "o-------oo"
                                                    "------oooo"
                                                    "------ooo-"])
                              {:search-on-edges true}))))

(defexpect find-matches:no-duplicating-edge-matches-test
  (expecting "No edge matches duplicating a full one — no intersection"
    (expect (more-of [m1 m2 :as matches]
                     2 (count matches)
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1
                     (more-> [7 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m2)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooo---ooo"
                                                    "-ooo---ooo"
                                                    "-------ooo"
                                                    "----------"])
                              {:search-on-edges true})))
  (expecting "No edge matches duplicating a full one — some intersection"
    (expect (more-of [m1 m2 :as matches]
                     2 (count matches)
                     (more-> [1 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1
                     (more-> [3 2] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m2)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["-ooo------"
                                                    "-ooo------"
                                                    "-ooooo----"
                                                    "---ooo----"
                                                    "---ooo----"])
                              {:search-on-edges true})))
  (expecting "No edge matches duplicating a full one — more intersection"
    (expect (more-of [m1 m2 m3 :as matches]
                     3 (count matches)
                     (more-> [3 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m1
                     (more-> [7 1] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m2
                     (more-> [5 2] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"
                                "ooo"]) :match/char-seqs) m3)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["---ooo----"
                                                    "---ooo-ooo"
                                                    "---ooooooo"
                                                    "-----ooooo"
                                                    "-----ooo--"])
                              {:search-on-edges true})))
  (expecting "No edge matches duplicating a full one — w/ lower accuracy"
    (expect empty?
            (filter :match/edge-kind
                    (sut/find-matches (t/str-vec->text-str ["ooo"
                                                            "ooo"
                                                            "ooo"])
                                      (t/str-vec->text-str ["---ooo----"
                                                            "---ooo-ooo"
                                                            "---ooooooo"
                                                            "-----ooooo"
                                                            "-----ooo--"])
                                      {:min-accuracy    60.0
                                       :search-on-edges true})))))

(defexpect find-matches:partial-matches:only-full:lower-accuracy-test
  (expecting "Partial full matches with 80% accuracy"
    (expect (more-of [m1 :as matches]
                     1 (count matches)
                     (more-> [3 1] :match/location
                             1 :match/distance
                             (≈ 88.88) :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "oxo"
                                "ooo"]) :match/char-seqs) m1)
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
                     5 (count matches)
                     (more-> [3 0] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["---"
                                "ooo"
                                "oxo"]) :match/char-seqs) m1
                     (more-> [2 1] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["-oo"
                                "-ox"
                                "-oo"]) :match/char-seqs) m2
                     (more-> [3 1] :match/location
                             1 :match/distance
                             (≈ 88.88) :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "oxo"
                                "ooo"]) :match/char-seqs) m3
                     (more-> [4 1] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["oo-"
                                "xo-"
                                "oo-"]) :match/char-seqs) m4
                     (more-> [3 2] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["oxo"
                                "ooo"
                                "---"]) :match/char-seqs) m5)
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
                     1 (count matches)
                     (more-> [3 1] :match/location
                             1 :match/distance
                             (≈ 88.88) :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "oxo"
                                "ooo"]) :match/char-seqs) m1)
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
    (expect (more-of [m1 m2 m3 m4 m5 :as matches]
                     5 (count matches)
                     (more-> [3 0] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["---"
                                "ooo"
                                "oxo"]) :match/char-seqs) m1
                     (more-> [2 1] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["-oo"
                                "-ox"
                                "-oo"]) :match/char-seqs) m2
                     (more-> [3 1] :match/location
                             1 :match/distance
                             (≈ 88.88) :match/accuracy
                             (t/str-vec->char-seqs
                               ["ooo"
                                "oxo"
                                "ooo"]) :match/char-seqs) m3
                     (more-> [4 1] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["oo-"
                                "xo-"
                                "oo-"]) :match/char-seqs) m4
                     (more-> [3 2] :match/location
                             4 :match/distance
                             (≈ 55.55) :match/accuracy
                             (t/str-vec->char-seqs
                               ["oxo"
                                "ooo"
                                "---"]) :match/char-seqs) m5)
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
                     1 (count matches)
                     (more-> [3 1] :match/location
                             3 :match/distance
                             (≈ 66.66) :match/accuracy
                             (t/str-vec->char-seqs
                               ["oox"
                                "oxo"
                                "xoo"]) :match/char-seqs) m1)
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

(defexpect find-matches:on-edges:min-sub-pattern-option-test
  (expecting "The `:min-sub-pattern` option works as a cut-off"
    (expect (more-of [m1 :as matches]
                     1 (count matches)
                     (more-> [3 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :top :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"]) :match/char-seqs) m1)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["---ooo----"
                                                    "---ooo----"
                                                    "----------"
                                                    "----------"
                                                    "----------"])
                              {:search-on-edges true
                               :min-sub-pattern 1}))
    (expect (more-of [m1 :as matches]
                     1 (count matches)
                     (more-> [3 0] :match/location
                             0 :match/distance
                             100.0 :match/accuracy
                             :top :match/edge-kind
                             (t/str-vec->char-seqs
                               ["ooo"
                                "ooo"]) :match/char-seqs) m1)
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["---ooo----"
                                                    "---ooo----"
                                                    "----------"
                                                    "----------"
                                                    "----------"])
                              {:search-on-edges true
                               :min-sub-pattern 2}))
    (expect empty?
            (sut/find-matches (t/str-vec->text-str ["ooo"
                                                    "ooo"
                                                    "ooo"])
                              (t/str-vec->text-str ["---ooo----"
                                                    "---ooo----"
                                                    "----------"
                                                    "----------"
                                                    "----------"])
                              {:search-on-edges true
                               :min-sub-pattern 3}))))

;;

(comment
  (expectations.clojure.test/run-tests)
  .)

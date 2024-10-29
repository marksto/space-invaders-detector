(ns space-invaders.core-test
  (:require [clojure.string :as str]
            [expectations.clojure.test
             :refer [approximately defexpect expect expecting in more-> more-of]]
            [space-invaders.core :as sut]))

(defn ≈ [^double v]
  (approximately v 0.01))

(defn char-seqs [str-vec]
  (map seq str-vec))

(defn ->text-str [str-vec]
  (str/join \newline str-vec))

;;

(defexpect validate-pattern-str-test
  (expecting "Task test data patterns are valid"
    (doseq [valid-pattern (map :invader/pattern sut/invaders)]
      (expect nil? (sut/validate-pattern-str valid-pattern))))
  (expecting "Pattern must contain only valid characters"
    (expect {:error/msg "Pattern must contain only valid characters"}
            (in (sut/validate-pattern-str
                  (->text-str ["--o-----o--"
                               "---o---x---"
                               "--ooooooo--"
                               "-oo-ooo-oo-"
                               "ooooooooooo"
                               "o-ooooooo-o"
                               "o-o-----o-o"
                               "---oo-oo---"])))))
  (expecting "Pattern lines have to be of the same length"
    (expect {:error/msg "Pattern lines have to be of the same length"}
            (in (sut/validate-pattern-str
                  (->text-str ["--o-----o--"
                               "---o---o-"
                               "--ooooooo--"
                               "-oo-ooo-oo-"
                               "ooooooooooo"
                               "o-ooooooo-o"
                               "o-o-----o-o"
                               "---oo-oo---"]))))))

(defexpect validate-input-str-test
  (let [max-invader-dims (sut/max-invader-dims sut/invaders)]
    (expecting "Task test data input is valid"
      (expect nil? (sut/validate-input-str sut/radar-sample
                                           max-invader-dims)))
    (expecting "Pattern must contain only valid characters"
      (expect {:error/msg "Input lines have to be of the same length"}
              (in (sut/validate-input-str
                    (->text-str ["--------"
                                 "-------"
                                 "--------"
                                 "--------"
                                 "------"
                                 "--------"
                                 "--------"
                                 "--------"])
                    max-invader-dims))))
    (expecting "Input dimension must be equal to or bigger than the minimal"
      (expect {:error/msg  "Input dimension must be equal to or bigger than the minimal"
               :error/data {:input-dims [8 8]
                            :min-dims   max-invader-dims}}
              (in (sut/validate-input-str
                    (->text-str ["--------"
                                 "--------"
                                 "--------"
                                 "--------"
                                 "--------"
                                 "--------"
                                 "--------"
                                 "--------"])
                    max-invader-dims))))))

;;

(defexpect find-invaders:single-exact-match-test
  (expecting "An exact match is found with 100% accuracy — Squid"
    (let [squid-invader-pattern (sut/read-text-file "invaders/squid.txt")]
      (expect (more->
                #{:invader.type/squid} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [0 0] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         (char-seqs ["---oo---"
                                                     "--oooo--"
                                                     "-oooooo-"
                                                     "oo-oo-oo"
                                                     "oooooooo"
                                                     "--o--o--"
                                                     "-o-oo-o-"
                                                     "o-o--o-o"]) :match/char-seqs)
                                 m1))
                :invader.type/squid)
              (sut/find-invaders [{:invader/type    :invader.type/squid
                                   :invader/pattern squid-invader-pattern}]
                                 squid-invader-pattern
                                 {}))))
  (expecting "An exact match is found with 100% accuracy — Crab"
    (let [crab-invader-pattern (sut/read-text-file "invaders/crab.txt")]
      (expect (more->
                #{:invader.type/crab} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [0 0] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         (char-seqs ["--o-----o--"
                                                     "---o---o---"
                                                     "--ooooooo--"
                                                     "-oo-ooo-oo-"
                                                     "ooooooooooo"
                                                     "o-ooooooo-o"
                                                     "o-o-----o-o"
                                                     "---oo-oo---"]) :match/char-seqs)
                                 m1))
                :invader.type/crab)
              (sut/find-invaders [{:invader/type    :invader.type/crab
                                   :invader/pattern crab-invader-pattern}]
                                 crab-invader-pattern
                                 {}))))
  (expecting "An exact match is found with 100% accuracy — UFO"
    (let [ufo-invader-pattern (sut/read-text-file "invaders/ufo.txt")]
      (expect (more->
                #{:invader.type/ufo} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [0 0] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         (char-seqs ["-----oooooo-----"
                                                     "---oooooooooo---"
                                                     "--oooooooooooo--"
                                                     "-oo-oo-oo-oo-oo-"
                                                     "oooooooooooooooo"
                                                     "--ooo--oo--ooo--"
                                                     "---o--------o---"]) :match/char-seqs)
                                 m1))
                :invader.type/ufo)
              (sut/find-invaders [{:invader/type    :invader.type/ufo
                                   :invader/pattern ufo-invader-pattern}]
                                 ufo-invader-pattern
                                 {})))))

(defexpect find-invaders:single-matches-on-edges-test
  (expecting "Exact match on the TOP edge is found with 100% accuracy — Crab"
    (let [crab-invader-pattern (sut/read-text-file "invaders/crab.txt")
          radar-sample         (sut/read-text-file "radar_samples/crab-top-3.txt")]
      (expect (more->
                #{:invader.type/crab} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [0 0] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         true :match/partial?
                                         :top :match/edge-kind
                                         (char-seqs ["o-ooooooo-o"
                                                     "o-o-----o-o"
                                                     "---oo-oo---"]) :match/char-seqs)
                                 m1))
                :invader.type/crab)
              (sut/find-invaders [{:invader/type    :invader.type/crab
                                   :invader/pattern crab-invader-pattern}]
                                 radar-sample
                                 {:sensitivity 100.0
                                  :edges       true}))))
  (expecting "Exact match on the LEFT edge is found with 100% accuracy — Crab"
    (let [crab-invader-pattern (sut/read-text-file "invaders/crab.txt")
          radar-sample         (sut/read-text-file "radar_samples/crab-left-2.txt")]
      (expect (more->
                #{:invader.type/crab} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [0 0] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         true :match/partial?
                                         :left :match/edge-kind
                                         (char-seqs ["--"
                                                     "--"
                                                     "--"
                                                     "o-"
                                                     "oo"
                                                     "-o"
                                                     "-o"
                                                     "--"]) :match/char-seqs)
                                 m1))
                :invader.type/crab)
              (sut/find-invaders [{:invader/type    :invader.type/crab
                                   :invader/pattern crab-invader-pattern}]
                                 radar-sample
                                 {:sensitivity 100.0
                                  :edges       true}))))
  (expecting "Exact match on the BOTTOM edge is found with 100% accuracy — Crab"
    (let [crab-invader-pattern (sut/read-text-file "invaders/crab.txt")
          radar-sample         (sut/read-text-file "radar_samples/crab-bottom-1.txt")]
      (expect (more->
                #{:invader.type/crab} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [0 7] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         true :match/partial?
                                         :bottom :match/edge-kind
                                         (char-seqs ["--o-----o--"]) :match/char-seqs)
                                 m1))
                :invader.type/crab)
              (sut/find-invaders [{:invader/type    :invader.type/crab
                                   :invader/pattern crab-invader-pattern}]
                                 radar-sample
                                 {:sensitivity 100.0
                                  :edges       true}))))
  (expecting "Exact match on the RIGHT edge is found with 100% accuracy — Crab"
    (let [crab-invader-pattern (sut/read-text-file "invaders/crab.txt")
          radar-sample         (sut/read-text-file "radar_samples/crab-right-1.txt")]
      (expect (more->
                #{:invader.type/crab} (-> keys set)
                (more-of [m1 :as matches]
                         (expect 1 (count matches))
                         (expect (more-> [10 0] :match/location
                                         0 :match/distance
                                         100.0 :match/accuracy
                                         true :match/partial?
                                         :right :match/edge-kind
                                         (char-seqs ["-"
                                                     "-"
                                                     "-"
                                                     "-"
                                                     "o"
                                                     "o"
                                                     "o"
                                                     "-"]) :match/char-seqs)
                                 m1))
                :invader.type/crab)
              (sut/find-invaders [{:invader/type    :invader.type/crab
                                   :invader/pattern crab-invader-pattern}]
                                 radar-sample
                                 {:sensitivity 100.0
                                  :edges       true})))))

;;

(defexpect find-invaders:only-full-matches-test
  (expecting "Full matches at 99.8% sensitivity — Squid & Crab (task test data)"
    (expect (more->
              #{:invader.type/squid :invader.type/crab} (-> keys set)

              (more-of [s1 s2 s3 s4 :as squids]
                       (expect 4 (count squids))
                       (expect (more-> [42 0] :match/location
                                       (≈ 99.88) :match/accuracy
                                       (char-seqs ["---oo---"
                                                   "--ooo-o-"
                                                   "--ooooo-"
                                                   "oo--o-oo"
                                                   "oo-ooooo"
                                                   "-----o--"
                                                   "oo-oo-o-"
                                                   "o-o--ooo"]) :match/char-seqs)
                               s1)
                       (expect (more-> [35 15] :match/location
                                       (≈ 99.84) :match/accuracy
                                       (char-seqs ["---oo---"
                                                   "--oooo--"
                                                   "oooooooo"
                                                   "oo-oo--o"
                                                   "-ooooooo"
                                                   "--o--ooo"
                                                   "-o-oo---"
                                                   "oo--oo-o"]) :match/char-seqs)
                               s2)
                       (expect (more-> [16 28] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seqs ["---o-o--"
                                                   "-ooooo--"
                                                   "-oooooo-"
                                                   "o--oo-oo"
                                                   "oooooooo"
                                                   "-ooo-o--"
                                                   "--ooo-o-"
                                                   "o-o--ooo"]) :match/char-seqs)
                               s3)
                       (expect (more-> [82 41] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seqs ["---oo---"
                                                   "--ooooo-"
                                                   "-oo-ooo-"
                                                   "oo-o-ooo"
                                                   "o-oooooo"
                                                   "--o--o--"
                                                   "oo-oo-o-"
                                                   "--oooo-o"]) :match/char-seqs)
                               s4))
              :invader.type/squid

              (more-of [c1 c2 c3 :as crabs]
                       (expect 3 (count crabs))
                       (expect (more-> [74 1] :match/location
                                       (≈ 99.88) :match/accuracy
                                       (char-seqs ["ooo-----o--"
                                                   "o--o-o-o---"
                                                   "--o-ooooo--"
                                                   "oo--ooo-oo-"
                                                   "ooooooo-ooo"
                                                   "oooo--ooo-o"
                                                   "o-o-----o-o"
                                                   "---oo-oo---"]) :match/char-seqs)
                               c1)
                       (expect (more-> [85 12] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seqs ["--oo----o--"
                                                   "-------o---"
                                                   "o--oooooo--"
                                                   "-oo--oo--o-"
                                                   "oo-oooooooo"
                                                   "o-ooooooo-o"
                                                   "oo-o----o-o"
                                                   "--ooo-oo--o"]) :match/char-seqs)
                               c2)
                       (expect (more-> [60 13] :match/location
                                       (≈ 99.91) :match/accuracy
                                       (char-seqs ["--o-----o--"
                                                   "-------o---"
                                                   "--oooo-oo--"
                                                   "----ooo-oo-"
                                                   "o--oooooo-o"
                                                   "o-o-ooooo-o"
                                                   "o-o-----o-o"
                                                   "---oo-oo---"]) :match/char-seqs)
                               c3))
              :invader.type/crab)

            (sut/find-invaders sut/invaders sut/radar-sample)))

  (expecting "Full matches at 99.7% sensitivity — Squid & Crab (task test data)"
    (expect (more->
              #{:invader.type/squid :invader.type/crab} (-> keys set)

              (more-of [s1 s2 s3 s4 s5 s6 s7 s8 :as squids]
                       (expect 8 (count squids))
                       (expect (more-> [42 0] :match/location
                                       (≈ 99.88) :match/accuracy
                                       (char-seqs ["---oo---"
                                                   "--ooo-o-"
                                                   "--ooooo-"
                                                   "oo--o-oo"
                                                   "oo-ooooo"
                                                   "-----o--"
                                                   "oo-oo-o-"
                                                   "o-o--ooo"]) :match/char-seqs)
                               s1)
                       (expect (more-> [77 2] :match/location
                                       (≈ 99.72) :match/accuracy
                                       (char-seqs ["o-o-o---"
                                                   "-ooooo--"
                                                   "-ooo-oo-"
                                                   "oooo-ooo"
                                                   "o--ooo-o"
                                                   "-----o-o"
                                                   "oo-oo---"
                                                   "o-o---o-"]) :match/char-seqs)
                               s2)
                       (expect (more-> [60 14] :match/location
                                       (≈ 99.72) :match/accuracy
                                       (char-seqs ["-------o"
                                                   "--oooo-o"
                                                   "----ooo-"
                                                   "o--ooooo"
                                                   "o-o-oooo"
                                                   "o-o-----"
                                                   "---oo-oo"
                                                   "-------o"]) :match/char-seqs)
                               s3)
                       (expect (more-> [63 14] :match/location
                                       (≈ 99.70) :match/accuracy
                                       (char-seqs ["----o---"
                                                   "ooo-oo--"
                                                   "-ooo-oo-"
                                                   "oooooo-o"
                                                   "-ooooo-o"
                                                   "-----o-o"
                                                   "oo-oo---"
                                                   "----o---"]) :match/char-seqs)
                               s4)
                       (expect (more-> [35 15] :match/location
                                       (≈ 99.84) :match/accuracy
                                       (char-seqs ["---oo---"
                                                   "--oooo--"
                                                   "oooooooo"
                                                   "oo-oo--o"
                                                   "-ooooooo"
                                                   "--o--ooo"
                                                   "-o-oo---"
                                                   "oo--oo-o"]) :match/char-seqs)
                               s5)
                       (expect (more-> [16 28] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seqs ["---o-o--"
                                                   "-ooooo--"
                                                   "-oooooo-"
                                                   "o--oo-oo"
                                                   "oooooooo"
                                                   "-ooo-o--"
                                                   "--ooo-o-"
                                                   "o-o--ooo"]) :match/char-seqs)
                               s6)
                       (expect (more-> [83 40] :match/location
                                       (≈ 99.70) :match/accuracy
                                       (char-seqs ["--------"
                                                   "--oo----"
                                                   "-ooooo--"
                                                   "oo-ooo--"
                                                   "o-o-ooo-"
                                                   "-oooooo-"
                                                   "-o--o---"
                                                   "o-oo-o--"]) :match/char-seqs)
                               s7)
                       (expect (more-> [82 41] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seqs ["---oo---"
                                                   "--ooooo-"
                                                   "-oo-ooo-"
                                                   "oo-o-ooo"
                                                   "o-oooooo"
                                                   "--o--o--"
                                                   "oo-oo-o-"
                                                   "--oooo-o"]) :match/char-seqs)
                               s8))
              :invader.type/squid

              (more-of [c1 c2 c3 c4 :as crabs]
                       (expect 4 (count crabs))
                       (expect (more-> [74 1] :match/location
                                       (≈ 99.88) :match/accuracy
                                       (char-seqs ["ooo-----o--"
                                                   "o--o-o-o---"
                                                   "--o-ooooo--"
                                                   "oo--ooo-oo-"
                                                   "ooooooo-ooo"
                                                   "oooo--ooo-o"
                                                   "o-o-----o-o"
                                                   "---oo-oo---"]) :match/char-seqs)
                               c1)
                       (expect (more-> [85 12] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seqs ["--oo----o--"
                                                   "-------o---"
                                                   "o--oooooo--"
                                                   "-oo--oo--o-"
                                                   "oo-oooooooo"
                                                   "o-ooooooo-o"
                                                   "oo-o----o-o"
                                                   "--ooo-oo--o"]) :match/char-seqs)
                               c2)
                       (expect (more-> [60 13] :match/location
                                       (≈ 99.91) :match/accuracy
                                       (char-seqs ["--o-----o--"
                                                   "-------o---"
                                                   "--oooo-oo--"
                                                   "----ooo-oo-"
                                                   "o--oooooo-o"
                                                   "o-o-ooooo-o"
                                                   "o-o-----o-o"
                                                   "---oo-oo---"]) :match/char-seqs)
                               c3)
                       (expect (more-> [82 40] :match/location
                                       (≈ 99.74) :match/accuracy
                                       (char-seqs ["-----------"
                                                   "---oo------"
                                                   "--ooooo----"
                                                   "-oo-ooo----"
                                                   "oo-o-ooo---"
                                                   "o-oooooo---"
                                                   "--o--o-----"
                                                   "oo-oo-o--o-"]) :match/char-seqs)
                               c4))
              :invader.type/crab)

            (sut/find-invaders sut/invaders sut/radar-sample {:sensitivity 99.7}))))

;;

(comment
  (expectations.clojure.test/run-tests)
  .)

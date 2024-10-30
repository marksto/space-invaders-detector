(ns space-invaders.domain-test
  (:require [expectations.clojure.test
             :refer [approximately defexpect expect expecting more-of more->]]
            [space-invaders.domain :as sut]
            [space-invaders.text :as t]))

(defn ≈ [^double v]
  (approximately v 0.01))

;;

(defexpect find-invaders:single-exact-match-test
  (expecting "An exact match is found with 100% accuracy — Squid"
    (let [[squid-invader] (sut/build-invaders ["space_invaders/invaders/squid.txt"])
          squid-invader-pattern (:invader/pattern squid-invader)]
      (expect (more->
                #{:squid} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [0 0] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--oooo--"
                                    "-oooooo-"
                                    "oo-oo-oo"
                                    "oooooooo"
                                    "--o--o--"
                                    "-o-oo-o-"
                                    "o-o--o-o"]) :match/char-seqs) m1)
                :squid)
              (sut/find-invaders [squid-invader] squid-invader-pattern))))
  (expecting "An exact match is found with 100% accuracy — Crab"
    (let [[crab-invader] (sut/build-invaders ["space_invaders/invaders/crab.txt"])
          crab-invader-pattern (:invader/pattern crab-invader)]
      (expect (more->
                #{:crab} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [0 0] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--o-----o--"
                                    "---o---o---"
                                    "--ooooooo--"
                                    "-oo-ooo-oo-"
                                    "ooooooooooo"
                                    "o-ooooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) m1)
                :crab)
              (sut/find-invaders [crab-invader] crab-invader-pattern))))
  (expecting "An exact match is found with 100% accuracy — UFO"
    (let [[ufo-invader] (sut/build-invaders ["space_invaders/invaders/ufo.txt"])
          ufo-invader-pattern (:invader/pattern ufo-invader)]
      (expect (more->
                #{:ufo} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [0 0] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["-----oooooo-----"
                                    "---oooooooooo---"
                                    "--oooooooooooo--"
                                    "-oo-oo-oo-oo-oo-"
                                    "oooooooooooooooo"
                                    "--ooo--oo--ooo--"
                                    "---o--------o---"]) :match/char-seqs) m1)
                :ufo)
              (sut/find-invaders [ufo-invader] ufo-invader-pattern)))))

(defexpect find-invaders:single-matches-on-edges-test
  (expecting "Exact match on the TOP edge is found with 100% accuracy — Crab"
    (let [invaders     (sut/build-invaders ["space_invaders/invaders/crab.txt"])
          radar-sample (sut/build-radar-sample
                         "space_invaders/radar_samples/crab-top-3.txt" invaders)]
      (expect (more->
                #{:crab} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [0 0] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 true :match/partial?
                                 :top :match/edge-kind
                                 (t/str-vec->char-seqs
                                   ["o-ooooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) m1)
                :crab)
              (sut/find-invaders invaders radar-sample
                                 {:sensitivity 100.0
                                  :edges       true}))))
  (expecting "Exact match on the LEFT edge is found with 100% accuracy — Crab"
    (let [invaders     (sut/build-invaders ["space_invaders/invaders/crab.txt"])
          radar-sample (sut/build-radar-sample
                         "space_invaders/radar_samples/crab-left-2.txt" invaders)]
      (expect (more->
                #{:crab} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [0 0] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 true :match/partial?
                                 :left :match/edge-kind
                                 (t/str-vec->char-seqs
                                   ["--"
                                    "--"
                                    "--"
                                    "o-"
                                    "oo"
                                    "-o"
                                    "-o"
                                    "--"]) :match/char-seqs) m1)
                :crab)
              (sut/find-invaders invaders radar-sample
                                 {:sensitivity 100.0
                                  :edges       true}))))
  (expecting "Exact match on the BOTTOM edge is found with 100% accuracy — Crab"
    (let [invaders     (sut/build-invaders ["space_invaders/invaders/crab.txt"])
          radar-sample (sut/build-radar-sample
                         "space_invaders/radar_samples/crab-bottom-1.txt" invaders)]
      (expect (more->
                #{:crab} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [0 7] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 true :match/partial?
                                 :bottom :match/edge-kind
                                 (t/str-vec->char-seqs
                                   ["--o-----o--"]) :match/char-seqs) m1)
                :crab)
              (sut/find-invaders invaders radar-sample
                                 {:sensitivity 100.0
                                  :edges       true}))))
  (expecting "Exact match on the RIGHT edge is found with 100% accuracy — Crab"
    (let [invaders     (sut/build-invaders ["space_invaders/invaders/crab.txt"])
          radar-sample (sut/build-radar-sample
                         "space_invaders/radar_samples/crab-right-1.txt" invaders)]
      (expect (more->
                #{:crab} (-> keys set)
                (more-of [m1 :as matches]
                         1 (count matches)
                         (more-> [10 0] :match/location
                                 0 :match/distance
                                 100.0 :match/accuracy
                                 true :match/partial?
                                 :right :match/edge-kind
                                 (t/str-vec->char-seqs
                                   ["-"
                                    "-"
                                    "-"
                                    "-"
                                    "o"
                                    "o"
                                    "o"
                                    "-"]) :match/char-seqs) m1)
                :crab)
              (sut/find-invaders invaders radar-sample
                                 {:sensitivity 100.0
                                  :edges       true})))))

;;

(defexpect find-invaders:only-full-matches:default-sensitivity-test
  (expecting "FULL matches at default sensitivity — Squid & Crab (example data)"
    (let [invaders     (sut/build-invaders)
          radar-sample (sut/build-radar-sample invaders)]
      (expect (more->
                #{:squid :crab} (-> keys set)

                (more-of [s1 s2 s3 s4 :as squids]
                         4 (count squids)
                         (more-> [42 0] :match/location
                                 (≈ 87.5) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--ooo-o-"
                                    "--ooooo-"
                                    "oo--o-oo"
                                    "oo-ooooo"
                                    "-----o--"
                                    "oo-oo-o-"
                                    "o-o--ooo"]) :match/char-seqs) s1
                         (more-> [35 15] :match/location
                                 (≈ 84.38) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--oooo--"
                                    "oooooooo"
                                    "oo-oo--o"
                                    "-ooooooo"
                                    "--o--ooo"
                                    "-o-oo---"
                                    "oo--oo-o"]) :match/char-seqs) s2
                         (more-> [16 28] :match/location
                                 (≈ 85.94) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---o-o--"
                                    "-ooooo--"
                                    "-oooooo-"
                                    "o--oo-oo"
                                    "oooooooo"
                                    "-ooo-o--"
                                    "--ooo-o-"
                                    "o-o--ooo"]) :match/char-seqs) s3
                         (more-> [82 41] :match/location
                                 (≈ 85.94) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--ooooo-"
                                    "-oo-ooo-"
                                    "oo-o-ooo"
                                    "o-oooooo"
                                    "--o--o--"
                                    "oo-oo-o-"
                                    "--oooo-o"]) :match/char-seqs) s4)
                :squid

                (more-of [c1 c2 c3 :as crabs]
                         3 (count crabs)
                         (more-> [74 1] :match/location
                                 (≈ 87.5) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["ooo-----o--"
                                    "o--o-o-o---"
                                    "--o-ooooo--"
                                    "oo--ooo-oo-"
                                    "ooooooo-ooo"
                                    "oooo--ooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) c1
                         (more-> [85 12] :match/location
                                 (≈ 86.36) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--oo----o--"
                                    "-------o---"
                                    "o--oooooo--"
                                    "-oo--oo--o-"
                                    "oo-oooooooo"
                                    "o-ooooooo-o"
                                    "oo-o----o-o"
                                    "--ooo-oo--o"]) :match/char-seqs) c2
                         (more-> [60 13] :match/location
                                 (≈ 90.9) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--o-----o--"
                                    "-------o---"
                                    "--oooo-oo--"
                                    "----ooo-oo-"
                                    "o--oooooo-o"
                                    "o-o-ooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) c3)
                :crab)

              (sut/find-invaders invaders radar-sample
                                 {:sensitivity 80.0})))))

(defexpect find-invaders:only-full-matches:lower-sensitivity-test
  (expecting "FULL matches at a lower sensitivity — Squid & Crab (example data)"
    (let [invaders     (sut/build-invaders)
          radar-sample (sut/build-radar-sample invaders)]
      (expect (more->
                #{:squid :crab} (-> keys set)

                (more-of [s1 s2 s3 s4 s5 s6 s7 s8 :as squids]
                         8 (count squids)
                         (more-> [42 0] :match/location
                                 (≈ 87.5) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--ooo-o-"
                                    "--ooooo-"
                                    "oo--o-oo"
                                    "oo-ooooo"
                                    "-----o--"
                                    "oo-oo-o-"
                                    "o-o--ooo"]) :match/char-seqs) s1
                         (more-> [77 2] :match/location
                                 (≈ 71.88) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["o-o-o---"
                                    "-ooooo--"
                                    "-ooo-oo-"
                                    "oooo-ooo"
                                    "o--ooo-o"
                                    "-----o-o"
                                    "oo-oo---"
                                    "o-o---o-"]) :match/char-seqs) s2
                         (more-> [60 14] :match/location
                                 (≈ 71.88) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["-------o"
                                    "--oooo-o"
                                    "----ooo-"
                                    "o--ooooo"
                                    "o-o-oooo"
                                    "o-o-----"
                                    "---oo-oo"
                                    "-------o"]) :match/char-seqs) s3
                         (more-> [63 14] :match/location
                                 (≈ 70.31) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["----o---"
                                    "ooo-oo--"
                                    "-ooo-oo-"
                                    "oooooo-o"
                                    "-ooooo-o"
                                    "-----o-o"
                                    "oo-oo---"
                                    "----o---"]) :match/char-seqs) s4
                         (more-> [35 15] :match/location
                                 (≈ 84.38) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--oooo--"
                                    "oooooooo"
                                    "oo-oo--o"
                                    "-ooooooo"
                                    "--o--ooo"
                                    "-o-oo---"
                                    "oo--oo-o"]) :match/char-seqs) s5
                         (more-> [16 28] :match/location
                                 (≈ 85.94) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---o-o--"
                                    "-ooooo--"
                                    "-oooooo-"
                                    "o--oo-oo"
                                    "oooooooo"
                                    "-ooo-o--"
                                    "--ooo-o-"
                                    "o-o--ooo"]) :match/char-seqs) s6
                         (more-> [83 40] :match/location
                                 (≈ 70.31) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--------"
                                    "--oo----"
                                    "-ooooo--"
                                    "oo-ooo--"
                                    "o-o-ooo-"
                                    "-oooooo-"
                                    "-o--o---"
                                    "o-oo-o--"]) :match/char-seqs) s7
                         (more-> [82 41] :match/location
                                 (≈ 85.94) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--ooooo-"
                                    "-oo-ooo-"
                                    "oo-o-ooo"
                                    "o-oooooo"
                                    "--o--o--"
                                    "oo-oo-o-"
                                    "--oooo-o"]) :match/char-seqs) s8)
                :squid

                (more-of [c1 c2 c3 c4 :as crabs]
                         4 (count crabs)
                         (more-> [74 1] :match/location
                                 (≈ 87.5) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["ooo-----o--"
                                    "o--o-o-o---"
                                    "--o-ooooo--"
                                    "oo--ooo-oo-"
                                    "ooooooo-ooo"
                                    "oooo--ooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) c1
                         (more-> [85 12] :match/location
                                 (≈ 86.36) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--oo----o--"
                                    "-------o---"
                                    "o--oooooo--"
                                    "-oo--oo--o-"
                                    "oo-oooooooo"
                                    "o-ooooooo-o"
                                    "oo-o----o-o"
                                    "--ooo-oo--o"]) :match/char-seqs) c2
                         (more-> [60 13] :match/location
                                 (≈ 90.9) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--o-----o--"
                                    "-------o---"
                                    "--oooo-oo--"
                                    "----ooo-oo-"
                                    "o--oooooo-o"
                                    "o-o-ooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) c3
                         (more-> [82 40] :match/location
                                 (≈ 73.86) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["-----------"
                                    "---oo------"
                                    "--ooooo----"
                                    "-oo-ooo----"
                                    "oo-o-ooo---"
                                    "o-oooooo---"
                                    "--o--o-----"
                                    "oo-oo-o--o-"]) :match/char-seqs) c4)
                :crab)

              (sut/find-invaders invaders radar-sample
                                 {:sensitivity 70.0})))))

(defexpect find-invaders:all-matches:default-sensitivity-test
  (expecting "ALL matches at default sensitivity — Squid & Crab (example data)"
    (let [invaders     (sut/build-invaders)
          radar-sample (sut/build-radar-sample invaders)]
      (expect (more->
                #{:squid :crab} (-> keys set)

                (more-of [s1 s2 s3 s4 s5 s6 :as squids]
                         6 (count squids)
                         (more-> [42 0] :match/location
                                 (≈ 87.5) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--ooo-o-"
                                    "--ooooo-"
                                    "oo--o-oo"
                                    "oo-ooooo"
                                    "-----o--"
                                    "oo-oo-o-"
                                    "o-o--ooo"]) :match/char-seqs) s1
                         (more-> [35 15] :match/location
                                 (≈ 84.38) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--oooo--"
                                    "oooooooo"
                                    "oo-oo--o"
                                    "-ooooooo"
                                    "--o--ooo"
                                    "-o-oo---"
                                    "oo--oo-o"]) :match/char-seqs) s2
                         (more-> [16 28] :match/location
                                 (≈ 85.94) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---o-o--"
                                    "-ooooo--"
                                    "-oooooo-"
                                    "o--oo-oo"
                                    "oooooooo"
                                    "-ooo-o--"
                                    "--ooo-o-"
                                    "o-o--ooo"]) :match/char-seqs) s3
                         (more-> [82 41] :match/location
                                 (≈ 85.94) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["---oo---"
                                    "--ooooo-"
                                    "-oo-ooo-"
                                    "oo-o-ooo"
                                    "o-oooooo"
                                    "--o--o--"
                                    "oo-oo-o-"
                                    "--oooo-o"]) :match/char-seqs) s4
                         (more-> [18 0] :match/location
                                 (≈ 80.36) :match/accuracy
                                 true :match/partial?
                                 :top :match/edge-kind
                                 (t/str-vec->char-seqs
                                   ["o--ooo--"
                                    "-oooooo-"
                                    "oo-oo-oo"
                                    "o-oo-o--"
                                    "o----o--"
                                    "ooooo-o-"
                                    "o-o--o--"]) :match/char-seqs) s5
                         (more-> [17 45] :match/location
                                 (≈ 92.5) :match/accuracy
                                 true :match/partial?
                                 :bottom :match/edge-kind
                                 (t/str-vec->char-seqs
                                   ["---ooo--"
                                    "o-oooo--"
                                    "-oooooo-"
                                    "oo--o-oo"
                                    "oooooooo"]) :match/char-seqs) s6)
                :squid

                (more-of [c1 c2 c3 :as crabs]
                         3 (count crabs)
                         (more-> [74 1] :match/location
                                 (≈ 87.5) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["ooo-----o--"
                                    "o--o-o-o---"
                                    "--o-ooooo--"
                                    "oo--ooo-oo-"
                                    "ooooooo-ooo"
                                    "oooo--ooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) c1
                         (more-> [85 12] :match/location
                                 (≈ 86.36) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--oo----o--"
                                    "-------o---"
                                    "o--oooooo--"
                                    "-oo--oo--o-"
                                    "oo-oooooooo"
                                    "o-ooooooo-o"
                                    "oo-o----o-o"
                                    "--ooo-oo--o"]) :match/char-seqs) c2
                         (more-> [60 13] :match/location
                                 (≈ 90.9) :match/accuracy
                                 (t/str-vec->char-seqs
                                   ["--o-----o--"
                                    "-------o---"
                                    "--oooo-oo--"
                                    "----ooo-oo-"
                                    "o--oooooo-o"
                                    "o-o-ooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]) :match/char-seqs) c3)
                :crab)

              (sut/find-invaders invaders radar-sample
                                 {:sensitivity   80.0
                                  :edges         true
                                  :edges-cut-off 3})))))

;;

(comment
  (expectations.clojure.test/run-tests)
  .)

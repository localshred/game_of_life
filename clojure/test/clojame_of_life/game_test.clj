(ns clojame-of-life.game-test
  (:require [clojure.test :refer :all]
            [clojame-of-life.game :refer :all]))

(deftest test-str-repeat
  (are [character n expected] (= expected (str-repeat character n))
       "a" 3 "aaa"
       " " 10 "          "))

(defn is-dead-or-alive? [v] (or (= v dead) (= v alive)))

(deftest test-generate-row
  (is (= 5 (count (generate-row 5))))
  (is (true? (every? is-dead-or-alive? (generate-row 5)))))

(deftest test-rows->population
  (let [two-rows [[ 1 2 3] [4 5 6]]
        irregular-row-lengths [(repeat 0 100)
                               (repeat 0 50)
                               (repeat 0 200)
                               [1]]]
    (is (= 21 (rows->population two-rows)))
    (is (= 1 (rows->population irregular-row-lengths)))))

(deftest test-cell->char
  (is (= " " (cell->char dead)))
  (is (= "•" (cell->char alive))))

(deftest test-seq-range-inclusive
  (let [myseq [1 2 3 4 5 6 7 8 9]]
    (are [begin end expected] (= expected (seq-range-inclusive myseq begin end))
         1 4 [2 3 4 5]
         5 7 [6 7 8]
         7 15 [8 9]
         0 0 [1]
         0 -1 []
         10 20 [])))

(deftest test-gte-zero
  (are [n expected] (is (= expected (gte-zero n)))
       10 10
       1 1
       0 0
       -1 0))

(deftest test-neighbors
  (let [source [[ 1  2  3  4  5  ]
                [ 6  7  8  9  10 ]
                [ 11 12 13 14 15 ]
                [ 16 17 18 19 20 ]
                [ 21 22 23 24 25 ]]]
    (are [x y expected] (= expected (neighbors source x y))
         0 0 [ 1 2 6 7 ]
         2 0 [ 2 3 4 7 8 9 ]
         4 0 [ 4 5 9 10 ]
         4 2 [ 9 10 14 15 19 20 ]
         4 4 [ 19 20 24 25 ]
         2 4 [ 17 18 19 22 23 24 ]
         0 4 [ 16 17 21 22 ]
         0 2 [ 6 7 11 12 16 17 ]
         2 2 [ 7 8 9 12 13 14 17 18 19 ])))

(deftest test-new-cell-health-for-living-cell
  (are [population expected-health] (= expected-health (new-cell-health-for-living-cell population))
       0 dead
       1 dead
       2 alive
       3 alive
       4 dead
       5 dead
       6 dead
       7 dead
       8 dead
       9 dead))

(deftest test-new-cell-health-for-dead-cell
  (are [population expected-health] (= expected-health (new-cell-health-for-dead-cell population))
       0 dead
       1 dead
       2 dead
       3 alive
       4 dead
       5 dead
       6 dead
       7 dead
       8 dead
       9 dead))

(deftest test-neighbor-population
  (let [alive-neighbor-cells (repeat 9 1)
        dead-neighbor-cells (repeat 9 0)
        mixed-neighbor-cells [ 1 0 1 0 1 0 1 0 1 ]
        all-dead-except-target-neighbor-cells [ 0 0 0 0 1 0 0 0 0 ]]
    (are [cell-health cell-and-neighbors expected-population] (= expected-population (neighbor-population cell-health cell-and-neighbors))
         alive alive-neighbor-cells 8
         dead alive-neighbor-cells 9
         alive dead-neighbor-cells 0
         dead dead-neighbor-cells 0
         alive mixed-neighbor-cells 4
         dead mixed-neighbor-cells 5
         alive all-dead-except-target-neighbor-cells 0
         dead all-dead-except-target-neighbor-cells 1)))

; (deftest test-inc-generation
;   (let [source {:width 5
;                 :height 3
;                 :population 3
;                 :generation 0
;                 :rows [[ 0 0 0 0 0 ]
;                        [ 0 1 1 1 0 ]
;                        [ 0 0 0 0 0 ]]}
;         expected {:width 5
;                   :height 3
;                   :population 3
;                   :generation 0
;                   :rows [[ 0 0 1 0 0 ]
;                          [ 0 0 1 0 0 ]
;                          [ 0 0 1 0 0 ]]}
;         actual (inc-board-generation source)]
;     (is (= (:rows expected) (= (:rows actual))))
;     (is (= (:width expected) (= (:width actual))))
;     (is (= (:height expected) (= (:height actual))))
;     (is (= (:generation expected) (= (:generation actual))))
;     (is (= (:population expected) (= (:population actual))))))

(deftest test-next-row-generation
  (let [blinker1 [[ 0 0 0 0 0 ]
                  [ 0 1 1 1 0 ]
                  [ 0 0 0 0 0 ]]
        blinker2 [[ 0 0 1 0 0 ]
                  [ 0 0 1 0 0 ]
                  [ 0 0 1 0 0 ]]
        glider1 [[ 0 1 0 0 ]
                 [ 0 0 1 0 ]
                 [ 1 1 1 0 ]
                 [ 0 0 0 0 ]]
        glider2 [[ 0 0 0 0 ]
                 [ 1 0 1 0 ]
                 [ 0 1 1 0 ]
                 [ 0 1 0 0 ]]
        glider3 [[ 0 0 0 0 ]
                 [ 0 0 1 0 ]
                 [ 1 0 1 0 ]
                 [ 0 1 1 0 ]]
        glider4 [[ 0 0 0 0 ]
                 [ 0 1 0 0 ]
                 [ 0 0 1 1 ]
                 [ 0 1 1 0 ]]
        glider5 [[ 0 0 0 0 ] ; glider 1 slid down and over
                 [ 0 0 1 0 ]
                 [ 0 0 0 1 ]
                 [ 0 1 1 1 ]]]
    (are [gen1 gen2] (= gen2 (next-rows-generation gen1))
         blinker1 blinker2
         blinker2 blinker1
         glider1 glider2
         glider2 glider3
         glider3 glider4
         glider4 glider5)))


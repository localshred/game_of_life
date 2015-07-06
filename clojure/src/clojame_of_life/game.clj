(ns clojame-of-life.game
  (:gen-class))

(def default-width 90)
(def default-height 40)
(def alive 1)
(def dead 0)

(defn gte-zero
  "Return the max of n or zero"
  [n] (max 0 n))

(defn random-cell
  "Generate a random cell health"
  [& args]
  (if (= (mod (rand-int 100) 5) 0) alive dead))

(defn str-repeat
  "Repeat a character n times as a string"
  [character n]
  (apply str (repeat n character)))

(defn generate-row
  "Generate random cells for a given width"
  [ width ]
  (map random-cell (range width)))

(defn rows->population
  "Calculate the population of the given rows"
  [ rows ]
  (reduce #(reduce + %1 %2) 0 rows))

(defn make-coordinates
  [width height]
  (map (fn [y]
         (map (fn [x] [x y]) (range 0 width)))
       (range 0 height)))

(defn seq-range-inclusive
  "Gets the range of elements from a sequence, inclusive"
  [coll begin end]
  (->> coll
       (drop begin)
       (take (- (inc end) begin))))

(defn neighbors
  "Gets all neighbors and the target of a target cell at the (x,y) coordinate"
  [ rows x y ]
  (let [zero-based-width (gte-zero (- (count (first rows)) 1))
        zero-based-height (gte-zero (- (count rows) 1))
        min-x (gte-zero (dec x))
        max-x (min (inc x) zero-based-width)
        min-y (gte-zero (dec y))
        max-y (min (inc y) zero-based-height)]
     (->> (seq-range-inclusive rows min-y max-y)
          (reduce #(concat %1 (seq-range-inclusive %2 min-x max-x)) []))))

(defn neighbor-population
  "Calculates the population of the 8 neighbors surrounding the center cell"
  [target-cell-health neighbor-cells]
  (let [total-population (reduce + 0 neighbor-cells)]
    (gte-zero (- total-population target-cell-health))))

(defn alive?
  [health]
  (= health alive))

(defn new-cell-health-for-living-cell
  [population]
  (cond
    (or
      (= 2 population)
      (= 3 population)) alive
    :else dead))

(defn new-cell-health-for-dead-cell
  [population]
  (cond
    (= 3 population) alive
    :else dead))

(defn new-cell-health
  [current-cell-health population]
  (cond
    (alive? current-cell-health) (new-cell-health-for-living-cell population)
    :else (new-cell-health-for-dead-cell population)))

(defn next-row-generation
  [all-rows row-coordinates]
  (map (fn [[x y]]
         (let [cell (-> all-rows
                        (nth y)
                        (nth x))
               cell-neighbors (neighbors all-rows x y)
               population (neighbor-population cell cell-neighbors)]
           (new-cell-health cell population)))
       row-coordinates))

(defn next-rows-generation
  [current-rows]
  (let [width (count (first current-rows))
        height (count current-rows)
        coordinate-rows (make-coordinates width height)]
    (map (fn [row-coordinates]
           (next-row-generation current-rows row-coordinates))
         coordinate-rows)))

(defn generate-rows-or-get-next-generation
  [ height width current-rows ]
  (if (nil? current-rows)
    (map generate-row (repeat height width))
    (next-rows-generation current-rows)))

(defn next-board-generation
  "Get the next board generation"
  [ board ]
  (let [width (:width board)
        height (:height board)
        generation (:generation board -1)
        rows (generate-rows-or-get-next-generation height width (:rows board))
        population (rows->population rows)]
    (-> board
        (assoc-in [:rows] rows)
        (assoc-in [:population] population)
        (assoc-in [:generation] (inc generation)))))

(defn cell->char
  "The health representation character for the given cell value"
  [ cell ]
  (or (and (= cell dead) " ") "•"))

(defn print-stat-line
  "Print out the stat line with generation, population, and board size"
  [ board ]
  (let [width (:width board)
        height (:height board)
        generation (:generation board 0)
        population (:population board 0)
        generation-display (str "g=" generation)
        population-display (str "p=" population)
        board-size-display (str " " width "x" height)
        empty-stat-line-chars (- width (+ (count board-size-display) (count population-display) (count generation-display)))
        stat-line (str generation-display " " population-display (str-repeat " " empty-stat-line-chars) board-size-display)]
    (println stat-line)))

(defn draw-board
  "Draw the board for the given dimensions"
  [ board ]
  (let [width (:width board)
        horizontal-lines (str-repeat "─" width)
        board            (next-board-generation board)
        rows             (:rows board)
        top-line         (str  "┌" horizontal-lines "┐")
        make-row-line    #(str "│" %                "│")
        bottom-line      (str  "└" horizontal-lines "┘")]
    (println (:population board))
    (println top-line)
    (doseq [ row-cells rows ]
      (println (make-row-line (apply str (map cell->char row-cells)))))
    (println bottom-line)
    (print-stat-line board)
    board))

(defn -main
  "Run the game of life"
  [& [width height & args]]
  (let [width (if (nil? width) default-width (Integer/parseInt width))
        height (if (nil? height) default-height (Integer/parseInt height))]
    (loop [board (draw-board {:width width :height height})]
      (Thread/sleep 20)
      (recur (draw-board board)))))


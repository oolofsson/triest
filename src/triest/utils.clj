(ns triest.utils
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn flip-weighted-coin
  "Generating true with given probability"
  [probability]
  (> probability (rand)))

(defn get-neighbors
  "Return neigbors of a vertex"
  [edges vertex]
  (get edges vertex))

(defn parse-int
  "Converts string to int"
  [s]
  (try
    (Integer. (re-find  #"\d+" s ))
      (catch Exception e (str "invalid"))))

(defn convert-edge-to-set
  [u v]
  [(min u v) (max u v)])

(defn process-edge?
  "Checks whether to process an edge, conditions: valid input, different vertexes, not seen before"
  [edge-set edge]
  (let [u (:u edge)
        v (:v edge)]
    (cond
      (= u v) false ; if same vertex
      (or (not (integer? u)) (not (integer? v))) false ; invalid input
      (contains? edge-set (convert-edge-to-set u v)) false ; if already inserted
      :else true)))

(defn get-random-edge
  "Returns a random edge."
  [edge-set]
  (let [edge (rand-nth (seq edge-set))]
    {:u (nth edge 0)
     :v (nth edge 1)}))

; estimates according to triest paper.
(defn estimate-count
  [memory t global]
  (* (max (int (/ (* t (- t 1) (- t 2)) (* memory (- memory 1) (- memory 2)))) 1.0) global))

(defn estimate-weight
  [memory t]
  (max (/ (* (- t 1) (- t 2)) (* memory (- memory 1))) 1))

(def cli-options
  [["-a" "--algorithm ALGORITHM" "Algorithm type"
    :id :algorithm
    :default "base"
    :validate [(fn [a] (or (= a "base") (= a "impr"))) "Invalid input, choices: -a base, -a impr (default: base)"]]
   ["-f"  "--file FILEPATH" "Filepath"
    :id :filepath
    :default "./resources/out.moreno_health_health"
    :validate [(fn [f] (.exists (io/as-file f))) "Invalid filepath, specify: -f ./resources/(existing file) (default: ./resources/moreno_health_health)"]]
   ["-h" "--help"]])

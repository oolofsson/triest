(ns triest.core
  (:gen-class)
  (:require [triest.construct :refer :all]
            [clojure.set :refer [intersection]]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [triest.utils :refer :all]))

(defn update-count
  "Update count based on provided operation function, either increase or decrease."
  ([state edge operation]
  (let [shared-neighbors (intersection ; get shared neighbors
                             (get-neighbors (:edges state) (:u edge))
                             (get-neighbors (:edges state) (:v edge)))]
    (if (not= (count shared-neighbors) 0)
      (operation state edge shared-neighbors)
      state)))
  ([state edge operation weight]
  (let [shared-neighbors (intersection ; get shared neighbors
                             (get-neighbors (:edges state) (:u edge))
                             (get-neighbors (:edges state) (:v edge)))]
    (if (not= (count shared-neighbors) 0)
      (operation state edge shared-neighbors weight)
      state))))

(defmulti sample :type)

(defmethod sample :base
  [{state :state edge :edge}]
  (if (flip-weighted-coin (/ (:memory state) (:t state))) ; return true with probability (memory / t)
    (let [edge-to-remove (get-random-edge (:edge-set state))] ; remove random edge if sampling
      (->
        (remove-edge state edge-to-remove)
        (update-count edge-to-remove decrease-counters)
        (insert-edge edge)
        (update-count edge increase-counters)))
    state))

(defmethod sample :impr
  [{state :state edge :edge}]
  (if (flip-weighted-coin (/ (:memory state) (:t state))) ; return true with probability (memory / t)
    (let [edge-to-remove (get-random-edge (:edge-set state))] ; remove random edge if sampling
      (->
        (remove-edge state edge-to-remove)
        (insert-edge edge)))
    state))

(defn process
  "Process edge, insert or sample."
  ([state edge]
  (if (< (:t state) (:memory state)) ; insert if space, otherwise sample
    (->
      (insert-edge state edge)
      (update-count edge increase-counters))
    (sample {:type :base :state state :edge edge})))
  ([state edge weight]
  (let [state (update-count state edge increase-counters weight)] ; always update count in impr
    (if (< (:t state) (:memory state)) ; insert if space, otherwise sample
      (insert-edge state edge)
      (sample {:type :impr :state state :edge edge})))))

; TRIEST - functions
(defmulti triest :type)

(defmethod triest :base
  [args]
  (let [state (:state args)
        edge  (:edge args)]
    (if (process-edge? (:edge-set state) edge)
      (as->
        (increase-t state) $
        (process $ edge)
        (assoc-in $ [:count :estimate] (estimate-count (:memory $) (:t $) (get-in $ [:count :global]))))
      state)))

(defmethod triest :impr
  [args]
  (let [state (:state args)
        edge  (:edge args)]
    (if (process-edge? (:edge-set state) edge)
      (as->
        (increase-t state) $
        (process $ edge (estimate-weight (:memory $) (:t $)))
        (assoc-in $ [:count :estimate] (int (get-in $ [:count :global]))))
      state)))

(defn run-counting
  "Runs through the whole file."
  [args]
  (reduce
    (fn [state line]
      (let [edge {:u (parse-int (nth (str/split line #" ") 0))
                  :v (parse-int (nth (str/split line #" ") 1))}]
        (triest {:type (:type args) :state state :edge edge}))) ; run triest updating state with one edge at a time
      (init-state (:memory args))
      (line-seq (:stream args))))

(defn get-chunk-size ; get chunk size 1/10 of number of edges for sampling
  [filepath]
  (with-open [stream (clojure.java.io/reader filepath)]
    (int (/ (count (line-seq stream)) 10))))

(defn write-to-file
  [estimates]
  (with-open [wrtr (clojure.java.io/writer "./plot.txt")]
    (doall
      (map
        (fn [estimate] ; write estimate to file, 0: memory (sample size), 1: estimate (number of triangles)
          (.write wrtr
            (str
              (nth estimate 0) " "
              (nth estimate 1) "\n")))
          estimates))))

(defn -main
  "Triest, triangle counting"
  [& args]
  (let [parsed (parse-opts args cli-options)
        options (:options parsed)
        errors (:errors parsed)]
    (if (not (nil? errors))
      (doall (map println errors)) ; print all errors
      (let [chunk (get-chunk-size (:filepath options))]
        (println (str "Running, algorithm: Triest " (:algorithm options) ", inputfile: " (:filepath options)))
        (->
          (doall
            (map ; run through 10 steps of counting with (size / 10 * iterator) memory size
              (fn [i]
                (println (str "Counting with sample size: " (max (* i chunk) 3)))
                (with-open [stream (clojure.java.io/reader (:filepath options))]
                  (as->
                      (run-counting {:type (keyword (:algorithm options)) :stream stream :memory (max (* i chunk) 3)}) $ ; to avoid division by zero at estimate
                      (do (println (str "Estimated number of global triangles: " (get-in $ [:count :estimate]))) $)
                      [(:memory $) (get-in $ [:count :estimate])])))
              (range 1 11)))
          (write-to-file )))))) ; print this to file for plotting, (memory, estimate)

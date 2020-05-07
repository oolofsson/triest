(ns triest.construct
  (:gen-class)
  (:require [triest.utils :refer :all]))

(defn init-state
  "Init starting state with available sample size given."
  [memory]
  {:memory memory
   :t 0
   :edges {}
   :edge-set nil
   :count {:local {}
           :global 0
           :estimate 0}})

(defn increase-t
  [state]
  (update state :t inc))

; local counters are dissociated when zero or less.
(defn remove-local-counter
  "Dissociate local counter."
  [state vertex]
  (update-in state [:count :local]
    (fn [local-counters]
      (dissoc local-counters vertex))))

(defn increase-local
  "Increase vertex specific counter."
  [state vertex increase]
  (update-in state [:count :local vertex]
    (fn [local-count]
      (if (nil? local-count)
        increase
        (+ local-count increase)))))

(defn decrease-local
  "Decrease vertex specific counter."
  [state vertex decrease]
  (let [local-count (get-in state [:count :local vertex])]
    (if (<= (- local-count decrease) 0)
      (remove-local-counter state vertex)
      (update-in state [:count :local vertex] - decrease))))

(defn increase-counters
  "Increase counters, global and locals."
  ([state edge neighbors]
  (let [u (:u edge)
        v (:v edge)
        increase (count neighbors)]
    (as->
      (update-in state [:count :global] + increase) $
      (increase-local $ u increase)
      (increase-local $ v increase)
      (reduce
        (fn [state neighbor]
          (increase-local state neighbor 1))
        $
        (vec neighbors)))))
  ([state edge neighbors weight] ; update with number of shared neighbors * weight
  (let [u (:u edge)
        v (:v edge)
        increase (count neighbors)
        weight (double weight)]
    (as->
      (update-in state [:count :global] + (* increase weight)) $
      (increase-local $ u (* increase weight))
      (increase-local $ v (* increase weight))
      (reduce
        (fn [state neighbor]
          (increase-local state neighbor weight))
        $
        (vec neighbors))))))

(defn decrease-counters
  "Decrease counters, global and locals."
  [state edge neighbors]
  (let [u (:u edge)
        v (:v edge)
        decrease (count neighbors)]
        (as->
          (update-in state [:count :global] - decrease) $
          (decrease-local $ u decrease)
          (decrease-local $ v decrease)
          (reduce
            (fn [state neighbor]
              (decrease-local state neighbor 1))
            $
            (vec neighbors)))))

(defn insert-edge
  "Insert given edge."
  [state edge]
  (let [u (:u edge)
        v (:v edge)]
    (->
      (update state :edge-set
          (fn [s]
            (let [inserted-edge (convert-edge-to-set u v)]
              (if (nil? s) #{inserted-edge} (conj s inserted-edge)))))
      (update-in [:edges u]
        (fn [e]
          (if (nil? e) #{v} (conj e v))))
      (update-in [:edges v]
        (fn [e]
          (if (nil? e) #{u} (conj e u)))))))

(defn dissoc-if-empty
  "Vertexes that has no connections should be dissociated."
  [state vertex]
  (update state :edges
          (fn [edges]
            (if (empty? (get edges vertex))
              (dissoc edges vertex)
              edges))))

(defn remove-edge
  "Remove given edge."
  [state edge]
  (let [u (:u edge)
        v (:v edge)]
    (->
      (update state :edge-set
        (fn [s]
          (let [removed-edge (convert-edge-to-set u v)]
            (set (remove #{removed-edge} s)))))
      (update-in [:edges u]
        (fn [e]
          (set (remove #{v} e))))
      (dissoc-if-empty u)
      (update-in [:edges v]
        (fn [e]
          (set (remove #{u} e))))
      (dissoc-if-empty v))))

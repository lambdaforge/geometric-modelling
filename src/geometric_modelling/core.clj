(ns geometric-modelling.core
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.core.matrix :refer :all]
            [incanter.core :as math]
            [clojure.core.matrix.operators :refer :all]))


(defn generic-bernstein
  "Generic Bernstein polynomial of degree n"
  [i n t]
  (* (math/choose n i)
     (pow t i)
     (pow (- 1 t) (- n i))))

(defn recursive-bernstein
  "Generic Bernstein polynomial of degree n with recursion"
  [i n t]
  (if (or (< i 0) (> i n) )
    0
    (if (and (= 0 i) (= 0 n))
      1
      (+ (* (- 1 t) (recursive-bernstein i (dec n) t))
         (* t (recursive-bernstein (dec i) (dec n) t))))))


(defn de-casteljau
  "Calculate de Casteljau points "
  [i r t b points]
  (if (= r 0)
    (let [res (get b i)]
      (println "i:" i ",r:" r " -> " res)
      (swap! points conj res)
      res)
    (let [b1 (de-casteljau i (dec r) t b points)
          b2 (de-casteljau (inc i) (dec r) t b points)
          res (+ (* (- 1 t) b1) (* t b2))]
      (println "i:" i ",r:" r " -> " res)
      (swap! points conj b1 b2)
      res)))

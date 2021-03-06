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

(defn vandermonde
  "Create Vandermonde matrix"
  [t0 t1 t2]
  (array [[1 t0 (* t0 t0)]
          [1 t1 (* t1 t1)]
          [1 t2 (* t2 t2)]]))

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


(defn de-boor
  "Calculates de boor points"
  [u0 u i r d n]
  (if ( = r 0)
    (get d i)
    (if (= u0 (get u (+ i r)))
      0
      (let [result (+ (* (de-boor u0 u i (dec r) d n)
                         (- 1 (/ (- u0 (get u i))
                                 (- (get u (+ i n)) (get u i)))))
                      (* (de-boor u0 u (inc i) (dec r) d n)
                         (/ (- u0 (get u (+ i r)))
                            (- (get u (+ i n 1)) (get u (+ i r))))))]
        (println "i:" i ",r:" r "->" result)))))



(comment

  (def points [[1 1] [2 4] [3 7] [4 11] [5 5]])

  (def de-casteljau-points (atom #{}))

  (de-casteljau 0 4 1/2 points de-casteljau-points)


  (de-boor 2 [0 0 0 1 2 3 3 3] 2 1 [10 20 30 40 50] 2)

  (de-boor 3 [0 0 0 0 1/4 1/2 3/4 1 1 1 1] )
  
)

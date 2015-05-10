(ns geometric-modelling.core
  (:require [incanter.core :refer [choose pow]]))


(defn generic-bernstein
  "Generic Bernstein polynomial of degree n"
  [i n t]
  (* (choose n i)
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

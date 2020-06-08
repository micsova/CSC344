(ns project2.core
  (:require [clojure.string :as str]))

(defn lookup
  "Lookup a value, i, in map m and returns the result if it exists.
  Otherwise returns i."
  [i m]
  (get m i i))

;(defn substitute
;  "Substitute each element in l with the value the element
;  is mapped to in m."
;  [l m]
;  (map (fn [i]
;         (lookup i m))
;       l))

(defn deep-substitute
  "Substitute, but recursive."
  [l m]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i m)
           (lookup i m)))
       l))

(defn anyFalse?
  "Checks a boolean expression for a false value. If it finds one, it returns false."
  [expr]
  (some false? expr))

(defn allFalse?
  "Checks a boolean expression if every value is false. If they are, it returns false."
  [expr]
  (every? false? expr))

(defn anyTrue?
  "Checks a boolean expression for a true value. If it finds one, it returns true."
  [expr]
  (some true? expr))

(defn allTrue?
  "Checks a boolean expression if every value is true. If they are, it returns true."
  [expr]
  (every? true? expr))

(defn removeTrue
  "Removes all true values from expression, leaving only variables and nested expressions."
  [expr]
  (let [reduced (remove true? expr)]
    (if (> (count reduced) 1)
      (str/join " " reduced)
      (str/join reduced))))

(defn removeFalse
  "Removes all false values from expression, leaving only variables and nested expressions."
  [expr]
  (let [reduced (remove false? expr)]
    (if (> (count reduced) 1)
      (str/join " " reduced)
      (str/join reduced))))

(declare notCheck)
(defn notSubstitute
  [expr]
  (map (fn [i]
         (if (seq? i)
           (notCheck i)
           (if (true? i)
             false
             (if (false? i)
               true
               (if (= i 'and)
                 'or
                 (if (= i 'or)
                   'and
                   (read-string (str "(not " i ")"))))))))
       expr))

(defn notCheck
  [expr]
  (if (= (first expr) 'not)                                 ; If the first value is NOT, check the second value
    (let [second (first (drop 1 expr))]
      (if (seq? second)                                     ; If the second value is not a sequence, return the expression
        (if (= (first second) 'not)                         ; If the second value is a sequence, check if its the second NOT in a row...
          (notCheck (first (drop 1 second)))                ; If so, drop both and recheck the rest of the expression
          (notSubstitute second))                           ; If not, do notSubstitute on the rest
        second))
    expr))

(declare booleanParser)
(defn andSimplify
  [expression]
  (let [expr (drop 1 expression)]                           ; Drop the leading AND
    (let [reduced (map (fn [i]                              ; Simplify the sub sequences, if any
                         (if (seq? i)
                           (booleanParser i)
                           i))
                       expr)]
      (if (anyFalse? reduced)                               ; If there are any falses, the whole AND is false
        false
        (if (allTrue? reduced)                              ; If it's all trues, the whole AND is true
          true
          (removeTrue reduced))))))                         ; Otherwise, remove the trues to simplify and return the result

(defn orSimplify
  [expression]
  (let [expr (drop 1 expression)]                           ; Drop the leading OR
    (let [reduced (map (fn [i]                              ; Simplify the sub sequences, if any
                         (if (seq? i)
                           (booleanParser i)
                           i))
                       expr)]
      (if (anyTrue? reduced)                                ; If there are any trues, the whole OR is true
        true
        (if (allFalse? reduced)                             ; If it's all falses, the whole OR is false
          false
          (removeFalse reduced))))))                        ; Otherwise, remove the falses to simplify and return the result

(defn handleAnd
  "Handles AND expressions and returns the value"
  [expr]
  (let [result (andSimplify expr)]
    (if (boolean? result)
      result
      (if (= (count (read-string (str "(" result ")"))) 1)
        (read-string result)
        (read-string (str "(and " result ")"))))))

(defn handleOr
  "Handles OR expressions and returns the value"
  [expr]
  (let [result (orSimplify expr)]
    (if (boolean? result)
      result
      (if (= (count (read-string (str "(" result ")"))) 1)
        (read-string result)
        (read-string (str "(or " result ")"))))))

(defn booleanParser
  "Takes in a boolean statement and produced either a result or a simplified statement."
  [expr]
  (let [op (first expr)]
    (if (= op 'not) ; if NOT, do notCheck and recurse
      (let [notChecked (notCheck expr)]
        (if (seq? notChecked)
          (booleanParser notChecked)
          (read-string (str "(not " notChecked ")"))))
      (if (= op 'and)
        (handleAnd expr)
        (if (= op 'or)                                      ; If OR, do orSimplify and return the result
          (handleOr expr))))))

(defn evalexp
  "Takes in a boolean statement and a binding map. It replaces the keys in the map with the corresponding values and
  then reduces the boolean expression accordingly."
  [expr map]
  (booleanParser (deep-substitute expr map)))

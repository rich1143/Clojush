(ns clojush.problems.ec-ai-demos.mean
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojush pushstate interpreter]
        clojush.instructions.common))

;; Take in five integers, return mean of integers

; (def input-set
;   [[4,3,2,1,5]
;    [17,8,4,0,7]
;    [1,2,3,4,5]
;    [14,3,20,7,9]
;    [6,9,8,1,2]
;    [1,1,1,1,1]
;    [2,2,2,2,2]
;    [17,17,17,17,17]
;    [9,9,9,9,9]
;    [20,21,22,8,9]
;    [5,6,8,7,40]
;    [14,14,15,15,16]
;    [14,14,15,16,16]
;    [90,80,70,50,40]
;    [60,1000,200,40,5]
;    [19,17,15,1,3]
;    [7,4,5,6,89]
;    [69,23,1,46,3]
;    [54,79,89,56,21]
;    [25,5,5,5,10]
;    [10,10,10,10,10]
;    [28,237,19,3,7]
;    [101,568,995,455,2]
;    [9001,4,500,63,23]
;    [14,28,56,112,224]
;    [2,4,8,16,32]
;    [5,10,15,20,25]
;    [3,1,4,1,5]
;    [314,15,9,26,65]
;    [2,2,3,4,4]])

(def input-set
  [[[4,3,2,1,5]]
   [[17,8,4,0,7]]
   [[1,2,3,4,5]]
   [[14,3,20,7,9]]
   [[6,9,8,1,2]]
   [[1,0,0,0,0]]
   [[0,2,0,0,0]]
   [[0,0,17,0,0]]
   [[0,0,0,9,0]]
   [[20,21,22,8,9]]
   [[5,6,8,7,40]]
   [[14,14,15,15,16]]
   [[0,0,0,16,16]]
   [[90,80,70,50,40]]
   [[60,1000,200,40,5]]
   [[19,17,15,1,3]]
   [[7,4,5,6,89]]
   [[69,23,1,46,3]]
   [[54,79,89,56,21]]
   [[0,5,5,5,0]]
   [[0,0,0,0,10]]
   [[28,237,19,3,7]]
   [[101,568,995,455,2]]
   [[9001,4,500,63,23]]
   [[14,28,56,112,224]]
   [[2,4,8,16,32]]
   [[5,10,15,20,25]]
   [[0,1,0,1,0]]
   [[314,15,9,26,65]]
   [[2,2,0,0,0]]])

(defn expected-output
  [inputs]
  (let [[[one two three four five]] inputs]
    (/ (+ one two three four five) 5)))

(defn make-start-state
  [inputs]
  (reduce (fn [state input]
            (push-item input :input state))
          (make-push-state)
          inputs))

(defn actual-output
  [program inputs]
  (let [start-state (make-start-state inputs)
        end-state (run-push program start-state)
        result (top-item :float end-state)]
    result))

(defn abs [n]
  (if (< n 0)
    (- n)
    n))

(defn all-errors
  [program]
  (doall
    (for [inputs input-set]
      (let [expected (expected-output inputs)
            actual (actual-output program inputs)]
        (if (= actual :no-stack-item)
          10000
          (abs (- expected actual)))))))

(def atom-generators
  (concat (registered-for-stacks [:vector_integer :float :integer])
          (list 5)
          (list 'in1)))

(def argmap
  {
   :parent-selection :epsilon-lexicase
   :error-function all-errors
   :atom-generators atom-generators
   })

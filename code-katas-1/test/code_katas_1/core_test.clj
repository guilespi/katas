(ns code-katas-1.core-test
  (:require [clojure.test :refer :all]
            [code-katas-1.core :as kata]))

(deftest filter-odd
  (is (= (kata/filter-odd #{1 2 3 4 5}) '(1 3 5)))
  (is (= (kata/filter-odd [4 2 1 6]) '(1)))
  (is (= (kata/filter-odd [2 2 4 6]) '()))
  (is (= (kata/filter-odd [1 1 1 3]) '(1 1 1 3))))

(deftest nil-key
  (is (true?  (kata/nil-key :a {:a nil :b 2})))
  (is (false? (kata/nil-key :b {:a nil :b 2})))
  (is (false? (kata/nil-key :c {:a nil :b 2}))))

(deftest implement-range
  (is (= (kata/range 5 8) '(5 6 7)))
  (is (= (kata/range -2 2) '(-2 -1 0 1)))
  (is (= (kata/range 1 4) '(1 2 3))))

(deftest compress-sequence
  (is (= (apply str (kata/compress-sequence "Leeeeeerrroyyy")) "Leroy"))
  (is (= (kata/compress-sequence [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (kata/compress-sequence [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

(deftest max-value
  (is (= (kata/max-value 1 8 3 4) 8))
  (is (= (kata/max-value 30 20) 30))
  (is (= (kata/max-value 45 67 11) 67)))

(deftest split-two
  (is (= (kata/split-two 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (kata/split-two 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (kata/split-two 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest inter-two
  (is (= (kata/inter-two [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (is (= (kata/inter-two [1 2] [3 4 5 6]) '(1 3 2 4)))
  (is (= (kata/inter-two [1 2 3 4] [5]) [1 5]))
  (is (= (kata/inter-two [30 20] [25 15]) [30 25 20 15])))

(deftest retrieve-caps
  (is (= (kata/retrieve-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (kata/retrieve-caps "nothing")))
  (is (= (kata/retrieve-caps "$#A(*&987Zf") "AZ")))

(deftest find-truth
  (is (= false (kata/find-truth false false)))
  (is (= true (kata/find-truth true false)))
  (is (= false (kata/find-truth true)))
  (is (= true (kata/find-truth false true false)))
  (is (= false (kata/find-truth true true true)))
  (is (= true (kata/find-truth true true true false))))

(deftest zip-map
  (is (= (kata/zip-map [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (kata/zip-map [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (kata/zip-map [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

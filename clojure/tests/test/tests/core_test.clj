(ns tests.core-test
  (:require [clojure.test :refer :all]
            [tests.core :refer :all]))

(deftest fac-test
  (testing "3! = 6"
    (is (= (fac 3) 6)))
  (testing "7! = 5040"
    (is (= (fac 7) 5040))))

(deftest dumb-fibo-test
  (testing "fibo 0 = 0"
    (is (= (dumb-fibo 0) 0)))
  (testing "fibo 1 = fibo 2"
    (is (= (dumb-fibo 1)
           (dumb-fibo 2))))
  (testing "fibo 10 = 55"
    (is (= (dumb-fibo 10) 55))))

(deftest fibo-test
  (testing "dumb-fibo = fibo"
    (is (= (map dumb-fibo (range 20))
           (map fibo (range 20)))))
  (testing "fibo 0 = 0"
    (is (= (fibo 0) 0)))
  (testing "fibo 1 = 1"
    (is (= (fibo 1) 1)))
  (testing "fibo 1 = fibo 2"
    (is (= (fibo 1) (fibo 2))))
  (testing "fibo 13 = 233"
    (is (= (fibo 13) 233))))

(deftest fibo-range-test
  (testing "fibo-range = fibo for range 20"
    (is (= (fibo-range 20)
           (map fibo (range 20))))))

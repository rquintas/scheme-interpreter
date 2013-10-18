(ns interpreter.core-test
  (:use clojure.test
        interpreter.core
        interpreter.driver))

(deftest eval-addition
    (testing "Evaluating addition."
        (is (= (eval (list '+ 1 1) the-global-environment) 2))))

(deftest eval-subtraction
    (testing "Evaluating subtraction."
        (is (= (eval (list '- 1 1) the-global-environment) 0))))

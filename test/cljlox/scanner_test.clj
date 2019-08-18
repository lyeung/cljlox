(ns cljlox.scanner-test
  (:require 
    [clojure.test :refer :all] 
    [cljlox.scanner :as scanner]))

(defn wrap-setup
  "Reset app-state before running test codes."
  [f]
  (scanner/reset-app-state!)
  (f))

(use-fixtures :once wrap-setup)
  
(deftest report-test
  (testing "change had-error state to true"
    (scanner/report "line" "where" "message")
    (is (true? (:had-error @scanner/app-state)))))

(deftest error-test
  (testing "change had-error state to true"
    (scanner/error "line" "where")
    (is (true? (:had-error @scanner/app-state)))))

(deftest token-tostring
  (testing "to string"
    (is (= "tok1 foo bar baz"
           (scanner/token-tostring
             {:token-type "tok1"
              :lexeme "foo"
              :literal "bar baz"
              :line 100})))))

(deftest is-at-end?-test
  (testing "current position is at end of string"
    (is (true? (scanner/is-at-end? "" 0)))
    (is (true? (scanner/is-at-end? "hello" 5))))
  (testing "current position is before end of string"
    (is (false? (scanner/is-at-end? "hello" 0)))
    (is (false? (scanner/is-at-end? "hello" 4)))))

(deftest advance-test
  (testing "increments and get character"
    (is (= \E
           (scanner/advance "HELP")))
    (is (= \L
           (scanner/advance "HELP")))))

(deftest add-token-test
  (testing "add token"
    (is (= {:tokens [{:token-type :identifier
                      :lexeme "HELP"
                      :literal "foo"
                      :line 100}]}
           (scanner/add-token "HELP ME"
                              :identifier
                              "foo"
                              0
                              4
                              100)))))

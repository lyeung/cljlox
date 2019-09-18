(ns cljlox.scanner-test
  (:require 
    [clojure.test :refer :all] 
    [cljlox.scanner :as scanner]))

(defn wrap-setup
  "Reset app-state before running test codes."
  [f]
  (scanner/reset-app-state!)
  (scanner/reset-output-state!)
  (f))

(use-fixtures :each wrap-setup)
  
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
    (is (= \H
           (scanner/advance "HELP")))
    (is (= \E
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

(defn scan-verify
  [source
   token-type
   lexeme
   literal]
  (let [result (scanner/scan-tokens source)]
    (is (= {:tokens
            [{:token-type token-type
              :lexeme lexeme
              :literal literal
              :line 1}]}
           result))))

(deftest scan-tokens-left-paren-test
  (testing "scan left paren"
    (scan-verify "(foo"
                 :left-paren
                 "("
                 nil)))

(deftest scan-tokens-right-paren-test
  (testing "scan right paren"
    (scan-verify ")foo"
                 :right-paren
                 ")"
                 nil)))

(deftest scan-tokens-left-brace-test
  (testing "scan left brace"
    (scan-verify "{foo"
                 :left-brace
                 "{"
                 nil)))

(deftest scan-tokens-right-brace-test
  (testing "scan right brace"
    (scan-verify "}foo"
                 :right-brace
                 "}"
                 nil)))

(deftest scan-tokens-comma-test
  (testing "scan comma"
    (scan-verify ",foo"
                 :comma
                 ","
                 nil)))

(deftest scan-tokens-dot-test
  (testing "scan dot"
    (scan-verify ".foo"
                 :dot
                 "."
                 nil)))

(deftest scan-tokens-minus-test
  (testing "scan minus"
    (scan-verify "-foo"
                 :minus
                 "-"
                 nil)))

(deftest scan-tokens-plus-test
  (testing "scan plus"
    (scan-verify "+foo"
                 :plus
                 "+"
                 nil)))

(deftest scan-tokens-semicolon-test
  (testing "scan semicolon"
    (scan-verify ";foo"
                 :semicolon
                 ";"
                 nil)))

(deftest scan-tokens-star-test
  (testing "scan star"
    (scan-verify "*foo"
                 :star
                 "*"
                 nil)))

(deftest scan-tokens-bang-test
  (comment testing "scan bang"
    (scan-verify "!foo"
                 :bang
                 "!"
                 nil)))

(deftest scan-tokens-bang-equal-test
  (testing "scan bang equal"
    (scan-verify "!=foo"
                 :bang-equal
                 "!="
                 nil)))

(deftest scan-tokens-equal-test
  (testing "scan equal"
    (scan-verify "=foo"
                 :equal
                 "="
                 nil)))

(deftest scan-tokens-equal-equal-test
  (testing "scan equal equal"
    (scan-verify "==foo"
                 :equal-equal
                 "=="
                 nil)))

(deftest scan-tokens-less-test
  (testing "scan less"
    (scan-verify "<foo"
                 :less
                 "<"
                 nil)))

(deftest scan-tokens-less-equal-test
  (testing "scan less equal"
    (scan-verify "<=foo"
                 :less-equal
                 "<="
                 nil)))

(deftest scan-tokens-greater-test
  (testing "scan greater"
    (scan-verify ">foo"
                 :greater
                 ">"
                 nil)))

(deftest scan-tokens-greater-equal-test
  (testing "scan greater equal"
    (scan-verify ">=foo"
                 :greater-equal
                 ">="
                 nil)))


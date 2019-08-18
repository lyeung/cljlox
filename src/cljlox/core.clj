(ns cljlox.core
  (:require [cljlox.scanner 
             :as scanner])
  (:gen-class))

(defn run
  [source]
  (let [tokens 
        (scanner/scan-tokens source)]
    (doseq [t tokens]
      (println t))))

(defn run-file
  [file]
  (run (slurp file))
  (if (scanner/had-error?)
    (throw (ex-info "had-error"))))

(defn print-prompt
  []
  (print "> ")
  (flush))

(defn run-prompt
  []
  (print-prompt)
  (loop [input (read-line)]
    (run input)
    (if (scanner/had-error?)
      (scanner/set-no-had-error!))
    (print-prompt)
    (recur (read-line))))

(defn run-cond
  [args]
  (cond
    (> (count args) 1) (println "usage: jlox <script>")
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (try
    (run-cond args)
    (catch clojure.lang.ExceptionInfo e
      (System/exit 65))))


(ns cljlox.scanner)

(def app-state
  "Application state."
  (atom {:had-error false
         :start 0
         :current 0
         :line 1}))

(def output-state
  "Output state."
  (atom {:tokens []}))

(defn reset-app-state!
  "Reset app-state to original state."
  []
  (reset! app-state 
          {:had-error false
           :start 0
           :current 0
           :line 1}))


(defn had-error?
  "Had error?"
  []
  (:had-error @app-state))

(defn set-no-had-error!
  "Reset had-error to false."
  []
  (swap! app-state assoc :had-error false))

(defn report
  "Prints error message and update had-error
   to true."
  [line where message]
  (println 
    (str "[" line "] Error" where
         ": " message))
  (swap! app-state assoc :had-error true))

(defn error
  "Report an error message."
  [line message]
  (report line "" message))

(def token-type
  "Token types."
  #{;; single-char token
    :left-paren :right-paren
    :left-brace :right-brace
    :comma :dot :minus :plus
    :semicolon :slash :star
    ;; one or two char tokens
    :bang :bang-equal
    :equal :equal-equal
    :greater :greater-equal
    :less :less-equal
    ;; literals
    :identifier :string :number
    ;; keywords
    :and :class :else :false
    :fun :for :if :nil :or
    :print :return :super :this
    :true :var :while :eof})

(defn create-token
  "Create token."
  [token-type lexeme literal line]
  {:token-type token-type
   :lexeme lexeme
   :literal literal
   :line line})

(defn token-tostring
  "Returns a string representation of token
  as \"<token-type> <lexeme> <literal>\"."
  [{:keys [token-type lexeme literal line]
   :as token}]
  (str token-type " " lexeme " " literal))

(defn is-at-end?
  "Is current position at the end of source?"
  ([source] (is-at-end? 
              source (:current @app-state)))
  ([source current] (>= 
                      current (count source))))

(defn advance
  "Increments current variable and returns 
  the character of source that position."
  [source] 
  (swap! app-state update-in [:current] inc)
  (.charAt source 
           (:current @app-state)))

(defn add-token
  ([source token-type] 
   (add-token token-type nil))
  ([source token-type literal] 
   (add-token token-type literal
              (:start @app-state)
              (dec (:current @app-state))
              (:line @app-state)))
  ([source token-type literal start current line] 
   (let [text (subs source start current)]
     (swap! output-state update-in [:tokens]
            #(conj % (create-token token-type
                                 text
                                 literal
                                 line))))))

(defn scan-tokens
  [source]
  ["stubbed" "for" "now"])


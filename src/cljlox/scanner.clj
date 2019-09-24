(ns cljlox.scanner)

(defn new-app-state
  "New app state."
  []
  {:had-error false
         :start 0
         :current 0
         :line 1})

(def app-state
  "Application state."
  (atom (new-app-state)))

(defn reset-app-state!
  "Reset app-state to original state."
  []
  (reset! app-state (new-app-state)))

(defn new-output-state
  "New output state."
  []
  {:tokens []})

(def output-state
  "Output state."
  (atom (new-output-state)))

(defn reset-output-state!
  "Reset output state."
  []
  (reset! output-state (new-output-state)))


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
    (str "[" line "] Error " where
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
  ([source] 
   (is-at-end? 
    source (:current @app-state)))
  ([source current] 
   (or (nil? current)
       (>= current (count source)))))

(defn advance
  "Increments current variable and returns 
  the character of source that position."
  [source] 
  (swap! app-state update-in [:current] inc)
  (.charAt source 
           (dec (:current @app-state))))

(defn add-token
  ([source token-type] 
   (add-token source token-type nil))
  ([source token-type literal] 
   (add-token source token-type literal
              (:start @app-state)
              (:current @app-state)
              (:line @app-state)))
  ([source token-type literal start current line] 
   (let [text (subs source start current)]
     (swap! output-state update-in [:tokens]
            #(conj % (create-token token-type
                                 text
                                 literal
                                 line))))))

(defn match?
  [source current expected]
  (and (not (is-at-end? source current))
            (= expected 
               (.charAt source current))))

(defn match!
  ([source expected]
   (match! source 
           (:current 
             @app-state)
           expected))

  ([source current expected]
   (let [matched (match?
                 source current expected)]
     (if matched
       (swap! app-state update-in [:current] inc))

     matched)))

(defn peek
  ([source]
   (peek source (:current @app-state)))
  ([source current]
   (if (is-at-end? source current)
     nil
     (.charAt source current))))

(defn scan-tokens
  "Scan tokens by advancing the current position."
  [source]
  (let [c (advance source)]
    (cond
      (= c \() (add-token source :left-paren)
      (= c \)) (add-token source :right-paren)
      (= c \{) (add-token source :left-brace)
      (= c \}) (add-token source :right-brace)
      (= c \,) (add-token source :comma)
      (= c \.) (add-token source :dot)
      (= c \-) (add-token source :minus)
      (= c \+) (add-token source :plus)
      (= c \;) (add-token source :semicolon)
      (= c \*) (add-token source :star)
      (= c \!) (add-token 
                 source
                 (cond 
                   (match! source \=) :bang-equal
                   :else :bang))
      (= c \=) (add-token
                 source
                 (cond
                   (match! source \=) :equal-equal
                   :else :equal))
      (= c \<) (add-token
                 source
                 (cond
                   (match! source \=) :less-equal
                   :else :less))
      (= c \>) (add-token
                 source
                 (cond
                   (match! source \=) :greater-equal
                   :else :greater))
      (= c \/) (if (match! source \/)
                 (do 
                   (while 
                     (and 
                       (not= (peek source) \newline)
                       (not (is-at-end? source)))
                     (advance source)))
                 (add-token source :slash))
      (= c \space) (@output-state)
      (= c \return) (@output-state)
      (= c \tab) (@output-state)
      (= c \newline) (do
                       (swap! app-state update-in
                              [:line] inc)
                       @output-state)
      :else (error (:line @app-state)
                   "Unexpected character."))))


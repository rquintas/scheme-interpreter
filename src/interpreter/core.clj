(ns interpreter.core)

(defn tagged-list? [exp tag]
    (if (list? exp)
        (= (first exp) tag) 
        false))

(defn self-evaluating? [exp]
    (cond (number? exp) true
          (string? exp) true
          :else false))

(defn variable? [exp]
    (symbol? exp))
    
(defn quoted? [exp]
    (tagged-list? exp 'quote))

(defn text-of-quotation [exp]
    (second exp))

(defn assignment? [exp]
    (tagged-list? exp 'set!))
    
(defn definition? [exp]
    (tagged-list? exp 'define))
    
(defn lambda? [exp]
    (tagged-list? exp 'lambda))

(defn if? [exp]
    (tagged-list? exp 'if))

(defn begin? [exp]
    (tagged-list? exp 'begin))

(defn cond? [exp]
    (tagged-list? exp 'cond))

(defn eval [exp env]
    (cond (self-evaluating? exp) exp
          (variable? exp) nil
          (quoted? exp) (text-of-quotation exp)
          (assignment? exp) nil
          (definition? exp) nil
          (if? exp) nil
          (lambda? exp) nil
          (begin? exp) nil
          (cond? exp) nil
          ;(application? exp) nil
          :else nil))
          
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main [] 
    (foo "queijo")
    (self-evaluating? :a)
    (quoted? (list 'quote "Queijo"))
    (eval (list 'quote "CACA") (list :a)))

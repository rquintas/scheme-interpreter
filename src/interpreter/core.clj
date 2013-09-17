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
    
(defn assignment-variable [exp]
    (second exp))
    
(defn assignment-value [exp]
    (nth exp 2))
    
(defn lambda? [exp]
    (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
    (second exp))

(defn lambda-body [exp]
    (nth exp 2))

(defn make-lambda [parameters body]
    (list 'lambda parameters body))
    
(defn definition? [exp]
    (tagged-list? exp 'define))
    
(defn definition-variable [exp]
    (if (symbol? (second exp))
        (second exp)
        (nth exp 2)))
        
(defn definition-value [exp]
    (if (symbol? (second exp))
        (nth exp 2)
        (make-lambda (nth exp 2)
                     (nth exp 3))))
    


(defn if? [exp]
    (tagged-list? exp 'if))

(defn if-predicate [exp]
    (second exp))
    
(defn if-consequent [exp]
    (nth exp 2))
    
(defn if-alternative [exp]
    (if (not (null? (nth exp 3)))
        (nth exp 3)
        'false))

(defn make-if [predicate consequent alternative]
    (list 'if predicate consequent alternative))
    


(defn begin? [exp]
    (tagged-list? exp 'begin))

(defn cond? [exp]
    (tagged-list? exp 'cond))


(defn list-of-values [exps env]
    (if (no-operands? exps)
        nil
        (list (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

(defn eval-if [exp env]
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
        
(defn eval-sequence [exps env]
    (cond (last-exp? exps) (eval (first-exp exps) env)
          :else (do (eval (first-exp exps) env)
                    (eval-sequence (rest-exps exps) env))))
                    
(defn eval-assignment [exp env]
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env))

(defn eval-definition [exp env]
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env))



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

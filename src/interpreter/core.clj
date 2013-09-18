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
    (if (not (nil? (nth exp 3)))
        (nth exp 3)
        'false))

(defn make-if [predicate consequent alternative]
    (list 'if predicate consequent alternative))
    


(defn begin? [exp]
    (tagged-list? exp 'begin))

(defn cond? [exp]
    (tagged-list? exp 'cond))


(defn no-operands? [ops]
    (nil? ops))
    
(defn first-operand [ops]
    (first ops))
    
(defn rest-operands [ops]
    (rest ops))

(defn list-of-values [exps env]
    (if (no-operands? exps)
        nil
        (list (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))


(defn last-exp? [seq]
    (empty? (rest seq)))
    
(defn first-exp [seq]
    (first seq))
    
(defn rest-exps [seq]
    (rest seq))



;; Environment

(defn enclosing-environment [env]
    (rest env))
    
(defn first-frame [env]
    (first env))
    
(defn the-empty-environment []
    nil)

(defn make-frame [variables values]
    (atom (list variables values)))
    
(defn frame-variables [frame]
    (first @frame))
    
(defn frame-values [frame]
    (second @frame))
    
(defn add-binding-to-frame! [var val frame]
    (reset! frame (list (conj (first @frame) var) (conj (second @frame) (atom val)))))

(defn extend-environment [vars vals base-env]
    (if (= (count vars) (count vals))
        (conj base-env (make-frame vars vals))
        (if (< (count vars) (count vals))
            (print "Too many arguments supplied")
            (print "Too few arguments supplied"))))

(defn lookup-variable-value [var env]
    (defn env-loop [env]
        (defn scan [vars vals]
            (cond (empty? vars) (env-loop (enclosing-environment env))
                  (= var (first vars)) @(first vals)
                  :else (scan (rest vars) (rest vals))))
        (if (empty? env)
            (print "Unbound var")
            (let [frame (first-frame env)]
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))


(defn set-variable-value! [var val env]
    (defn env-loop [env]
        (defn scan [vars vals]
            (cond (empty? vars) (env-loop (enclosing-environment env))
                  (= var (first vars)) (reset! (first vals) val)
                  :else (scan (rest vars) (rest vals))))
        (if (empty? env)
            (print "Unbound var -- SET")
            (let [frame (first-frame env)]
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))
    
(defn define-variable! [var val env]
    (let [frame (first-frame env)]
        (defn scan [vars vals]
            (cond (empty? vars) (add-binding-to-frame! var val frame)
                  (= var (first vars)) (reset! (first vals) val)
                  :else (scan (rest vars) (rest vals))))
        (scan (frame-variables frame)
              (frame-values frame))))

;;; Evals

(defn eval []
    nil)

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
          (variable? exp) (lookup-variable-value exp env)
          (quoted? exp) (text-of-quotation exp)
          (assignment? exp) (eval-assignment exp env)
          (definition? exp) (eval-definition exp env)
          (if? exp) (eval-if exp env)
          (lambda? exp) nil
          ;(begin? exp) (eval-sequence (begin-actions exp) env)
          (cond? exp) nil
          ;(application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env))
          :else "Unknown expression type -- EVAL"))
          
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main [] 
    (foo "queijo")
    (self-evaluating? :a)
    (quoted? (list 'quote "Queijo"))
    (eval (list 'quote "CACA") (list :a)))

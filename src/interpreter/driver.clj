(ns interpreter.driver
    (:use interpreter.core))

(defn setup-environment []
    (let [initial-env (extend-environment (primitive-procedure-names)
                                          (primitive-procedure-objects)
                                          the-empty-environment)]
         (do
             (define-variable! 'true true initial-env)
             (define-variable! 'false false initial-env)
             initial-env)))

(def the-global-environment (setup-environment))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(defn prompt-for-input [string]
    (do 
        (print "\n")
        (print "\n")
        (print string)
        (print "\n")
        (flush)))

(defn announce-output [string]
    (do
        (print "\n")
        (print string)
        (print "\n")
        (flush)))

(defn user-print [object]
    (if (compound-procedure? object)
        (print (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
        (print object)))

(defn driver-loop []
    (do
        (prompt-for-input input-prompt)
        (let [input (read)]
            (let [output (eval input the-global-environment)]
                (announce-output output-prompt)
                (user-print output)))
        (driver-loop)))
        
(defn -main []
    ;(self-evaluating? :a)
    ;(quoted? (list 'quote "Queijo"))
    ;(eval (list '+ 1 1) (list :a))
    (driver-loop))
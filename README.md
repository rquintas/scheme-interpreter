# interpreter

A Scheme interpreter implemented in Clojure, following SICP Chapter 4. http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1 

## Usage

lein run

Once in the REPL it can evaluate expressions like:

(+ 40 2)
(- 48 6)
(* 6 7)
(/ 168 4)

(define x 21)
(+ x x)

(set! x 10)
(+ (* x 4) 2)

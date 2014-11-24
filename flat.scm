(module flat ()
(import chicken scheme) (use extras)
(import pat)
(import atomic builtins)

;; The FLAT intermediate representation has the following syntax

;; <flat> ::= <def>*
;;
;; <def> ::= (define (<function> <symbol>*) <body>)
;;
;; <body> ::= <ref>* <stmt>* (return <exp>)
;;
;; <ref> ::= (increment-refcount <symbol> <int>)
;;         | (decrement-refcount <symbol>)
;;
;; <stmt> ::= (if <exp> <stmt> <stmt>)
;;          | (set! <symbol> <exp>)
;;          | <exp>
;;
;; <exp> ::= <symbol> | <atomic>
;;         | (<function> <exp>*)

)

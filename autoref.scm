(module autoref ()
(import chicken scheme) (use extras)
(import pat)
(import atomic builtins verify)

;; An AUTOREF program has the following syntax

;; <autoref> ::= <def>*
;;
;; <def> ::= (define (<function> <symbol>*) <exp>)
;;
;; <exp> ::= <symbol> | <atomic>
;;         | (begin <exp>*)
;;         | (if <exp> <exp> <exp>)
;;         | (<function> <exp>*)
;;
;; <atomic> ::= <number> | <boolean> | <character> | <string>

;; There are a number of built-in functions

(define (main)
  (let ((file-name (car (command-line-arguments))))
    (let ((content (read-file file-name)))
      (verify-program content))))

(main)

)

(module autoref ()
(import chicken scheme) (use extras)
(import pat)

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

(define (atomic? t)
  (or (number? t) (boolean? t) (char? t) (string? t)))

;; There are a number of built-in functions

(define built-ins '())

(define (verify-exp t)
  (match t
    (`(begin . ,exps) => (begin (print "verifying begin")
                                (for-each verify-exp exps)))
    (else (if (or (symbol? t) (atomic? t))
              (print "OK!")
              (error (list "Invalid exp!" t))))))

(define (main)
  (let ((file-name (car (command-line-arguments))))
    (let ((content (read-file file-name)))
      (for-each verify-exp content))))

(main)

)

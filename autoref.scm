(module autoref ()
(import chicken scheme) (use extras srfi-1)
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
    (`(if ,p ,t ,e) => (begin (print "verifying if")
                              (for-each verify-exp (list p t e))))
    (`(,f . ,args) => (begin (print "verifying application")
                             (for-each verify-exp t)))
    (else (if (or (symbol? t) (atomic? t))
              (print "OK!")
              (error (list "Invalid exp!" t))))))

(define (verify-def d)
  (print "verifying definition")
  (match d
    (`(define (,name . ,args) ,body) =>
     (begin (unless (every symbol? (cons name args))
              (error (list "Invalid definition!" d)))
            (verify-exp body)))
    (else (error(list "Invalid exp!" d)))))

(define (verify-program p)
  (for-each verify-def p))

(define (main)
  (let ((file-name (car (command-line-arguments))))
    (let ((content (read-file file-name)))
      (verify-program content))))

(main)

)

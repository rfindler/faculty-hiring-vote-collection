#lang racket
(require "threaded-store.rkt" net/url)
(provide
 (contract-out
  [votes-page
   (-> string? any/c)]))

(define proposals
  (list "Bio-inspired Robotics - Brenna presenting"
        "Conversational AI - Ken presenting"
        "Critical Networked Systems - Fabian presenting"
        "Parallel Systems - Peter presenting"
        "Quantum - Nikos presenting"
        "Vision - Mike presenting"))

(define (build-url code)
  (url->string
   (make-url "http" ;; scheme
             #f ;; user
             #f ;; host
             #f ;; port
             #t ;; absolute
             (list (path/param "vote" '())
                   (path/param code '()))
             '()
             #f)))

(define (votes-page code)
  (define current (get code))
  `(html
    (head (title "Vote for " ,code))
    (body
     (h1 "Vote for " (tt ,code))
     (form ((action ,(build-url code)))
           (table
            ,@(for/list ([p (in-list proposals)])
                `(tr (td ,p)
                     (td (input ((size "100")
                                 (type "text")
                                 (value "")
                                 (onchange "this.form.submit()")
                                 (id ,p)
                                 (name ,p)))))))))))

#|
     )))


      (form ((action ,(build-url #f #f #f)))
            (div
             "Instructor"
             (select ((name "instructor") (id "instructor") (onchange "this.form.submit()"))
                     (option ((value "--")
                              ,@(if instructor (list) (list `(selected "yes"))))
                             "--")
                     ,@(for/list ([name (in-list (all-instructions-that-teach-at-least-one-CS-class))])
                         `(option ((value ,(~a name))
                                   ,@(add-selected-if (equal? instructor name)))
                                  ,(~a name))))))

      (define (text-field description lab selector)
    `(div ,@(if (list? description) description (list description)) (br)
          (input ((size "100")
                  (type "text")
                  (value ,(or (selector class-status) ""))
                  (onchange "this.form.submit()")
                  (id ,lab) (name ,lab)))))

|#
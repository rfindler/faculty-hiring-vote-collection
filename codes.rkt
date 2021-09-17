#lang racket
(require "ids.rkt" "faculty.rkt")

(provide codes-page)

(define (codes-page)
  (define ids (gen-ids))
  `(html
    (head (title "New Codes"))
    (body (table
           ,@(for/list ([faculty (in-list all-faculty)])
               `(tr (td ,faculty)
                    (td (tt ,(get-id faculty ids)))))))))
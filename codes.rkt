#lang racket
(require "ids.rkt" "faculty.rkt")

(provide codes-page)

(define (codes-page)
  (define ids (gen-ids))
  `(html
    (head (title "New Codes"))
    (body (table
           ,@(for/list ([faculty (in-list all-faculty)])
               (define id (get-id faculty ids))
               `(tr (td ,faculty)
                    (td
                     (a ((href ,(~a "/vote/" id)))
                        (tt ,id)))))))))
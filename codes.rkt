#lang racket
(require "ids.rkt" "faculty.rkt")

(provide codes-page current-port)

(define current-port (make-parameter #f))

(define (codes-page)
  (define ids (gen-ids))
  `(html
    (head (title "New Codes"))
    (body (table
           ,@(for/list ([faculty (in-list (sort all-faculty string<?))])
               (define id (get-id faculty ids))
               `(tr (td ,faculty)
                    (td
                     (a ((href ,(~a "/vote/" id)))
                        (tt ,(~a "http://penghu.eecs.northwestern.edu:"
                                 (current-port)
                                 "/vote/" id))))))))))

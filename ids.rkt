#lang racket
(require "faculty.rkt")

(provide
 gen-ids
 (contract-out
  [get-id (-> (or/c tenure-track? teaching-track?) ids? string?)]))

(define digits
  (list->vector
   (string->list
    "123456789abcdefghijklmnopqrstuvwxyz")))

(define (make-an-id)
  (apply
   string
   (for/list ([i (in-range 20)])
     (vector-ref digits (random (vector-length digits))))))

(define all-ids
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 0)
    (for/list ([e (in-list all-faculty)])
      (make-an-id))))

(struct ids (ids))
(define (gen-ids) (ids (apply vector (shuffle all-ids))))
(define (get-id faculty ids)
  (define raw-id (vector-ref (ids-ids ids) (faculty->n faculty)))
  (cond
    [(teaching-track? faculty)
     (string-append "a" raw-id)]
    [(tenure-track? faculty)
     (string-append "z" raw-id)]))

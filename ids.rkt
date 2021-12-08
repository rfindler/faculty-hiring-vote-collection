#lang racket
(require "faculty.rkt")
(module+ test (require rackunit))

(provide
 gen-ids
 build-all-ids
 (contract-out
  [get-id (-> faculty-member? ids? string?)]
  [id->area (-> string? symbol?)]))

(define digits
  (list->vector
   (string->list
    "123456789abcdefghijklmnopqrstuvwxyz")))

(define (make-an-id)
  (apply
   string
   (for/list ([i (in-range 20)])
     (vector-ref digits (random (vector-length digits))))))

(define all-ids #f)

(define (build-all-ids seed)
  (set! all-ids
        (parameterize ([current-pseudo-random-generator
                        (make-pseudo-random-generator)])
          (random-seed seed)
          (for/list ([e (in-list all-faculty)])
            (make-an-id)))))

(struct ids (ids))
(define (gen-ids)
  (unless all-ids (error 'gen-ids "seed not set"))
  (ids (apply vector (shuffle all-ids))))
(define (get-id faculty ids)
  (string-append 
   (get-area-prefix faculty)
   (vector-ref (ids-ids ids) (faculty->n faculty))))

(define (get-area-prefix faculty)
  (~a (integer->char
       (+ (char->integer #\A) (faculty->area-tag faculty)))))

(define (id->area id)
  (area-tag->area
   (- (char->integer (string-ref id 0))
      (char->integer #\A))))

(module+ test
  (check-equal? (id->area
                 (get-area-prefix "Larry Birnbaum"))
                'ai)
  (check-equal? (id->area
                 (get-area-prefix "Kate Compton"))
                'teaching))

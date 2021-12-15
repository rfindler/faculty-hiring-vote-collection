#lang racket
(require racket/runtime-path)
(define-runtime-path votes.csv "votes.csv")
(require "threaded-store.rkt" "ids.rkt")

(define vote (get-all))

(define (send-line args)
  (for ([arg (in-list args)]
        [i (in-naturals)])
    (unless (zero? i) (display ","))
    (define str
      (cond
        [(number? arg) (~r arg)]
        [(boolean? arg) (if arg "true" "false")]
        [(string? arg) arg]
        [(symbol? arg) (~a arg)]
        [else (error 'send-line "unknown thing to send ~s" arg)]))
    (display str))
  (newline))

(define keys (map second proposals+ids))

(define (to-boolean x)
  (match x
    ["on" #t]
    [#t x]
    [#f x]))

(define (main)
  (send-line (list* "id" "area" "noopinion" keys))
  (for ([id (in-list (sort (hash-keys vote) string<? #:key ~s))])
    (define area (id->area id))
    (cond
      [area
       (define this-vote (hash-ref vote id))
       (send-line (list* id
                         area
                         (to-boolean (hash-ref this-vote "noopinion" #f))
                         (for/list ([key (in-list keys)])
                           (hash-ref this-vote key ""))))]
      [else
       (eprintf "discarding vote from id ~a" id)])))

(module+ main
  (call-with-output-file votes.csv
    (λ (port)
      (parameterize ([current-output-port port])
        (main)))
    #:exists 'truncate))

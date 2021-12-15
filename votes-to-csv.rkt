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
        [(number? arg) (~a arg)]
        [(boolean? arg) (if arg "true" "false")]
        [(string? arg) arg]
        [(symbol? arg) (~a arg)]
        [else (error 'send-line "unknown thing to send ~s" arg)]))
    (display str))
  (newline))

(define keys (cons "noopinion" (map second proposals+ids)))

(define (main)
  (send-line (list* "id" "area" keys))
  (for ([id (in-list (sort (hash-keys vote) string<? #:key ~s))])
    (define area (id->area id))
    (cond
      [area
       (send-line (list* id
                         area
                         (for/list ([key (in-list keys)])
                           (hash-ref (hash-ref vote id) key ""))))]
      [else
       (eprintf "discarding vote from id ~a" id)])))

(module+ main
  (call-with-output-file votes.csv
    (λ (port)
      (parameterize ([current-output-port port])
        (main)))
    #:exists 'truncate))

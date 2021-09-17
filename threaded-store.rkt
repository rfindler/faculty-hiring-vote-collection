#lang racket
(require racket/runtime-path)

(provide
 (contract-out
  [get (-> string? any/c)]
  [set (-> string? any/c void?)]))

(define (get s)
  (define c (make-channel))
  (channel-put get-chan (cons s c))
  (channel-get c))

(define (set s v)
  (define c (make-channel))
  (channel-put set-chan (cons s v)))

(define get-chan (make-channel))
(define set-chan (make-channel))

(define-runtime-path votes.rktd "votes.rktd")
(define-runtime-path votes.rktd.old "votes.rktd.old")
(define (read-from-file)
  (if (file-exists? votes.rktd)
      (call-with-input-file votes.rktd read)
      (hash)))
(define (write-to-file hash)
  (when (file-exists? votes.rktd)
    (copy-file votes.rktd votes.rktd.old #t))
  (call-with-output-file votes.rktd
    (λ (port)
      (write hash port)
      (newline port))
    #:exists 'truncate))

(void
 (thread
  (λ ()
    (let loop ()
      (sync
       (wrap-evt
        get-chan
        (λ (s+c)
          (match-define (cons s c) s+c)
          (channel-put c (hash-ref (read-from-file) s #f))
          (loop)))
       (wrap-evt
        set-chan
        (λ (s+v)
          (match-define (cons s v) s+v)
          (write-to-file (hash-set (read-from-file) s v))
          (loop))))))))

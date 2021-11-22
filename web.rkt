#lang racket
(require web-server/servlet
         web-server/servlet-env
         "faculty.rkt"
         "codes.rkt"
         "vote.rkt")
(module+ test (require rackunit))

(define (dispatch-request req)
  (cond
    [(equal? "new-codes" (extract-path-param req 0))
     (show-codes-page)]
    [(equal? "vote" (extract-path-param req 0))
     (show-votes-page (extract-path-param req 1) (request-bindings/raw req))]
    [else
     (response/xexpr
      `(html
        (head)
        (body (h1 "404"))))]))

(define servlet-regexp #rx"^/(new-codes)|(vote)")

(define (show-codes-page)
  (response/xexpr
   (codes-page)))

(define (show-votes-page code bindings)
  (if (string? code)
      (response/xexpr (votes-page code bindings))
      (response/xexpr `(html (head) (body "Unknown code " ,(~a code))))))

(define (extract-path-param req i)
  (define path (url-path (request-uri req)))
  (cond
    [(< i (length path))
     (define pth (path/param-path (list-ref path i)))
     (cond
       [(regexp-match #rx"^-+$" pth) #f]
       [else pth])]
    [else #f]))

(module+ main
  (require racket/runtime-path racket/cmdline "ids.rkt")
  (define port-as-string
    (command-line #:args (port seed) port))
  (define port (or (string->number port-as-string) 8532))
  (define seed (or (string->number port-as-string) 0))
  (current-port port)
  (build-all-ids seed)
  
  (define-runtime-path fonts "fonts")
  (printf "starting site\n")
  (serve/servlet dispatch-request
                 #:listen-ip #f
                 #:servlet-regexp servlet-regexp
                 #:extra-files-paths (list "img" fonts)
                 #:command-line? #t
                 #:port port))

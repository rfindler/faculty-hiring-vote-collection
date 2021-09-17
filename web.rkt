#lang racket
(require web-server/servlet
         web-server/servlet-env
         "faculty.rkt"
         "codes.rkt"
         "vote.rkt")
(module+ test (require rackunit))

(define (dispatch-request req)
  ;(printf "req ~a\n" (url->string (request-uri req))) (pretty-write (request-bindings/raw req)); (pretty-write req)
  (cond
    [(equal? "new-codes" (extract-path-param req 0))
     (show-codes-page)]
    [(equal? "vote" (extract-path-param req 0))
     (show-votes-page (extract-path-param req 1))]
    [else
     (response/xexpr
      `(html
        (head)
        (body (h1 "404"))))]))

(define servlet-path "class-schedule")
(define servlet-regexp #rx"^/(new-codes)|(vote)")

(define (req->vote req)
  (car #(class-status (req->checkbox-hash req all-slots)
                (req->str/f req "phd")
                (req->checkbox-hash req all-classes)
                (req->str/f req "cross")
                (req->str/f req "garage")
                (req->str/f req "labs"))))

(define (req->checkbox-hash req options)
  (for/hash ([slot (in-list options)]
             #:when (extract-binding req (string->bytes/utf-8 slot)))
    (values slot #t)))

(define (req->str/f req field)
  (define str (extract-binding req (string->bytes/utf-8 field)))
  (cond
    [(or (not str) (regexp-match #rx"^ *$" str)) #f]
    [else str]))

(define (show-codes-page)
  (response/xexpr
   (codes-page)))

(define (show-votes-page code)
  (response/xexpr
   (votes-page
    code)))

(define (extract-path-param req i)
  (define path (url-path (request-uri req)))
  (cond
    [(< i (length path))
     (define pth (path/param-path (list-ref path i)))
     (cond
       [(regexp-match #rx"^-+$" pth) #f]
       [else pth])]
    [else #f]))

(define (hyphens->false-otherwise-bytes->string/utf-8 bytes)
  (cond
    [(regexp-match #rx"^[-]+$" bytes) #f]
    [else (bytes->string/utf-8 bytes)]))

(define (extract-binding req what [convert hyphens->false-otherwise-bytes->string/utf-8])
  (define b
    (bindings-assq
     what
     (request-bindings/raw req)))
  (cond
    [(binding:form? b)
     (convert (binding:form-value b))]
    [else #f]))

(define (build-url instructor class status)
  (url->string
   (make-url "http" ;; scheme
             #f ;; user
             #f ;; host
             #f ;; port
             #t ;; absolute
             (append
              (list (path/param servlet-path '()))
              (if instructor (list (path/param (~a instructor) '())) '())
              (if class (list (path/param (~a class) '())) '()))
             '()
             #f)))



(module+ main
  (require racket/runtime-path)
  (define-runtime-path fonts "fonts")
  (printf "starting site\n")
  (serve/servlet dispatch-request
                 #:listen-ip #f
                 #:servlet-regexp servlet-regexp
                 #:extra-files-paths (list "img" fonts)
                 #:command-line? #t
                 #:port 8888))

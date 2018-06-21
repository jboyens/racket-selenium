#lang racket

(require "struct.rkt"
         uri-template
         racket/hash)

(provide base-url
         current-session
         format-url)

(define base-url "http://localhost:4444/wd/hub")

(define current-session (make-parameter null))

(define (format-url url [extra-args #hash()])
  (let ([template (cond [(string-prefix? url "/") (string-append "{+base}" url)]
                        [else url])]

        [uri-defaults (hash "base" base-url
                            "sessionid" (session-id (current-session)))])
    (expand-template template (hash-union uri-defaults extra-args #:combine/key (λ (k v1 v2) v2)))))

(module+ test
  (require rackunit)

  (parameterize ([current-session (session "fake-id")])
    (test-case "URL Formatting"
      (check-equal? (format-url "/session") (string-append base-url "/session"))
      (check-equal? (format-url "http://bob") "http://bob")
      (check-equal? (format-url "/session" (hash "base" "http://google.com")) "http://google.com/session")
      (check-equal? (format-url "/session/{danish}" (hash "base" "http://google.com" "danish" "cheese"))
                    "http://google.com/session/cheese"))))

#lang racket

(require "json.rkt" "session.rkt" net/cookies json)

(provide get-session-cookies
         cookie-parse
         cookies->header)


(define (get-session-cookies)
  (cookie-parse (json-get (format "/session/~a/cookie" (session-id (current-session))))))


(define (cookie-parse json)
  (define (build-cookie c)
    (make-cookie
     (hash-ref c 'name)
     (hash-ref c 'value)
     #:expires (hash-ref c 'expires #f)
     #:max-age (hash-ref c 'maxAge #f)
     #:domain (hash-ref c 'domain #f)
     #:path (hash-ref c 'path #f)
     #:secure? (hash-ref c 'secure #f)
     #:http-only? (hash-ref c 'httpOnly #f)))

  (map build-cookie (hash-ref json 'value)))


(define (cookies->header cookies [encode string->bytes/utf-8])
  (define (make-cookie-pair c)
    (bytes-append (encode (cookie-name c))
                  #"=" (encode (cookie-value c))))
  (define cookie-pairs
    (for/list ([c (in-list cookies)])
      (make-cookie-pair c)))
  (and (not (null? cookie-pairs)) (bytes-join cookie-pairs #"; ")))

(module+ test
  (require rackunit mock/rackunit)
  (parameterize ([current-session (session "fake-id")])

    (test-case "Test cookie parsing"
      (define json-response "{\"sessionId\": \"placeguidhere\",\"status\": 0,\"value\": [{\"domain\": \"example.com\",\"httpOnly\": false,\"name\": \"response_timestamp\",\"path\": \"/\",\"secure\": false,\"value\": \"0\"},{\"domain\": \"example.com\",\"expiry\": 1526802259.103884,\"httpOnly\": true,\"name\": \"uid\",\"path\": \"/\",\"secure\": true,\"value\": \"superawesomevalue\"},{\"domain\": \"example.com\",\"expiry\": 1526802259.103794,\"httpOnly\": true,\"name\": \"superawesomevalue\",\"path\": \"/\",\"secure\": true,\"value\": \"valuevaluevalue\"}]}")

      (define json (string->jsexpr json-response))

      (check-equal?
       (cookie-parse json)
       (list
        (make-cookie "response_timestamp" "0"
                     #:domain "example.com"
                     #:path "/")
        (make-cookie "uid" "superawesomevalue"
                     #:domain "example.com"
                     #:path "/"
                     #:http-only? #t
                     #:secure? #t)
        (make-cookie "superawesomevalue"
                     "valuevaluevalue"
                     #:domain "example.com"
                     #:path "/"
                     #:http-only? #t
                     #:secure? #t)
        )))))

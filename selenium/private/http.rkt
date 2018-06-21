#lang racket

(require http/request
         net/cookies/user-agent
         net/cookies/server
         net/head
         net/url
         json
         "utils.rkt"
         "struct.rkt")

(provide http-request
         json-post
         json-get
         json-delete
         json-request
         get-session-cookies
         cookie-parse
         cookies->header)

(define
  (http-request
   uri
   #:redirects [redirects 10]
   #:http-version [http-version "1.1"]
   #:method [method "get"]
   #:data [data #""]
   #:data-length [data-length #f]
   #:heads [headers empty]
   #:save-cookies? [save-cookies #f]
   #:entity-reader [entity-reader read-entity/bytes])

  (define input-request?
    (and (bytes? data)
         (bytes=? data #"")
         (eq? data-length #f)))

  (define (wrapped-entity-reader port headers)
    (when save-cookies
        (extract-and-save-cookies! (extract-all-fields (string->bytes/utf-8 headers)) (string->url uri)))
    (entity-reader port headers))

  (cond (input-request?
         (call/input-request http-version method uri headers wrapped-entity-reader #:redirects redirects))
        (else
         (call/output-request http-version method uri data data-length headers wrapped-entity-reader #:redirects redirects))))

(define (json-post url #:json [json #f])
  (json-request "post" url #:json json))


(define (json-get url)
  (json-request "get" url))

(define (json-get/formatted url)
  (json-get (format-url url)))


(define (json-delete url)
  (json-request "delete" url))


(define (json-request method url #:json [json #f])
  (define data
    (cond
      [(not (false? json)) (string->bytes/utf-8 (jsexpr->string json))]
      [else #""]))

  (define headers (list (cons "Content-Type" "application/json;charset=utf-8")))
  (define response (http-request url
                                 #:method method
                                 #:data data
                                 #:heads headers))
  (display (format "~n~a ~a~nDATA: ~a~nRESPONSE: ~a~n~n" (string-upcase method) url data response))
  (string->jsexpr (bytes->string/utf-8 response)))

(define (get-session-cookies)
  (cookie-parse (json-get (format-url "/session/{sessionid}/cookie"))))


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

      (define json (hash 'sessionId "placeguidhere"
                         'status 0
                         'value (list (hash 'domain "example.com"
                                            'httpOnly #f
                                            'name "response_timestamp"
                                            'path "/"
                                            'secure #f
                                            'value "0")
                                      (hash 'domain "example.com"
                                            'expiry 1526802259.103884
                                            'httpOnly #t
                                            'name "uid"
                                            'path "/"
                                            'secure #t
                                            'value "superawesomevalue")
                                      (hash 'domain "example.com"
                                            'expiry 1526802259.103794
                                            'httpOnly #t
                                            'name "superawesomevalue"
                                            'path "/"
                                            'secure #t
                                            'value "valuevaluevalue")
                                      )))

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

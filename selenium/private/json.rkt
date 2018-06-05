#lang racket

(require http/request net/cookies net/head net/url json)

(provide http-request
         json-post
         json-get
         json-delete
         json-request)

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
    (if save-cookies
        (extract-and-save-cookies! (extract-all-fields (string->bytes/utf-8 headers)) (string->url uri))
        (void))
    (entity-reader port headers))

  (cond (input-request?
         (call/input-request http-version method uri headers wrapped-entity-reader #:redirects redirects))
        (else
         (call/output-request http-version method uri data data-length headers wrapped-entity-reader #:redirects redirects))))

(define (json-post url #:json [json #f])
  (json-request "post" url #:json json))


(define (json-get url)
  (json-request "get" url))


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

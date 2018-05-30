#lang racket

(require http/request http/head net/head net/cookies net/url)
(require json)

(provide with-session
         implicit-wait-time!
         open-url
         fill-in
         click
         find-element
         find-element/by-link-text
         find-element/by-xpath
         find-element/by-class-name
         find-element/by-id
         find-element/by-name
         find-element/by-partial-link-text
         find-element/by-tag-name
         download-link
         get-element-attribute)

(provide (contract-out
          [struct session ((id string?))]
          [struct element ((id string?))]
          [create-session (-> session?)]))

(define chromedriver "http://localhost:9515")

(struct session (id) #:transparent)
(struct element (id))

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
  (define response (http-request (string-append chromedriver url)
                                 #:method method
                                 #:data data
                                 #:heads headers))
  (display (format "~n~a ~a~nDATA: ~a~nRESPONSE: ~a~n~n" (string-upcase method) url data response))
  (string->jsexpr (bytes->string/utf-8 response)))


(define current-session (make-parameter null))


(define (create-session)
  (let ([response (json-post "/session"
                             #:json #hasheq((desiredCapabilities . #hasheq())))])
    (session (hash-ref response 'sessionId))))


(define (delete-session! session)
  (void (json-delete (format "/session/~a" (session-id session)))))


(define (delete-all-sessions!)
  (let ([ids (map (λ (item) (hash-ref item 'id))
                  (hash-ref (json-get "/sessions") 'value))])
    (for-each (λ (id) (json-delete (format "/session/~a" id))) ids)))


(define (with-session body)
  (parameterize ([current-session (create-session)])
    (body)
    (delete-session! (current-session))
    ))

;; (define-syntax-rule (with-session body)
;;   (parameterize ([current-session (create-session)])
;;     (body)
;;     ;; (delete-session! (current-session))
;;     void
;;     ))

(define (find-element selector #:by [by "css selector"])
  (define response (http-request (format "http://localhost:9515/session/~a/element" (session-id (current-session)))
                                 #:method "post"
                                 #:data (string->bytes/utf-8 (jsexpr->string (hash 'using by 'value selector)))
                                 #:heads (list (cons "Content-Type" "application/json;charset=utf-8"))))
  (element (hash-ref (hash-ref (string->jsexpr (bytes->string/utf-8 response)) 'value) 'ELEMENT)))


(define (find-element/by-id id)
  (find-element id #:by "id"))


(define (find-element/by-xpath xpath)
  (find-element xpath #:by "xpath"))


(define (find-element/by-class-name name)
  (find-element name #:by "class name"))


(define (find-element/by-name name)
  (find-element name #:by "name"))


(define (find-element/by-link-text text)
  (find-element text #:by "link text"))


(define (find-element/by-partial-link-text text)
  (find-element text #:by "partial link text"))


(define (find-element/by-tag-name name)
  (find-element name #:by "tag name"))


(define (get-element-attribute elem attrib)
  (define json
    (json-get
     (format "/session/~a/element/~a/attribute/~a"
             (session-id (current-session))
             (element-id elem)
             attrib)))
  (hash-ref json 'value))


(define (fill-in elem value)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format "/session/~a/element/~a/value" (session-id (current-session)) (element-id e))
   #:json (hash 'value (map string (string->list value)))))


(define (click elem)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format "/session/~a/element/~a/click"
           (session-id (current-session))
           (element-id e))))


(define (open-url url)
  (let ([session-url (format "/session/~a/url" (session-id (current-session)))])
    (json-post session-url #:json (hash 'url url))))


(define (implicit-wait-time! ms)
  (http-request (format "http://localhost:9515/session/~a/timeouts/implicit_wait" (session-id (current-session)))
                #:method "post"
                #:data (string->bytes/utf-8 (jsexpr->string (hash 'ms ms)))
                #:heads (list (cons "Content-Type" "application/json;charset=utf-8"))))


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


(define (current-url)
  (define json (json-get (format "/session/~a/url" (session-id (current-session)))))
  (hash-ref json 'value))


(define (download-link href)
  (http-request href #:heads (list (cons "Cookie" (cookies->header (get-session-cookies)))
                                   (cons "Referer" (current-url)))))


(module+ test
  (require rackunit)

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
      ))))

#lang racket

(require mock
         json
         uri-template
         racket/hash
         net/cookies
         http/request
         net/cookies
         net/head
         net/url
         json)

(provide click
         cookie-parse
         cookies->header
         create-session
         current-session
         delete-all-sessions!
         delete-session!
         download-link
         fill-in
         find-element
         find-element/by-class-name
         find-element/by-id
         find-element/by-link-text
         find-element/by-name
         find-element/by-partial-link-text
         find-element/by-tag-name
         find-element/by-xpath
         get-element-attribute
         get-session-cookies
         http-request
         implicit-wait-time!
         json-delete
         json-get
         json-post
         json-request
         open-url
         (struct-out element)
         (struct-out session)
         update-configuration
         with-session)

;; (provide (contract-out
;;           [struct session ((id string?))]
;;           [struct element ((id string?))]
;;           [create-session (-> session?)]))

(define (open-url url)
  (let ([session-url (format-url "/session/{sessionid}/url")])
    (json-post session-url #:json (hash 'url url))))


(define/mock (implicit-wait-time! ms)
  #:mock json-post

  (json-post (format-url "/session/{sessionid}/timeouts/implicit_wait") #:json (hash 'ms ms)))

(define/mock (current-url)
  #:mock json-get #:as json-get-mock

  (hash-ref (json-get (format-url "/session/{sessionid}/url")) 'value))


(define/mock (download-link href)
  #:mock http-request
  #:mock get-session-cookies
  #:mock current-url

  (http-request href #:heads (list (cons "Cookie" (cookies->header (get-session-cookies)))
                                   (cons "Referer" (current-url)))))


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


(define (find-element selector #:by [by "css selector"])
  (define response (json-post (format-url "/session/{sessionid}/element")
                              #:json (hash 'using by 'value selector)))
  (element (hash-ref (hash-ref response 'value) 'ELEMENT)))


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
     (format-url "/session/{sessionid}/element/{elementid}/attribute/{attributeid}"
                 (hash "elementid" (element-id elem)
                       "attributeid" attrib))))
  (hash-ref json 'value))


(define (fill-in elem value)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format-url "/session/{sessionid}/element/{elementid}/value" (hash "elementid" (element-id e)))
   #:json (hash 'value (map string (string->list value)))))


(define (click elem)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format-url "/session/{sessionid}/element/{elementid}/click"
           (hash "elementid" (element-id e)))))

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

(define *desiredCapabilities* #hasheq((desiredCapabilities . #hasheq((browserName . "chrome")))))

(struct session (id) #:transparent)
(struct element (id))

(define (update-configuration #:browser [browser "chrome"])
  (set! *desiredCapabilities* (hash 'desiredCapabilities (hash 'browserName browser)))
  (unless (empty? (current-session))
    (delete-session! (current-session))))

(define current-session (make-parameter null))

(define/mock (create-session)
  #:mock json-post #:as json-post-mock

  (let ([response (json-post (string-append base-url "/session")
                             #:json *desiredCapabilities*)])
    (session (hash-ref response 'sessionId))))


(define/mock (delete-session! session)
  #:mock json-delete

  (void (json-delete (format-url "/session/{sessionid}" (hash "sessionid" (session-id session))))))


(define/mock (delete-all-sessions!)
  #:mock json-get
  #:mock json-delete

  (let ([ids (map (λ (item) (hash-ref item 'id))
                  (hash-ref (json-get (format "~a/sessions" base-url)) 'value))])
    (for-each (λ (id) (json-delete (format "~a/session/~a" base-url id))) ids)))


(define (with-session body)
  (parameterize ([current-session (create-session)])
    (body)
    (delete-session! (current-session))))


(define base-url "http://localhost:4444/wd/hub")

(define (format-url url [extra-args #hash()])
  (let ([template (cond [(string-prefix? url "/") (string-append "{+base}" url)]
                        [else url])]

        [uri-defaults (hash "base" base-url
                            "sessionid" (session-id (current-session)))])
    (expand-template template (hash-union uri-defaults extra-args #:combine/key (λ (k v1 v2) v2)))))



(module+ test
  (require rackunit mock/rackunit net/cookies)
  (parameterize ([current-session (session "fake-id")])
    (test-case "Test current-url"
      (with-mocks current-url
        (define mock-return (const (hash 'value "https://localhost")))

        (with-mock-behavior ([json-get-mock mock-return])
          (check-equal? (current-url) "https://localhost"))))


    (test-case "Test implicit-wait-time!"
      (with-mocks implicit-wait-time!
        (with-mock-behavior ([json-post void/kw])
          (implicit-wait-time! 250)
          (check-mock-called-with? json-post (arguments "http://localhost:4444/wd/hub/session/fake-id/timeouts/implicit_wait" #:json (hash 'ms 250))))))

    (test-case "Download-link"
      (test-case "- without cookies"
        (with-mocks download-link
          (with-mock-behavior ([http-request (const (string->bytes/utf-8 "contents"))]
                               [get-session-cookies (const empty)]
                               [current-url (const "https://localhost")])
            (download-link "href")
            (check-mock-called-with? http-request (arguments "href" #:heads '(("Cookie" . #f) ("Referer" . "https://localhost")))))))

      (test-case "- with cookies"
        (with-mocks download-link
          (with-mock-behavior ([http-request (const (string->bytes/utf-8 "contents"))]
                               [get-session-cookies (const (list (make-cookie "a" "b")))]
                               [current-url (const "https://localhost")])
            (download-link "href")
            (check-mock-called-with? http-request (arguments "href" #:heads '(("Cookie" . #"a=b") ("Referer" . "https://localhost")))))))

      (test-case "Test create-session"
        (with-mocks create-session
          (define mock-return (const (hash 'sessionId "random-session-id")))

          (with-mock-behavior ([json-post-mock mock-return])
            (check-equal? (session-id (create-session)) "random-session-id")
            (check-mock-called-with? json-post-mock (arguments "http://localhost:4444/wd/hub/session" #:json *desiredCapabilities*)))))

      (test-case "Test delete-session!"
        (with-mocks delete-session!
          (with-mock-behavior ([json-delete void])
            (delete-session! (session "session-id"))
            (check-mock-called-with? json-delete (arguments "http://localhost:4444/wd/hub/session/session-id")))))

      (test-case "Test delete-all-sessions!"
        (with-mocks delete-all-sessions!
          (with-mock-behavior ([json-get (const (hash 'value
                                                      (list (hash 'id "session-1")
                                                            (hash 'id "session-2")
                                                            (hash 'id "session-3"))))]
                               [json-delete void])
            (delete-all-sessions!)
            (check-mock-called-with? json-get (arguments "/sessions"))
            (check-mock-calls json-delete (list
                                           (arguments "http://localhost:4444/wd/hub/session/session-1")
                                           (arguments "http://localhost:4444/wd/hub/session/session-2")
                                           (arguments "http://localhost:4444/wd/hub/session/session-3"))))))

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
          )))

      (test-case "URL Formatting"
        (check-equal? (format-url "/session") (string-append base-url "/session"))
        (check-equal? (format-url "http://bob") "http://bob")
        (check-equal? (format-url "/session" (hash "base" "http://google.com")) "http://google.com/session")
        (check-equal? (format-url "/session/{danish}" (hash "base" "http://google.com" "danish" "cheese"))
                      "http://google.com/session/cheese"))
      )))

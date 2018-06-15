#lang info

(define collection "selenium")
(define version "0.1")
(define deps '("base" "mock" "net-cookies-lib" "http" "uri-template"))
(define build-deps '("rackunit-lib" "mock-rackunit"))
(define implies '())
(define compile-omit-paths '())
(define test-omit-paths '())
(define pkg-desc "Selenium library for Racket")
(define pkg-authors '(jboyens))

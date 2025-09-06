(define-module (rosenthal packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  #:use-module (rosenthal utils cargo)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages version-control))

(define-public go-1.25
  (package
    (inherit go-1.24)
    (name "go")
    (version "1.25.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pc6ybdsd2v6rviylmmdzns3v0ramrcbhn935ikff39shpij4xp4"))))
    ;; TODO
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.24)
       ((#:tests? _ #t) #f)))))

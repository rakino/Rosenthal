;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils services)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-35)
  ;; Utilities
  #:use-module (guix discovery)
  #:use-module (guix ui)
  ;; Guix System - services
  #:use-module (gnu services)
  #:export (%rosenthal-service-module-path
            all-rosenthal-services

            modify-services/by-name))

(define %rosenthal-root-directory
  ;; This is like %distro-root-directory from (gnu packages), with adjusted
  ;; paths.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("rosenthal/packages/binaries.scm" rosenthal/ packages/)
         ("rosenthal/packages.scm" rosenthal/))))

(define %rosenthal-service-module-path
  `((,%rosenthal-root-directory . "rosenthal/services")
    (,%rosenthal-root-directory . "rosenthal/home/services")))

(define (all-rosenthal-services)
  (fold-service-types cons
                      '()
                      (all-modules %rosenthal-service-module-path
                                   #:warn warn-about-load-error)))


;;;
;;; modify-services/by-name
;;;

;; Adapted from (gnu services).

(define-syntax clause-alist
  (syntax-rules (=> delete)
    ((_ (delete name) rest ...)
     (cons (list name
                 (lambda (service)
                   #f)
                 (current-source-location))
           (clause-alist rest ...)))
    ((_ (name param => exp ...) rest ...)
     (cons (list name
                 (lambda (svc)
                   (let ((param (service-value svc)))
                     (service (service-kind svc)
                       (begin exp ...))))
                 (current-source-location))
           (clause-alist rest ...)))
    ((_)
     '())))

(define (apply-clauses clauses service deleted-services)
  (define (raise-if-deleted name properties)
    (match (find (match-lambda
                   ((deleted-name _)
                    (eq? name deleted-name)))
                 deleted-services)
      ((_ deleted-properties)
       (raise (make-compound-condition
               (condition
                (&error-location
                 (location (source-properties->location properties))))
               (formatted-message
                (G_ "modify-services: service '~a' was deleted here: ~a")
                name
                (source-properties->location deleted-properties)))))
      (_ #t)))

  (match clauses
    (((name proc properties) . rest)
     (raise-if-deleted name properties)
     (if (eq? (and service (service-type-name (service-kind service))) name)
         (let ((new-service (proc service)))
           (apply-clauses rest new-service
                          (if new-service
                              deleted-services
                              (cons (list name properties)
                                    deleted-services))))
         (apply-clauses rest service deleted-services)))
    (()
     service)))

(define (%modify-services services clauses)
  (define (raise-if-not-found clause)
    (match clause
      ((name _ properties)
       (unless (find (lambda (service)
                       (eq? name (service-type-name (service-kind service))))
                     services)
         (raise (make-compound-condition
                 (condition
                  (&error-location
                   (location (source-properties->location properties))))
                 (formatted-message
                  (G_ "modify-services/by-name: service '~a' not found in service list")
                  name)))))))

  (for-each raise-if-not-found clauses)
  (reverse (filter-map identity
                       (fold (lambda (service services)
                               (cons (apply-clauses clauses service '())
                                     services))
                             '()
                             services))))

(define-syntax modify-services/by-name
  (syntax-rules ()
    "Similiar to modify-services but uses the name of the service type instead.
For example:

  (modify-services %base-services
    ('guix
     config => (guix-configuration
                 (inherit config)
                 (use-substitutes? #f)
                 (extra-options '(\"--gc-keep-derivations\"))))
    ('mingetty
     config => (mingetty-configuration
                 (inherit config)
                 (motd (plain-file \"motd\" \"Hi there!\"))))
    (delete 'udev))"
    ((_ services clauses ...)
     (%modify-services services (clause-alist clauses ...)))))

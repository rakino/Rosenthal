;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services nix)
  ;; Guile builtins
  #:use-module (srfi srfi-26)
  ;; Utilities
  #:use-module (guix gexp)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  ;; Guix Home - services
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  ;; Guix packages
  #:autoload   (gnu packages package-management) (guix)
  #:export (nix-search-paths-service-type
            home-nix-search-paths-service-type))

(define* (nix-search-paths-shepherd-extension config #:key home-service?)
  (list (shepherd-service
          (documentation
           "Build Nix profile and symlink it to the specified path.")
          (requirement
           (if home-service?
               '()
               '(user-processes nix-daemon networking)))
          (provision '(build-nix-profile))
          (one-shot? #t)
          (start
           #~(make-forkexec-constructor
              (list "build-nix-profile" #$config))))))

;; TODO: Avoid duplicating search paths from the system profile.
(define (nix-search-paths-etc-profile-d-extension config)
  (list (mixed-text-file "nix-search-paths.sh" "\
eval \"$(" guix "/bin/guix package --search-paths=suffix \
-p /run/current-system/profile -p" config ")\"")))

(define (nix-search-paths-home-shell-profile-extension config)
  (list (mixed-text-file "nix-search-paths.sh" "\
eval \"$(" guix "/bin/guix package --search-paths=suffix \
-p ~/.guix-home/profile -p" config ")\"")))

(define nix-search-paths-service-type
  (service-type
    (name 'nix-search-paths)
    (extensions
     (list (service-extension shepherd-root-service-type
                              nix-search-paths-shepherd-extension)
           (service-extension etc-profile-d-service-type
                              nix-search-paths-etc-profile-d-extension)))
    (default-value "/nix/var/nix/profiles/guix-system-nix-profile")
    (description "Set up search paths for @code{with-nix-profile}.")))

(define home-nix-search-paths-service-type
  (service-type
    (inherit nix-search-paths-service-type)
    (name 'home-nix-search-paths)
    (extensions
     (list (service-extension home-shepherd-service-type
                              (cut nix-search-paths-shepherd-extension <> #:home-service? #t))
           (service-extension home-shell-profile-service-type
                              nix-search-paths-home-shell-profile-extension)))
    (default-value "/var/tmp/guix-home-nix-profile")))

(define-service-type-mapping
  nix-search-paths-service-type => home-nix-search-paths-service-type)

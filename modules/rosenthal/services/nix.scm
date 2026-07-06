;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services nix)
  ;; Utilities
  #:use-module (guix gexp)
  ;; Guix System
  #:use-module (gnu system pam)
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

(define (nix-search-paths-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p (dirname #$config)))))

(define (nix-search-paths-environment-extension config)
  `(("LOCALE_ARCHIVE" . ,(in-vicinity config "lib/locale/locale-archive"))))

(define* (nix-search-paths-shepherd-extension #:key home?)
  (lambda (config)
    (list (shepherd-service
            (documentation
             "Build Nix profile and symlink it to the specified path.")
            (requirement
             (if home?
                 '()
                 '(user-processes nix-daemon networking)))
            (provision '(build-nix-profile))
            (one-shot? #t)
            (start
             #~(make-forkexec-constructor
                (list #$(if home?
                            "build-nix-profile"
                            "/run/current-system/profile/bin/build-nix-profile")
                      #$config)))))))

;; TODO: Avoid duplicating search paths from the system profile.
(define* (nix-search-paths-profile-extension #:key home?)
  (lambda (config)
    (list (mixed-text-file "nix-search-paths.sh" "\
eval \"$(" guix "/bin/guix package --search-paths=suffix"
" -p " (if home?
         "~/.guix-home/profile"
         "/run/current-system/profile")
" -p " config ")\""))))

(define nix-search-paths-service-type
  (service-type
    (name 'nix-search-paths)
    (extensions
     (list (service-extension session-environment-service-type
                              nix-search-paths-environment-extension)
           (service-extension activation-service-type
                              nix-search-paths-activation)
           (service-extension shepherd-root-service-type
                              (nix-search-paths-shepherd-extension))
           (service-extension etc-profile-d-service-type
                              (nix-search-paths-profile-extension))))
    (default-value "/nix/var/nix/profiles/guix-system-nix-profile")
    (description "Set up search paths for @code{with-nix-profile}.")))

(define home-nix-search-paths-service-type
  (service-type
    (inherit nix-search-paths-service-type)
    (name 'home-nix-search-paths)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              nix-search-paths-environment-extension)
           (service-extension home-activation-service-type
                              nix-search-paths-activation)
           (service-extension home-shepherd-service-type
                              (nix-search-paths-shepherd-extension #:home? #t))
           (service-extension home-shell-profile-service-type
                              (nix-search-paths-profile-extension #:home? #t))))
    (default-value "/var/tmp/guix-home-nix-profile")))

(define-service-type-mapping
  nix-search-paths-service-type => home-nix-search-paths-service-type)

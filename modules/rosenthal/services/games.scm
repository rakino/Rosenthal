;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services games)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (rosenthal utils file)
  ;; Guix System
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  ;; Guix packages
  #:use-module (gnu packages linux)
  #:export (gamemode-service-type
            gamemode-configuration))

(define-maybe gexp)

(define-configuration/no-serialization gamemode-configuration
  (gamemode
   (file-like gamemode)
   "@code{gamemode} package to use.")
  (config
   maybe-gexp
   ""))

(define (gamemode-account-service config)
  (list (user-group
          (system? #t)
          (name "gamemode"))))

(define gamemode-etc-service
  (match-record-lambda <gamemode-configuration>
      (gamemode config)
    `(("gamemode.ini"
       ,(if (maybe-value-set? config)
            (ini-file "gamemode.ini" config)
            (file-append (package-source gamemode) "/example/gamemode.ini")))
      ("security/limits.d/10-gamemode.conf"
       ,(file-append gamemode "/etc/security/limits.d/10-gamemode.conf")))))

(define gamemode-service-type
  (service-type
    (name 'gamemode)
    (extensions
     (list (service-extension profile-service-type
                              (compose list gamemode-configuration-gamemode))
           (service-extension account-service-type
                              gamemode-account-service)
           (service-extension etc-service-type
                              gamemode-etc-service)
           (service-extension dbus-root-service-type
                              (compose list gamemode-configuration-gamemode))
           (service-extension polkit-service-type
                              (compose list gamemode-configuration-gamemode))))
    (default-value (gamemode-configuration))
    (description "Set up Guix System environment for gamemode.")))

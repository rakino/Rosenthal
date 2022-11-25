;; SPDX-FileCopyrightText: 2014-2022 Ludovic Courtès <ludo@gnu.org>
;; SPDX-FileCopyrightText: 2015 Andy Wingo <wingo@igalia.com>
;; SPDX-FileCopyrightText: 2015 Mark H Weaver <mhw@netris.org>
;; SPDX-FileCopyrightText: 2016 Sou Bunnbu <iyzsong@gmail.com>
;; SPDX-FileCopyrightText: 2017, 2020, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;; SPDX-FileCopyrightText: 2017 Nikita <nikita@n0.is>
;; SPDX-FileCopyrightText: 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;; SPDX-FileCopyrightText: 2018 Ricardo Wurmus <rekado@elephly.net>
;; SPDX-FileCopyrightText: 2017, 2019 Christopher Baines <mail@cbaines.net>
;; SPDX-FileCopyrightText: 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;; SPDX-FileCopyrightText: 2019 David Wilson <david@daviwil.com>
;; SPDX-FileCopyrightText: 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;; SPDX-FileCopyrightText: 2020 Reza Alizadeh Majd <r.majd@pantherx.org>
;; SPDX-FileCopyrightText: 2021 Brice Waegeneire <brice@waegenei.re>
;; SPDX-FileCopyrightText: 2021, 2022 muradm <mail@muradm.net>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services desktop)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (seatd-configuration
            seatd-service-type))

;;;
;;; seatd-service-type -- minimal seat management daemon
;;;

(define (seatd-group-sanitizer group-or-name)
  (match group-or-name
    ((? user-group? group) group)
    ((? string? group-name) (user-group (name group-name) (system? #t)))
    (_ (leave (G_ "seatd: '~a' is not a valid group~%") group-or-name))))

(define-record-type* <seatd-configuration> seatd-configuration
  make-seatd-configuration
  seatd-configuration?
  (seatd seatd-package (default seatd))
  (group seatd-group                    ; string | <user-group>
         (default "seat")
         (sanitize seatd-group-sanitizer))
  (socket seatd-socket (default "/run/seatd.sock"))
  (logfile seatd-logfile (default "/var/log/seatd.log"))
  (loglevel seatd-loglevel (default "info")))

(define (seatd-shepherd-service config)
  (list (shepherd-service
         (documentation "Minimal seat management daemon")
         (requirement '())
         (provision '(seatd))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (seatd-package config) "/bin/seatd")
                         "-g" #$(user-group-name (seatd-group config)))
                   #:environment-variables
                   (list (string-append "SEATD_LOGLEVEL="
                                        #$(seatd-loglevel config))
                         (string-append "SEATD_DEFAULTPATH="
                                        #$(seatd-socket config)))
                   #:log-file #$(seatd-logfile config)))
         (stop #~(make-kill-destructor)))))

(define seatd-accounts
  (match-lambda (($ <seatd-configuration> _ group) (list group))))

(define seatd-environment
  (match-lambda
    (($ <seatd-configuration> _ _ socket)
     `(("SEATD_SOCK" . ,socket)))))

(define seatd-service-type
  (service-type
   (name 'seatd)
   (description "Seat management takes care of mediating access
to shared devices (graphics, input), without requiring the
applications needing access to be root.")
   (extensions
    (list
     (service-extension account-service-type seatd-accounts)
     (service-extension session-environment-service-type seatd-environment)
     (service-extension shepherd-root-service-type seatd-shepherd-service)))
   (default-value (seatd-configuration))))

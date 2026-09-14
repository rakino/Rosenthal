;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services base)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils contract combinators)
  #:use-module (rosenthal utils file)
  #:use-module (rosenthal utils records)
  #:use-module (rosenthal utils packages)
  ;; Guix System
  #:use-module (gnu system keyboard)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  ;; Guix packages
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (rosenthal packages admin)
  #:use-module (rosenthal packages wm)
  #:export (noctalia-greeter-configuration
            noctalia-greeter-service-type
            greetd-noctalia-greeter-session

            greetd-tuigreet-session))


;;;
;;; noctalia-greeter
;;;

(define-record-type/dolly <noctalia-greeter-configuration>
  noctalia-greeter-configuration
  make-noctalia-greeter-configuration
  noctalia-greeter-configuration?
  this-noctalia-greeter-configuration
  (package
   noctalia-greeter-configuration-package
   (default noctalia-greeter)
   (contract file-like?)
   (documentation
    "Noctalia greeter package."))
  (keyboard-layout
   noctalia-greeter-configuration-keyboard-layout
   (default #f)
   (contract (or/c #f keyboard-layout?))
   (documentation
    "Keyboard layout to use in a Noctalia greeter session.")))

(define (noctalia-greeter-activation-service _)
  #~(mkdir-p/perms "/var/lib/noctalia-greeter" (getpwnam "greeter") #o750))

(define noctalia-greeter-special-files-service
  (match-record-lambda/dolly <noctalia-greeter-configuration>
      (keyboard-layout)
    (if keyboard-layout
        (let* ((name (keyboard-layout-name keyboard-layout))
               (variant (keyboard-layout-variant keyboard-layout))
               (options (keyboard-layout-options keyboard-layout))
               (layout
                `(("layout"  . ,name)
                  ,@(if variant
                        `(("variant" . ,variant))
                        '())
                  ,@(if (null? options)
                        '()
                        `(("options" . ,(string-join options ",")))))))
          `(("/var/lib/noctalia-greeter/greeter.toml"
             ,(toml-file "greeter.toml"
                `(("keyboard" . ,layout))))))
        '())))

(define noctalia-greeter-service-type
  (service-type
    (name 'noctalia-greeter)
    (extensions
     (list (service-extension activation-service-type
                              noctalia-greeter-activation-service)
           (service-extension special-files-service-type
                              noctalia-greeter-special-files-service)
           (service-extension polkit-service-type
                              (compose list noctalia-greeter-configuration-package))
           (service-extension profile-service-type
                              (compose list noctalia-greeter-configuration-package))))
    (default-value (noctalia-greeter-configuration))
    (description "Set up environment for @command{noctalia-greeter-session}.")))

(define-record-type/dolly <greetd-noctalia-greeter-session>
  greetd-noctalia-greeter-session
  make-greetd-noctalia-greeter-session
  greetd-noctalia-greeter-session?
  this-greetd-noctalia-greeter-session
  (noctalia-greeter-session
   greetd-noctalia-greeter-session-noctalia-greeter-session
   (default (file-append noctalia-greeter "/bin/noctalia-greeter-session"))
   (contract (or/c file-like? string?))
   (documentation
    "@command{noctalia-greeter-session} executable file.")))

(define-gexp-compiler (greetd-tuigreet-session-compiler
                       (session <greetd-noctalia-greeter-session>)
                       system target)
  (match-record/dolly session <greetd-noctalia-greeter-session> (noctalia-greeter-session)
    (lower-object
     (program-file "noctalia-greeter-wrapper"
       #~(execl #$noctalia-greeter-session #$noctalia-greeter-session)))))


;;;
;;; tuigreet
;;;

(define-record-type* <greetd-tuigreet-session>
  greetd-tuigreet-session make-greetd-tuigreet-session
  greetd-tuigreet-session?
  this-greetd-tuigreet-session
  (tuigreet greetd-tuigreet-session-tuigreet
            (default tuigreet))
  (args     greetd-tuigreet-session-args
            (default '("--issue"
                       "--time"
                       "--user-menu"
                       "--asterisks"
                       "--remember"
                       "--remember-session"
                       "--power-shutdown" "loginctl poweroff"
                       "--power-reboot" "loginctl reboot"))))

(define-gexp-compiler (greetd-tuigreet-session-compiler
                       (session <greetd-tuigreet-session>)
                       system target)
  (match-record session <greetd-tuigreet-session> (tuigreet args)
    (let ((tuigreet (file-append tuigreet "/bin/tuigreet")))
      (lower-object
       (program-file "tuigreet-wrapper"
         #~(execl #$tuigreet #$tuigreet #$@args))))))

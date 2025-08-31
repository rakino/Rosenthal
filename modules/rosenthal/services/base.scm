;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services base)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils packages)

  #:use-module (gnu services base)

  #:use-module (gnu packages bash)
  #:use-module (rosenthal packages admin)
  #:export (greetd-tuigreet-session))

(define-record-type* <greetd-tuigreet-session>
  greetd-tuigreet-session make-greetd-tuigreet-session
  greetd-tuigreet-session?
  this-greetd-tuigreet-session
  (tuigreet greetd-tuigreet-session-tuigreet
            (default (spec->pkg "tuigreet")))
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

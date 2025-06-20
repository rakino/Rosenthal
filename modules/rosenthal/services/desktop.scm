;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services desktop)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (rosenthal utils packages)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (rosenthal services base)
  #:export (%rosenthal-desktop-services))

(define* (rosenthal-desktop-services-for-system
          #:optional (system (or (%current-target-system)
                                 (%current-system))))
  (define %display-manager-service-type
    (if (string-prefix? "x86_64" system)
        gdm-service-type
        sddm-service-type))

  (define %font
    (file-append (spec->pkg "font-terminus") "/share/consolefonts/ter-132n"))

  (cons* (service greetd-service-type
           (greetd-configuration
             (greeter-supplementary-groups '("video" "input"))
             (terminals
              (map (lambda (x)
                     (greetd-terminal-configuration
                       (terminal-vt (number->string x))
                       (terminal-switch (eqv? 1 x))
                       (default-session-command
                         (cond
                          ((eqv? 1 x)
                           (greetd-tuigreet-session))
                          (else
                           (greetd-agreety-session
                             (command
                              (greetd-user-session
                                (command #~(getenv "SHELL"))))))))))
                   (iota 6 1)))))

         (service screen-locker-service-type
           (screen-locker-configuration
             (name "swaylock")
             (program (plain-file "empty" "")) ;Not used.
             (using-setuid? #f)))
         (service screen-locker-service-type
           (screen-locker-configuration
             (name "waylock")
             (program (plain-file "empty" "")) ;Not used.
             (using-setuid? #f)))

         (modify-services %desktop-services
           (delete mingetty-service-type)
           (delete %display-manager-service-type)
           (delete screen-locker-service-type)

           (console-font-service-type
            _ => (map (lambda (num)
                        (cons (string-append "tty" (number->string num))
                              %font))
                      (iota 6 1))))))

(define-syntax %rosenthal-desktop-services
  (identifier-syntax (rosenthal-desktop-services-for-system)))

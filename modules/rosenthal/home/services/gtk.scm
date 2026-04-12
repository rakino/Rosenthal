;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal home services gtk)
  ;; Guilt builtins
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils file)
  ;; Guix Home - services
  #:use-module (gnu home services)
  #:export (home-gtk2-service-type
            home-gtk3-service-type
            home-gtk4-service-type))

(define (home-gtk2-environment-variables config)
  (if (null? config)
      '()
      ;; https://wiki.archlinux.org/title/XDG_Base_Directory
      '(("GTK2_RC_FILES" . "$XDG_CONFIG_HOME/gtk-2.0/gtkrc"))))

(define (home-gtk2-xdg-configuration-files config)
  (if (null? config)
      '()
      `(("gtk-2.0/gtkrc"
         ,(computed-file "gtkrc"
            #~(begin
                (use-modules (ice-9 match))
                (call-with-output-file #$output
                  (lambda (port)
                    (for-each (match-lambda
                                ((key . val)
                                 (format port "~a = ~a~%" key val)))
                              '#$config))))
            #:options '(#:substitutable? #f))))))

(define (home-gtk3-xdg-configuration-files config)
  (if (null? config)
      '()
      `(("gtk-3.0/settings.ini"
         ,(ini-file "settings.ini" #~'(("Settings" #$@config)))))))

(define (home-gtk4-xdg-configuration-files config)
  (if (null? config)
      '()
      `(("gtk-4.0/settings.ini"
         ,(ini-file "settings.ini" #~'(("Settings" #$@config)))))))

(define home-gtk2-service-type
  (service-type
    (name 'home-gtk2)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              home-gtk2-environment-variables)
           (service-extension home-xdg-configuration-files-service-type
                              home-gtk2-xdg-configuration-files)))
    (compose concatenate)
    (extend
     (lambda (config extension)
       (delete-duplicates
        ;; Allow overriding extensions via configuration.
        (append config
                extension)
        (lambda (a b)
          (string=? (car a)
                    (car b))))))
    (description "Set up GTK2 settings in @file{~/.config/gtk-2.0/gtkrc}")
    (default-value '())))

(define home-gtk3-service-type
  (service-type
    (inherit home-gtk2-service-type)
    (name 'home-gtk3)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              home-gtk3-xdg-configuration-files)))
    (description
     "Set up GTK3 settings in @file{~/.config/gtk-3.0/settings.ini}")))

(define home-gtk4-service-type
  (service-type
    (inherit home-gtk2-service-type)
    (name 'home-gtk4)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              home-gtk4-xdg-configuration-files)))
    (description
     "Set up GTK4 settings in @file{~/.config/gtk-4.0/settings.ini}")))

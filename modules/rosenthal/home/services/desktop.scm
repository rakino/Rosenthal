;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal home services desktop)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  ;; Guix Home - services
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  ;; Guix packages
  #:autoload   (rosenthal packages wm) (noctalia)
  #:export (home-graphical-session-service-type
            home-graphical-session-configuration

            home-noctalia-configuration
            home-noctalia-service-type))


;;;
;;; Graphical session.
;;;

(define-configuration/no-serialization home-graphical-session-configuration
  (timeout
   (integer 10)
   "Time in seconds to wait before a graphical session is found.")
  (wayland?
   (boolean #f)
   "Wait and require Wayland environment to be available, providing a
@code{wayland-display} Shepherd service.")
  (x11?
   (boolean #f)
   "Wait and require X11 environment to be available, providing a
@code{x11-display} Shepherd service."))

(define (home-graphical-session-find-socket)
  #~(lambda* (timeout directory pattern #:key wayland?)
      ;; Wait for an accessible socket matching PATTERN to show up in
      ;; DIRECTORY, up to TIMEOUT seconds.
      (let loop ((attempts timeout))
        (define socket
          (find (match-lambda
                  ((or "." "..") #f)
                  (name
                   (let ((name (in-vicinity directory name)))
                     (and (string-match pattern name)
                          (access? name O_RDWR)))))
                (or (reverse (scandir directory))
                    '())))

        (if socket
            (let ((display
                   (if wayland?
                       socket
                       (string-append ":" (string-drop socket 1)))))
              (format #t "Display server found at ~s.~%" display)
              display)
            (if (zero? attempts)
                (error
                 (format #f "Can't find ~a environment; giving up."
                         (if wayland? "Wayland" "X11")))
                (begin
                  (sleep 1)
                  (loop (- attempts 1))))))))

(define home-graphical-session-shepherd-service
  (match-record-lambda <home-graphical-session-configuration>
      (timeout wayland? x11?)
    (append
     (if (or wayland? x11?)
         (list (shepherd-service
                 (documentation
                  "Service target to indicate a graphical session is ready.")
                 (provision '(graphical-session))
                 (requirement
                  (append
                   (if wayland?
                       '(wayland-display)
                       '())
                   (if x11?
                       '(x11-display)
                       '())))
                 (respawn? #f)
                 (start #~(const #t))
                 (stop #~(const #f))
                 (actions
                  (list (shepherd-action
                          (name 'set-environment)
                          (documentation
                           "Set environment variables in Shepherd.")
                          (procedure
                           #~(lambda (_ . args)
                               (for-each putenv args))))))))
         '())
     (if wayland?
         (list (shepherd-service
                 (documentation "Find Wayland socket.")
                 (provision '(wayland-display))
                 (modules
                  '((ice-9 ftw)
                    (ice-9 match)
                    (ice-9 regex)
                    (srfi srfi-1)
                    (shepherd support)))
                 (respawn? #f)
                 (start
                  #~(lambda* (#:optional (wayland-display (getenv "WAYLAND_DISPLAY")))
                      (let ((wayland-display
                             (or wayland-display
                                 (#$(home-graphical-session-find-socket)
                                  #$timeout
                                  %user-runtime-dir
                                  "wayland-[0-9]+$"
                                  #:wayland? #t))))
                        (when wayland-display
                          (setenv "WAYLAND_DISPLAY" wayland-display))
                        wayland-display)))
                 (stop
                  #~(lambda (_)
                      (unsetenv "WAYLAND_DISPLAY")
                      #f))))
         '())
     (if x11?
         (list (shepherd-service
                 (documentation "Find X11 display server.")
                 (provision '(x11-display))
                 (requirement
                  ;; XWayland starts later than the compositor.
                  (if wayland?
                      '(wayland-display)
                      '()))
                 (modules
                  '((ice-9 ftw)
                    (ice-9 match)
                    (ice-9 regex)
                    (srfi srfi-1)))
                 (respawn? #f)
                 (start
                  #~(lambda* (#:optional (x11-display (getenv "DISPLAY")))
                      (let ((x11-display
                             (and #$x11?
                                  (or x11-display
                                      (#$(home-graphical-session-find-socket)
                                       #$timeout
                                       "/tmp/.X11-unix"
                                       "X[0-9]+$")))))
                        (when x11-display
                          (setenv "DISPLAY" x11-display))
                        x11-display)))
                 (stop
                  #~(lambda (_)
                      (unsetenv "DISPLAY")
                      #f))))
         '()))))

(define home-graphical-session-service-type
  (service-type
    (name 'home-graphical-session)
    (extensions
     (list (service-extension home-shepherd-service-type
                              home-graphical-session-shepherd-service)))
    (compose identity)
    (extend
     (lambda (config extensions)
       (match-record config <home-graphical-session-configuration>
                     (wayland? x11?)
         (home-graphical-session-configuration
          (inherit config)
          (wayland?
           (if (or wayland?
                   (member 'wayland extensions))
               #t
               #f))
          (x11?
           (if (or x11?
                   (member 'x11 extensions))
               #t
               #f))))))
    (default-value (home-graphical-session-configuration))
    (description "")))


;;;
;;; Noctalia
;;;

(define-configuration/no-serialization home-noctalia-configuration
  (noctalia
   (file-like noctalia)
   "File-like object to provide @command{/bin/noctalia}."))

(define home-noctalia-shepherd-service
  (match-record-lambda <home-noctalia-configuration>
      (noctalia)
    (list (shepherd-service
            (documentation "Start noctalia.")
            (provision '(noctalia))
            (requirement '(dbus graphical-session))
            (modules '((shepherd support)))
            (start
             #~(lambda args
                 ((make-forkexec-constructor
                   (list #$(file-append noctalia "/bin/noctalia"))
                   #:log-file (in-vicinity %user-log-dir "noctalia.log")
                   ;; Inherit graphical session environment.
                   #:environment-variables (environ))
                  args)))
            (stop #~(make-kill-destructor))))))

(define home-noctalia-service-type
  (service-type
    (name 'home-noctalia)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-noctalia-configuration-noctalia))
           (service-extension home-shepherd-service-type
                              home-noctalia-shepherd-service)
           (service-extension home-graphical-session-service-type
                              (const 'wayland))))
    (default-value (home-noctalia-configuration))
    (description "Run Noctalia, a lightweight Wayland shell and bar.")))

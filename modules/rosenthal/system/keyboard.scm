;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal system keyboard)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:export (keyboard-layout
            keyboard-layout->console-keymap))

;; Copied from (gnu system keyboard), with package dependencies removed.  To
;; use this module, packages ‘console-setup’ and ‘xkeyboard-config’ should be
;; installed into the system profile.

;; Copied from (gnu system keyboard) to avoid package dependencies.
(define-immutable-record-type <keyboard-layout>
  (%keyboard-layout name variant model options)
  keyboard-layout?
  (name      keyboard-layout-name)                ;string
  (variant   keyboard-layout-variant)             ;#f | string
  (model     keyboard-layout-model)               ;#f | string
  (options   keyboard-layout-options))            ;list of strings

(define* (keyboard-layout name #:optional variant
                          #:key model (options '()))
  "Return a new keyboard layout with the given NAME and VARIANT.

NAME must be a string such as \"fr\"; VARIANT must be a string such as
\"bepo\" or \"nodeadkeys\".  See the 'xkeyboard-config' package for valid
options."
  (%keyboard-layout name variant model options))

(define* (keyboard-layout->console-keymap layout)
  "Return a Linux console keymap file for LAYOUT, a <keyboard-layout> record.
Layout information is taken from the XKEYBOARD-CONFIG package."
  (define %current-system
    "/run/current-system/profile")

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 match))

          (define pipe
            (open-pipe* OPEN_READ
                        (in-vicinity %current-system "bin/ckbcomp")
                        (string-append
                         "-I" (in-vicinity %current-system "share/X11/xkb"))
                        "-rules" "base"
                        #$@(match (keyboard-layout-model layout)
                             (#f      '())
                             (model   `("-model" ,model)))
                        #$(keyboard-layout-name layout)
                        #$(or (keyboard-layout-variant layout)
                              "")
                        #$(string-join (keyboard-layout-options layout) ",")))

          (call-with-output-file #$output
            (lambda (output)
              (dump-port pipe output)))

          ;; Note: ckbcomp errors out when the layout name is unknown, but
          ;; merely emits a warning when the variant is unknown.
          (unless (zero? (close-pipe pipe))
            (error "failed to create console keymap for keyboard layout"
                   #$(keyboard-layout-name layout))))))

  (computed-file (string-append "console-keymap."
                                (string-map (match-lambda
                                              (#\, #\-)
                                              (chr chr))
                                            (keyboard-layout-name layout)))
                 build))

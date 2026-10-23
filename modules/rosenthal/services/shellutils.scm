;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services shellutils)
  ;; Guile builtins
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  ;; Utilities
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (rosenthal utils packages)
  #:use-module (rosenthal utils records)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  ;; Guix Home - services
  #:use-module (gnu home services shells)
  ;; Guix packages
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (rosenthal packages shellutils)
  #:export (home-fish-plugin-atuin-service-type
            home-atuin-configuration

            home-fish-plugin-direnv-service-type
            home-direnv-configuration

            home-fish-plugin-stinkpot-service-type
            home-stinkpot-configuration

            home-fish-plugin-zoxide-service-type
            home-zoxide-configuration))

(define (warn-deprecated-shells-field configuration)
  (lambda (value)
    (when (maybe-value-set? value)
      (warning #f (G_ "'~a': '~a' field is deprecated~%")
               configuration 'shells))))

;;;
;;; atuin
;;;

(define-configuration/no-serialization home-atuin-configuration
  (atuin
   (file-like atuin)
   "")
  (shells
   list
   "Deprecated."
   (sanitizer (warn-deprecated-shells-field 'home-atuin-configuration))))

(define %home-atuin-fish
  (match-record-lambda <home-atuin-configuration>
      (atuin)
    (home-fish-extension
      (config
       (list (mixed-text-file "atuin.fish" "\
status is-interactive
and begin
    " atuin "/bin/atuin init fish | source
end\n"))))))

(define home-fish-plugin-atuin-service-type
  (service-type
    (name 'home-atuin)
    (extensions
     (list (service-extension home-fish-service-type
                              %home-atuin-fish)))
    (default-value (home-atuin-configuration))
    (description "")))

(define-deprecated/public-alias home-atuin-service-type
  home-fish-plugin-atuin-service-type)


;;;
;;; direnv
;;;

(define-configuration/no-serialization home-direnv-configuration
  (direnv
   (file-like direnv)
   "")
  (shells
   list
   "Deprecated."
   (sanitizer (warn-deprecated-shells-field 'home-atuin-configuration))))

(define %home-direnv-fish
  (match-record-lambda <home-direnv-configuration>
      (direnv)
    (home-fish-extension
      (config
       (list (mixed-text-file "direnv.fish" "\
status is-interactive
and begin
    " direnv "/bin/direnv hook fish | source
end\n"))))))

(define home-fish-plugin-direnv-service-type
  (service-type
    (name 'home-direnv)
    (extensions
     (list (service-extension home-fish-service-type
                              %home-direnv-fish)))
    (default-value (home-direnv-configuration))
    (description "")))

(define-deprecated/public-alias home-direnv-service-type
  home-fish-plugin-direnv-service-type)


;;;
;;; stinkpot
;;;

(define-record-type/dolly <home-stinkpot-configuration>
  home-stinkpot-configuration
  make-home-stinkpot-configuration
  home-stinkpot-configuration?
  this-home-stinkpot-configuration
  (stinkpot
   home-stinkpot-configuration-stinkpot
   (default stinkpot)
   (contract file-like?)))

(define home-stinkpot-fish-service
  (match-record-lambda/dolly <home-stinkpot-configuration>
      (stinkpot)
    (home-fish-extension
      (config
       (list (mixed-text-file "stinkpot.fish" "\
status is-interactive
and begin
    " stinkpot "/bin/stinkpot init fish | source
end\n"))))))

(define home-fish-plugin-stinkpot-service-type
  (service-type
    (name 'home-stinkpot)
    (extensions
     (list (service-extension home-fish-service-type
                              home-stinkpot-fish-service)))
    (default-value (home-stinkpot-configuration))
    (description "Set up @command{stinkpot} for Fish shell.")))


;;;
;;; zoxide
;;;

(define-configuration/no-serialization home-zoxide-configuration
  (zoxide
   (file-like zoxide)
   "")
  (shells
   list
   "Deprecated."
   (sanitizer (warn-deprecated-shells-field 'home-atuin-configuration))))

(define %home-zoxide-fish
  (match-record-lambda <home-zoxide-configuration>
      (zoxide)
    (home-fish-extension
      (config
       (list (mixed-text-file "zoxide.fish" "\
status is-interactive
and begin
    " zoxide "/bin/zoxide init --cmd cd fish | source
end\n"))))))

(define home-fish-plugin-zoxide-service-type
  (service-type
    (name 'home-zoxide)
    (extensions
     (list (service-extension home-fish-service-type
                              %home-zoxide-fish)))
    (default-value (home-zoxide-configuration))
    (description "")))

(define-deprecated/public-alias home-zoxide-service-type
  home-fish-plugin-zoxide-service-type)

;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services shellutils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils packages)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)

  #:use-module (gnu home services shells)

  #:use-module (rosenthal packages rust-apps)

  #:export (home-atuin-configuration
            home-atuin-service-type

            home-direnv-configuration
            home-direnv-service-type

            home-zoxide-configuration
            home-zoxide-service-type))

(define (shells? val)
  (every (cut member <> '(bash zsh fish))
         val))

;;;
;;; atuin
;;;

(define-configuration/no-serialization home-atuin-configuration
  (atuin
   (file-like atuin)
   "")
  (shells
   shells
   ""))

(define %home-atuin-fish
  (match-record-lambda <home-atuin-configuration>
      (atuin shells)
    (home-fish-extension
      (config
       (if (member 'fish shells)
           (list (mixed-text-file "atuin.fish"
                   atuin "/bin/atuin init fish | source\n"))
           '())))))

(define home-atuin-service-type
  (service-type
    (name 'atuin)
    (extensions
     (list (service-extension home-fish-service-type
                              %home-atuin-fish)))
    (description "")))


;;;
;;; direnv
;;;

(define-configuration/no-serialization home-direnv-configuration
  (direnv
   (file-like (spec->pkg "direnv"))
   "")
  (shells
   shells
   ""))

(define %home-direnv-fish
  (match-record-lambda <home-direnv-configuration>
      (direnv shells)
    (home-fish-extension
      (config
       (if (member 'fish shells)
           (list (mixed-text-file "direnv.fish"
                   direnv "/bin/direnv hook fish | source\n"))
           '())))))

(define home-direnv-service-type
  (service-type
    (name 'direnv)
    (extensions
     (list (service-extension home-fish-service-type
                              %home-direnv-fish)))
    (description "")))


;;;
;;; zoxide
;;;

(define-configuration/no-serialization home-zoxide-configuration
  (zoxide
   (file-like (spec->pkg "zoxide"))
   "")
  (shells
   shells
   ""))

(define %home-zoxide-fish
  (match-record-lambda <home-zoxide-configuration>
      (zoxide shells)
    (home-fish-extension
      (config
       (if (member 'fish shells)
           (list (mixed-text-file "zoxide.fish"
                   zoxide "/bin/zoxide init --cmd cd fish | source\n"))
           '())))))

(define home-zoxide-service-type
  (service-type
    (name 'zoxide)
    (extensions
     (list (service-extension home-fish-service-type
                              %home-zoxide-fish)))
    (description "")))

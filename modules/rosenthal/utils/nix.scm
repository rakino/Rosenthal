;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils nix)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; Guix build systems
  #:use-module (guix build-system trivial)
  ;; Guix packages
  #:autoload   (gnu packages package-management) (nix)
  #:export (%nix-shell-wrapper-default-unset-env-vars

            nix-shell-wrapper
            nix-shell-wrapper->package))

;; These search paths may contain incompatible libraries and may crash the
;; program loading them.  See also GCD 004:
;; https://consensus.guix.gnu.org/gcd/004-set-search-paths-without-program-wrappers.html
(define %nix-shell-wrapper-default-unset-env-vars
  '("XDG_DATA_DIRS"
    "XDG_CONFIG_DIRS"
    ;; Glib
    "GIO_EXTRA_MODULES"
    "GSETTINGS_SCHEMA_DIR"
    ;; Qt
    "QML2_IMPORT_PATH"
    "QML_IMPORT_PATH"
    "QT_PLUGIN_PATH"))

;;;
;;; Helper utilities to use packages from Nix.
;;;

;; See also https://nix.dev/manual/nix/2.34/command-ref/new-cli/nix3-env-shell.html
(define* (nix-shell-wrapper name
                            #:optional (installables ".")
                            #:key expression
                            (run-command '())
                            (options '())
                            (environment-keep #t)
                            (environment-unset %nix-shell-wrapper-default-unset-env-vars)
                            (environment-set '())
                            (nix (file-append nix "/bin/nix")))
  "Return a file-like object that wraps the \"nix shell\" command-line utility
and spawns an one-off software environment if run.  Nix daemon is required to
use the wrapper.

INSTALLABLES (string / list of strings) is specified as Flake output attribute
and will be added into the environment.  When EXPRESSION (string / file-like
object) is set, INSTALLABLES will be optional and interpreted as attribute paths
relative to the Nix expression.

RUN-COMMAND (default: '(), list of strings) specifies command and arguments to
be executed in the environment.  Command-line arguments passed to the wrapper
will be appended.

OPTIONS (default: '(), list of strings) specifies extra options to pass to the
\"nix shell\" command-line utility.

When ENVIRONMENT-KEEP (default: #t, boolean / list of strings) is set to a value
other than #t, the environment will be cleared, keeping only specified
environment variables.

ENVIRONMENT-UNSET (default: %nix-shell-wrapper-default-unset-env-vars, list of
strings) unsets specified environment variables from the environment.  It's only
usable when ENVIRONMENT-KEEP is #t.

For each key-value pair in ENVIRONMENT-SET (default: '(), association list), the
environment sets its corresponding environment variable.

NIX (default: (file-append nix \"/bin/nix\"), string / file-like object)
specifies the Nix binary to use.

Examples:

    (nix-shell-wrapper \"cowsay-hello-coreutils-env\"
      '(\"github:NixOS/nixpkgs/nixos-26.05#coreutils\"
        \"github:NixOS/nixpkgs/nixos-26.05#cowsay\")
      #:run-command '(\"env\" \"LANG=C.UTF-8\" \"cowsay\" \"hello\"))

    (nix-shell-wrapper \"cowsay-hello\"
      \"github:NixOS/nixpkgs/nixos-26.05#cowsay\"
      #:run-command '(\"cowsay\" \"hello\")
      #:environment-set '((\"LANG\" . \"C.UTF-8\")))

    (nix-shell-wrapper \"python-with-numpy\"
      #:expression \"with (import (builtins.getFlake \\\"github:NixOS/nixpkgs/nixos-26.05\\\") {}); python3.withPackages (ps: [ps.numpy])\"
      #:run-command '(\"python\"))

    (nix-shell-wrapper \"coreutils-env\"
      \"coreutils\"
      #:expression (local-file \"/tmp/nixpkgs.nix\")
      #:run-command '(\"env\")
      #:environment-keep #f)
"
  (define %options
    (append (match environment-keep
              (#t
               (append-map (cut list "--unset-env-var" <>)
                           environment-unset))
              (#f
               '("--ignore-env"))
              (_
               (cons "--ignore-env"
                     (append-map (cut list "--keep-env-var" <>)
                                 environment-keep))))
            (append-map (match-lambda
                          ((name . value)
                           (list "--set-env-var" name value)))
                        environment-set)
            options))

  (program-file (string-append name "-nix-shell")
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (ice-9 match)
                       (guix build utils))
          (match (command-line)
            ((_ . args)
             (apply invoke #$nix
                    "--extra-experimental-features" "nix-command flakes"
                    "shell"
                    #$@%options
                    #$@(match expression
                         ((? file-like?)
                          (list "--impure" "--file" expression))
                         ((? string?)
                          (list "--impure" "--expr" expression))
                         (else
                          '()))
                    #$@(if (list? installables)
                           installables
                           (list installables))
                    "--command" #$@run-command args)))))))

(define (nix-shell-wrapper->package wrapper)
  "Return a package for WRAPPER, file-like object created by
'nix-shell-wrapper'.  The package will install a command under its /bin
directory, with the same name as WRAPPER.

Note that packages created by this procedure are not supposed to be used in the
build environment and won't be able to interoperate with other packages in
practice."
  (let* ((wrapper-name (program-file-name wrapper))
         (command (string-drop-right wrapper-name (string-length "-nix-shell"))))
    (package
      (name wrapper-name)
      (version "0.0.0")
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list #:builder
             (with-imported-modules '((guix build utils))
               #~(begin
                   (use-modules (guix build utils))
                   (let ((dest (string-append #$output "/bin/" #$command)))
                     (mkdir-p (dirname dest))
                     (symlink #$wrapper dest))))))
      (home-page #f)
      (synopsis #f)
      (description #f)
      (license #f))))

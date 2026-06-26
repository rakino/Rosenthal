;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils nix)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; Guix build systems
  #:use-module (guix build-system trivial)
  #:export (nix-shell-wrapper
            nix-shell-wrapper->package))

;;;
;;; Helper utilities to use packages from Nix.
;;;

(define* (nix-shell-wrapper name installables
                            #:key
                            expression?
                            (run-command '())
                            (options '())
                            (nix "/run/current-system/profile/bin/nix"))
  "Return a file-like object that wraps the \"nix shell\" command-line utility
and spawns an one-off software environment when run.  Nix daemon is required to
use the wrapper.

INSTALLABLES (string / list of strings) specifies packages to be added into the
environment.  If EXPRESSION? is set to #t (default: #f, boolean), It must
contain exactly one Nix expression.

RUN-COMMAND (default: '(), list of strings) specifies command to run in the
environment.  Arguments passed to the wrapper will be appended to it.

OPTIONS (default '(), list of strings) specifies extra options to pass to the
\"nix shell\" command-line utility.

NIX (default: \"/run/current-system/profile/bin/nix\", string / file-like
object) specifies the Nix binary to use.

Examples:

    (nix-shell-wrapper \"cowsay\"
      '(\"github:NixOS/nixpkgs/nixos-26.05#coreutils\"
        \"github:NixOS/nixpkgs/nixos-26.05#cowsay\")
      #:run-command '(\"env\" \"LANG=C.UTF-8\" \"cowsay\" \"hello\"))

    (nix-shell-wrapper \"python-with-numpy\"
      \"with (import (builtins.getFlake \\\"github:NixOS/nixpkgs/nixos-26.05\\\") {}); python3.withPackages (ps: [ps.numpy])\"
      #:expression? #t
      #:run-command '(\"python\"))
"
  (define %installables
    (if (list? installables)
        (begin
          (when (null? installables)
            (error "no installables provided"))
          (when (and (not (= (length installables) 1))
                     expression?)
            (error "more than one Nix expressions provided"))
          installables)
        (list installables)))

  (define %options
    (if expression?
        (cons* "--impure" "--expr" options)
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
                    "shell" #$@%options #$@%installables
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

;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils nix)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; Guix build systems
  #:use-module (guix build-system trivial)
  ;; Guix packages
  #:autoload   (gnu packages package-management) (nix)
  #:export (%nix-shell-wrapper-default-unset-env-vars
            %nix-build-profile-paths
            %nix-build-profile-extra-outputs

            installables->nix-expressions
            nix-expressions->profile-build-wrapper

            nix-shell-wrapper
            nix-shell-wrapper->package

            with-nix-profile))

;; These search paths may contain incompatible libraries crash programs
;; loading them.  See also GCD 004:
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

;; Safe-to-use paths when extending search paths.
(define %nix-build-profile-paths
  '("/share/fonts"
    "/share/icons"
    "/share/info"
    "/share/man"
    ;; Completions.
    "/share/bash-completion/completions"
    "/share/fish/vendor_completions.d"
    "/share/zsh/site-functions"))

;; Additional outputs needed for %nix-build-profile-paths.
(define %nix-build-profile-extra-outputs
  '("man" "info"))

(define %nix-shell-wrapper-for-profile?
  (make-parameter #f))


;;;
;;; Helper utilities to use packages from Nix.
;;;

(define (ensure-list x)
  (if (list? x)
      x
      (list x)))

;; Flake output attribute -> Nix expression
;; https://nix.dev/manual/nix/2.34/command-ref/new-cli/nix.html#flake-output-attribute
;; 1. flakeref
;;    (import (builtins.getFlake "FLAKEREF") {})
;; 2. flakeref#attrpath
;;    (import (builtins.getFlake "FLAKEREF") {}).ATTRPATH
;; 3. expression
;;    (EXPRESSION)
;; 4. expression + attrpath
;;    (EXPRESSION).ATTRPATH
;; 5. file
;;    (import "FILE")
;; 6. file + attrpath
;;    (import "FILE").ATTRPATH

(define* (installables->nix-expressions #:optional (installables ".")
                                        #:key expression)
  "Return a list of G-expressions to format Nix expressions from Flake output
attributes.

INSTALLABLES (string / list of strings) is specified as Flake output attribute.
When EXPRESSION (string / file-like object) is set, INSTALLABLES will be
optional and interpreted as attribute paths relative to the Nix expression."
  (define (installable->flakeref+attrpath installable)
    (if (string-contains installable "#")
        (apply values (string-split installable #\#))
        (if expression
            (values #f installable)
            (values installable #f))))

  (map (lambda (installable)
         (let* ((flakeref
                 attrpath
                 (installable->flakeref+attrpath installable))
                (attrpath
                 (if (and=> attrpath (cut string=? <> "."))
                     #f
                     attrpath)))
           (match expression
             ((? file-like?)
              #~(format #f "(import ~s)~a"
                        #$expression
                        (if #$attrpath
                            (string-append "." #$attrpath)
                            "")))
             ((? string?)
              #~(format #f "(~a)~a"
                        #$expression
                        (if #$attrpath
                            (string-append "." #$attrpath)
                            "")))
             (_
              #~(format #f "(import (builtins.getFlake ~s) {})~a"
                        #$flakeref
                        (if #$attrpath
                            (string-append "." #$attrpath)
                            ""))))))
       (ensure-list installables)))

(define* (nix-expressions->profile-build-wrapper
          expressions
          #:key
          link-to
          (paths-to-link %nix-build-profile-paths)
          (extra-outputs-to-install %nix-build-profile-extra-outputs)
          (nixpkgs-commit "714a5f8c4ead6b31148d829288440ed033ccc041")
          (nix (file-append nix "/bin/nix")))
  "Return a file-like object that wraps the \"nix build\" command-line utility
and builds a Nix profile if run.  Nix daemon is required to use the wrapper.

EXPRESSIONS (list of strings / list of G-expressions) can be formatted from
'installables->nix-expressions' and specifies packages to be added into the
profile.

If set, the resulted profile will be symlinked to LINK-TO (string).  This also
prevents garbage collection of the profile.

PATHS-TO-LINK (default: %nix-build-profile-paths, list of strings) limits
subdirectories of packages to be included into the profile.  All subdirectories
will be included if specifying '(\"/\").

EXTRA-OUTPUTS-TO-INSTALL (default: %nix-build-profile-extra-outputs, list of
strings) specifies additional outputs of packages to be included into the
profile.

NIXPKGS-COMMIT (default: 714a5f8c4ead6b31148d829288440ed033ccc041, string)
specifies Nixpkgs revision to provide the buildEnv function.

NIX (default: (file-append nix \"/bin/nix\"), string / file-like object)
specifies the Nix binary to use."
  (define profile.nix
    (computed-file "profile.nix"
      #~(begin
          (use-modules (ice-9 format))
          (call-with-output-file #$output
            (lambda (port)
              (format port "\
let
  pkgs = import (builtins.getFlake \"github:NixOS/nixpkgs/~a\") {};
in
  pkgs.buildEnv {
    name = \"nix-profile-for-search-paths\";
    paths = [
~{\
      ~a
~}\
    ];
    pathsToLink = [
~{\
      ~s
~}\
    ];
    extraOutputsToInstall = [
~{\
      ~s
~}\
    ];
  }
"
                      #$nixpkgs-commit
                      (list #$@expressions)
                      '#$paths-to-link
                      '#$extra-outputs-to-install))))
      #:options '(#:substitutable? #f)))

  (program-file "build-nix-profile-nix-wrapper"
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (invoke #$nix "build"
                  "--print-out-paths"
                  #$@(if link-to
                         (list "--out-link" link-to)
                         (list "--no-link"))
                  "--file" #$profile.nix)))))

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

  (define wrapper
    (program-file (string-append name "-nix-wrapper")
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
                           (_
                            '()))
                      #$@(ensure-list installables)
                      "--command" #$@run-command args)))))))

  (if (%nix-shell-wrapper-for-profile?)
      (list 'nix-shell-wrapper wrapper installables expression)
      wrapper))

(define (nix-shell-wrapper->package wrapper)
  "Return a package for WRAPPER, file-like object created by 'nix-shell-wrapper'
or 'nix-expressions->profile-build-wrapper'.  The package will install a command
under its /bin directory, with the same name as WRAPPER.

Note that packages created by this procedure are not supposed to be used in the
build environment and won't be able to interoperate with other packages in
practice."
  (let* ((wrapper-name (program-file-name wrapper))
         (command (string-drop-right wrapper-name (string-length "-nix-wrapper"))))
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

(define-syntax-rule (with-nix-profile path packages)
  "Transform PACKAGES, turning wrappers created by 'nix-shell-wrapper' into
packages, along with a package containing the build-nix-profile script created
for them by 'nix-expressions->profile-build-wrapper'.

When invoking the build-nix-profile script, the resulted Nix profile will be
linked to PATH.

This macro is intended for use in Guix System and Guix Home package
declarations.  Note that calls to 'nix-shell-wrapper' must be in the scope of
this macro.

Example:

    (with-nix-profile \"/tmp/test\"
      (list hello

            (nix-shell-wrapper \"cowsay-hello-coreutils-env\"
              '(\"github:NixOS/nixpkgs/714a5f8c4ead6b31148d829288440ed033ccc041#coreutils\"
                \"github:NixOS/nixpkgs/714a5f8c4ead6b31148d829288440ed033ccc041#cowsay\")
              #:run-command '(\"env\" \"LANG=C.UTF-8\" \"cowsay\" \"hello\"))

            (nix-shell-wrapper \"cowsay-hello\"
              \"github:NixOS/nixpkgs/714a5f8c4ead6b31148d829288440ed033ccc041#cowsay\"
              #:run-command '(\"cowsay\" \"hello\")
              #:environment-set '((\"LANG\" . \"C.UTF-8\")))

            (nix-shell-wrapper \"python-with-numpy\"
              #:expression \"with (import (builtins.getFlake \\\"github:NixOS/nixpkgs/714a5f8c4ead6b31148d829288440ed033ccc041\\\") {}); python3.withPackages (ps: [ps.numpy])\"
              #:run-command '(\"python\"))

            (nix-shell-wrapper \"coreutils-env\"
              \"coreutils\"
              #:expression (local-file \"/tmp/nixpkgs.nix\")
              #:run-command '(\"env\")
              #:environment-keep #f)))
"
  (let ((wrappers
         others
         (partition (match-lambda
                      (('nix-shell-wrapper _ _ _) #t)
                      (_ #f))
                    (parameterize ((%nix-shell-wrapper-for-profile? #t))
                      packages))))
    `(,(nix-shell-wrapper->package
        (nix-expressions->profile-build-wrapper
         (append-map (match-lambda
                       ((_ _ installables expression)
                        (installables->nix-expressions
                         installables #:expression expression)))
                     wrappers)
         #:link-to path))
      ,@(map (match-lambda
               ((_ wrapper _ _)
                (nix-shell-wrapper->package wrapper)))
             wrappers)
      ,@others)))

;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils nix)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; Guix build systems
  #:use-module (guix build-system trivial)
  ;; Guix packages
  #:autoload   (gnu packages package-management) (nix)
  #:export (nix-shell-wrapper
            nix-shell-wrapper->package
            with-nix-profile))

;; These search paths may contain incompatible libraries and crash programs
;; loading them.  See also GCD 004:
;; https://consensus.guix.gnu.org/gcd/004-set-search-paths-without-program-wrappers.html
;;
;; Valid items below, paths are started with "/":
;;   ENV_VAR
;;   '(ENV_VAR . PATH)
;;   '(ENV_VAR . (PATHS ...))

(define %nix-wrapper-default-exclude-env-paths
  '((#f                     . ("/bin" "/sbin"))
    "XDG_DATA_DIRS"
    "XDG_CONFIG_DIRS"
    ;; Glib
    ("GIO_EXTRA_MODULES"    . "/lib/gio/modules")
    ("GSETTINGS_SCHEMA_DIR" . "/share/glib-2.0/schemas")
    ;; Qt
    ("QML2_IMPORT_PATH"     . "/lib/qt5/qml")
    ("QML_IMPORT_PATH"      . "/lib/qt6/qml")
    ("QT_PLUGIN_PATH"       . ("/lib/qt5/plugins" "/lib/qt6/plugins"))))

;; Additional outputs added to the profile we build.
(define %nix-profile-extra-outputs
  '("man" "info"))

(define %nix-wrapper-default-unset-env-vars
  (filter-map (match-lambda
                ((? string? env) env)
                (((? string? env) . _) env)
                (_ #f))
              %nix-wrapper-default-exclude-env-paths))

(define %nix-wrapper-default-exclude-paths
  (filter identity
          (append-map (match-lambda
                        ((_ . (? string? path)) (list path))
                        ((_ (? string? paths) ...) paths)
                        (_ (list #f)))
                      %nix-wrapper-default-exclude-env-paths)))

(define-record-type <nix-wrapper>
  (nix-wrapper name command file expressions)
  nix-wrapper?
  (name        nix-wrapper-name)         ;string
  (command     nix-wrapper-command)      ;string
  (file        nix-wrapper-file)         ;file-like object
  (expressions nix-wrapper-expressions)) ;list of strings / list of G-expressions

(define-gexp-compiler (nix-wrapper-compiler (wrapper <nix-wrapper>) system target)
  (lower-object (nix-wrapper-file wrapper) system #:target target))


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
          (paths-to-exclude %nix-wrapper-default-exclude-paths)
          (extra-outputs-to-install %nix-profile-extra-outputs)
          (nixpkgs-commit "714a5f8c4ead6b31148d829288440ed033ccc041")
          (nix (file-append nix "/bin/nix")))
  "Return a file-like object that wraps the \"nix build\" command-line utility
and builds a Nix profile if run.  Nix daemon is required to use the wrapper.

The wrapper accepts an optional path argument and will symlink the resulted
profile to the path.

EXPRESSIONS (list of strings / list of G-expressions) can be formatted from
'installables->nix-expressions' and specifies packages to be added into the
profile.

PATHS-TO-EXCLUDE (default: %nix-wrapper-default-exclude-paths, list of strings)
excludes subdirectories from being added into the profile.

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
          (use-modules (ice-9 format)
                       (srfi srfi-1))
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
    extraOutputsToInstall = [
~{\
      ~s
~}\
    ];
    postBuild = ''
~{\
      rm -rf $out~a
~}\
    '';
  }
"
                      #$nixpkgs-commit
                      (delete-duplicates (list #$@expressions) string=?)
                      '#$extra-outputs-to-install
                      '#$paths-to-exclude))))
      #:options '(#:substitutable? #f)))

  (define wrapper
    (program-file "build-nix-profile-nix-wrapper"
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (ice-9 match)
                         (guix build utils))
            (match (command-line)
              ((_ . args)
               (apply invoke #$nix
                      "build" "--print-out-paths"
                      `(,@(if (null? args)
                              (list "--no-link")
                              (list "--out-link" (car args)))
                        "--file" #$profile.nix))))))))

  (nix-wrapper "build-nix-profile-nix-wrapper"
               "build-nix-profile"
               wrapper
               expressions))

;; See also https://nix.dev/manual/nix/2.34/command-ref/new-cli/nix3-env-shell.html
(define* (nix-shell-wrapper name
                            #:optional (installables ".")
                            #:key expression
                            (run-command '())
                            (options '())
                            (environment-keep #t)
                            (environment-unset %nix-wrapper-default-unset-env-vars)
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

ENVIRONMENT-UNSET (default: %nix-wrapper-default-unset-env-vars, list of
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

  (define wrapper-name
    (string-append name "-nix-wrapper"))

  (define wrapper
    (program-file wrapper-name
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

  (nix-wrapper wrapper-name
               name
               wrapper
               (installables->nix-expressions
                installables
                #:expression expression)))

(define (nix-shell-wrapper->package wrapper)
  "Return a package for WRAPPER, file-like object created by 'nix-shell-wrapper'
or 'nix-expressions->profile-build-wrapper'.  The package will install a command
under its /bin directory, with the command name of WRAPPER.

Note that packages created by this procedure are not supposed to be used in the
build environment and won't be able to interoperate with other packages in
practice."
  (package
    (name (nix-wrapper-name wrapper))
    (version "0.0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           (with-imported-modules '((guix build utils))
             #~(begin
                 (use-modules (guix build utils))
                 (let ((dest (string-append #$output "/bin/"
                                            #$(nix-wrapper-command wrapper))))
                   (mkdir-p (dirname dest))
                   (symlink #$wrapper dest))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define (with-nix-profile packages)
   "Transform PACKAGES, turning wrappers created by 'nix-shell-wrapper' into
packages, additionally adding a package containing a build-nix-profile script
created for them by 'nix-expressions->profile-build-wrapper'.

The build-nix-profile script accepts an optional path argument and will symlink
the resulted profile to the path.

This procedure is intended for use in Guix System and Guix Home package
declarations.

Example:

    (with-nix-profile
     (append (list (nix-shell-wrapper \"readest\"
                     '(\"github:NixOS/nixpkgs/714a5f8c4ead6b31148d829288440ed033ccc041#readest\")
                     #:run-command '(\"readest\")
                     #:environment-set '((\"LANG\" . \"C.UTF-8\"))))
             %base-packages))
"
  (let ((wrappers others (partition nix-wrapper? packages)))
    (append (list (nix-shell-wrapper->package
                   (nix-expressions->profile-build-wrapper
                    (append-map nix-wrapper-expressions wrappers))))
            (map nix-shell-wrapper->package wrappers)
            others)))

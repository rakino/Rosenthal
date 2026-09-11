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
  #:export (%nix-wrapper-default-exclude-env-paths
            %nix-wrapper-profile-nixpkgs-commit
            %nix-wrapper-profile-extra-outputs

            nix-shell-wrapper
            nix-wrapper->package nix-shell-wrapper->package
            with-nix-profile))

;; Environment variables and subdirectories to exclude in `nix-shell-wrapper'
;; and `nix-expressions->profile-build-wrapper', respectively.
;;
;; This is to avoid adding search paths with incompatible libraries and crashing
;; programs.  See also GCD 004:
;; https://consensus.guix.gnu.org/gcd/004-set-search-paths-without-program-wrappers.html
;;
;; Valid item format below.  Paths are started with `/' and non-string values
;; are ignored:
;;
;;   ENV_VAR
;;   (ENV_VAR . PATH)
;;   (ENV_VAR . (PATHS ...))
(define %nix-wrapper-default-exclude-env-paths
  (make-parameter
   '((#f                     . ("/bin" "/sbin"))
     "XDG_DATA_DIRS"
     "XDG_CONFIG_DIRS"
     ;; Glib
     ("GIO_EXTRA_MODULES"    . "/lib/gio/modules")
     ("GSETTINGS_SCHEMA_DIR" . "/share/glib-2.0/schemas")
     ;; Qt
     ("QML2_IMPORT_PATH"     . "/lib/qt5/qml")
     ("QML_IMPORT_PATH"      . "/lib/qt6/qml")
     ("QT_PLUGIN_PATH"       . ("/lib/qt5/plugins" "/lib/qt6/plugins")))))

;; Nixpkgs revision to provide `buildEnv' function when building the profile,
;; used by `nix-expressions->profile-build-wrapper'.
(define %nix-wrapper-profile-nixpkgs-commit
  (make-parameter "714a5f8c4ead6b31148d829288440ed033ccc041"))

;; Additional package outputs to include when building the profile, used by
;; `nix-expressions->profile-build-wrapper'.
(define %nix-wrapper-profile-extra-outputs
  (make-parameter '("man" "info")))

(define (%nix-wrapper-profile-default-exclude-paths)
  "Subdirectories to exclude when building the profile, used by
`nix-expressions->profile-build-wrapper'."
  (filter identity
          (append-map (match-lambda
                        ((_ . (? string? path)) (list path))
                        ((_ (? string? paths) ...) paths)
                        (_ (list #f)))
                      (%nix-wrapper-default-exclude-env-paths))))

(define (%nix-wrapper-shell-default-unset-env-vars)
  "Environment variables to unset in the `nix shell' environment, used by
`nix-shell-wrapper' by default."
  (filter-map (match-lambda
                ((? string? env) env)
                (((? string? env) . _) env)
                (_ #f))
              (%nix-wrapper-default-exclude-env-paths)))


;;;
;;; Helper utilities to use packages from Nix.
;;;

(define (ensure-list x)
  (if (list? x)
      x
      (list x)))

(define-record-type <nix-wrapper>
  (nix-wrapper name command file expressions)
  nix-wrapper?
  ;; For `nix-wrapper->package', package name.
  (name        nix-wrapper-name)         ;string
  ;; For `nix-wrapper->package', command name.
  (command     nix-wrapper-command)      ;string
  ;; Wrapper script.
  (file        nix-wrapper-file)         ;file-like object
  ;; Nix expression equivalents to Nix dependencies of the wrapper.
  (expressions nix-wrapper-expressions)) ;list of strings / list of G-expressions

(define-gexp-compiler (nix-wrapper-compiler (wrapper <nix-wrapper>) system target)
  (lower-object (nix-wrapper-file wrapper) system #:target target))

(define (nix-wrapper->package wrapper)
  "Return a package for nix-wrapper WRAPPER.  The package will install a command
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

(define nix-shell-wrapper->package nix-wrapper->package) ;alias

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
optional and interpreted as attribute path relative to the Nix expression."
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
          #:key (nix (file-append nix "/bin/nix")))
  "Return a nix-wrapper that wraps the `nix build' command-line utility and
builds a Nix profile if run.  Nix daemon is required to use the wrapper.

The wrapper accepts an optional path argument and will symlink the resulted
profile to the path.

EXPRESSIONS (list of strings / list of G-expressions) can be formatted from
`installables->nix-expressions' and specifies packages to be added into the
profile.

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
      pkgs.shared-mime-info
      pkgs.desktop-file-utils
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
      if [[ -w $out/share/mime ]] && [[ -d $out/share/mime/packages ]]; then
        XDG_DATA_DIRS=$out/share ${pkgs.shared-mime-info}/bin/update-mime-database -V $out/share/mime
      fi
      if [[ -w $out/share/applications ]]; then
        ${pkgs.desktop-file-utils}/bin/update-desktop-database $out/share/applications
      fi
~{\
      rm -rf $out~a
~}\
    '';
  }
"
                      #$(%nix-wrapper-profile-nixpkgs-commit)
                      (delete-duplicates (list #$@expressions) string=?)
                      '#$(%nix-wrapper-profile-extra-outputs)
                      '#$(%nix-wrapper-profile-default-exclude-paths)))))
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
                            (environment-unset (%nix-wrapper-shell-default-unset-env-vars))
                            (environment-set '())
                            (nix (file-append nix "/bin/nix")))
  "Return a nix-wrapper that wraps the `nix shell' command-line utility and
spawns an one-off software environment if run.  Nix daemon is required to use
the wrapper.

INSTALLABLES (string / list of strings) is specified as Flake output attribute
and will be added into the environment.  When EXPRESSION (string / file-like
object) is set, INSTALLABLES will be optional and interpreted as attribute paths
relative to the Nix expression.

RUN-COMMAND (default: '(), list of strings) specifies command and arguments to
be executed in the environment.  Command-line arguments passed to the wrapper
will be appended.

OPTIONS (default: '(), list of strings) specifies extra options to pass to the
`nix shell' command-line utility.

When ENVIRONMENT-KEEP (default: #t, boolean / list of strings) is set to a value
other than #t, the environment will be cleared, keeping only specified
environment variables.

ENVIRONMENT-UNSET (default: (%nix-wrapper-shell-default-unset-env-vars), list of
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
                           (lset-difference string=?
                                            environment-unset
                                            (append
                                             (if (boolean? environment-keep)
                                                 '()
                                                 environment-keep)
                                             (map car environment-set)))))
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

(define (with-nix-profile packages)
   "Transform PACKAGES, turning nix-wrapper into packages, additionally adding a
new package containing a `build-nix-profile' script created for them by
`nix-expressions->profile-build-wrapper'.

The `build-nix-profile' script accepts an optional path argument and will
symlink the resulted profile to that path.

This procedure is intended for use in Guix System and Guix Home package
declarations, along with services `nix-search-paths-service-type' and
`home-nix-search-paths-service-type'.

Example:

    (with-nix-profile
     (append (list (nix-shell-wrapper \"readest\"
                     '(\"github:NixOS/nixpkgs/714a5f8c4ead6b31148d829288440ed033ccc041#readest\")
                     #:run-command '(\"readest\")
                     #:environment-set '((\"LANG\" . \"C.UTF-8\"))))
             %base-packages))
"
  (let ((wrappers others (partition nix-wrapper? packages)))
    (append (list (nix-wrapper->package
                   (nix-expressions->profile-build-wrapper
                    (append-map nix-wrapper-expressions wrappers))))
            (map nix-wrapper->package wrappers)
            others)))

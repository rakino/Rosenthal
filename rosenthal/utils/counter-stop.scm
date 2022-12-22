;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal utils counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages less)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages nvi)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal utils kicksecure)
  #:export (%guix-authorized-key-nonguix
            %guix-authorized-key-tobias

            normalize-package

            %xdg-base-directory-environment-variables

            %rosenthal-default-kernel-arguments
            %rosenthal-default-keyboard-layout
            %rosenthal-base-initrd-modules
            %rosenthal-base-file-systems
            %rosenthal-base-packages
            %rosenthal-base-services))

;; Common procedures and variables shared across my home environment and
;; operating system definitions.

;; Keys
;; https://substitutes.nonguix.org/signing-key.pub
(define %guix-authorized-key-nonguix
  (plain-file "nonguix.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

;; https://guix.tobias.gr/signing-key.pub
(define %guix-authorized-key-tobias
  (plain-file "tobias.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #E21911E159DB6D031A763509A255B054360A4A96F5668CBBAC48052E67D274D3#)))"))

;; Procedures
(define (normalize-package pkg)
  (if (package? pkg)
      `(,pkg "out")
      pkg))

;; Variables
;; Source: <https://wiki.archlinux.org/title/XDG_Base_Directory>
(define %xdg-base-directory-environment-variables
  '(;; XDG Cache Home
    ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")

    ;; XDG Config Home
    ("AWS_CONFIG_FILE" . "$XDG_CONFIG_HOME/aws/config")
    ("AWS_SHARED_CREDENTIALS_FILE" . "$XDG_CONFIG_HOME/aws/credentials")
    ("INPUTRC" . "$XDG_CONFIG_HOME/readline/inputrc")
    ("MBSYNCRC" . "$XDG_CONFIG_HOME/isync/mbsyncrc")
    ("NPM_CONFIG_USERCONFIG" . "$XDG_CONFIG_HOME/npm/npmrc")
    ("WAKATIME_HOME" . "$XDG_CONFIG_HOME/wakatime")
    ("WGETRC" . "$XDG_CONFIG_HOME/wgetrc")

    ;; XDG Data Home
    ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
    ("GDBHISTFILE" . "$XDG_DATA_HOME/gdb/history")
    ("GNUPGHOME" . "$XDG_DATA_HOME/gnupg")
    ("GOPATH" . "$XDG_DATA_HOME/go")
    ("PASSWORD_STORE_DIR" . "$XDG_DATA_HOME/pass")))

(define %rosenthal-default-kernel-arguments
  `(,@(delete "nosmt=force"
              %kicksecure-kernel-arguments)
    "net.ifnames=0"
    "nmi_watchdog=0"))

(define %rosenthal-default-keyboard-layout
  (keyboard-layout "us" "dvorak"
                   #:options '("ctrl:nocaps")))

(define %rosenthal-base-initrd-modules
  '("btrfs" "xxhash_generic"))

(define %rosenthal-base-file-systems
  (cons* (file-system
           (device "none")
           (mount-point "/tmp")
           (type "tmpfs")
           (check? #f))

         (file-system
           (device "none")
           (mount-point "/run")
           (type "tmpfs")
           (needed-for-boot? #t)
           (check? #f))

         (file-system
           (device "none")
           (mount-point "/var/run")
           (type "tmpfs")
           (needed-for-boot? #t)
           (check? #f))

         (delete %debug-file-system
                 %base-file-systems)))

(define %rosenthal-base-packages
  (let ((to-add    (list curl
                         mosh
                         nss-certs
                         unzip
                         zstd))
        (to-remove (list bash-completion
                         info-reader
                         mg
                         nano
                         nvi
                         inetutils
                         isc-dhcp
                         iw
                         wireless-tools)))
    (append to-add (lset-difference eqv? %base-packages to-remove))))

(define %rosenthal-base-services
  (cons* (service ntp-service-type)

         (service openssh-service-type
                  (openssh-configuration
                   (permit-root-login 'prohibit-password)))

         (rngd-service)

         (modify-services %base-services
           (sysctl-service-type
            config => (sysctl-configuration
                       (inherit config)
                       (settings `(,@%kicksecure-sysctl-rules
                                   ("net.core.rmem_max" . "2500000")
                                   ("net.ipv4.tcp_sack" . "0")
                                   ("net.ipv4.tcp_dsack" . "0")
                                   ("net.ipv4.tcp_fack" . "0")
                                   ("vm.page-cluster" . "0")
                                   ("vm.swappiness" . "90")))))
           (guix-service-type
            config => (guix-configuration
                       (inherit config)
                       (substitute-urls
                        (append %default-substitute-urls
                                '("https://nonguix.org"
                                  "https://guix.tobias.gr")))
                       (authorized-keys
                        (cons* %guix-authorized-key-nonguix
                               %guix-authorized-key-tobias
                               %default-authorized-guix-keys)))))))

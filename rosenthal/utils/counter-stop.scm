;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal utils counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
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
  #:export (%channel-guix
            %channel-nonguix
            %channel-rosenthal

            %guix-authorized-key-dorphine
            %guix-authorized-key-nonguix

            normalize-package

            %xdg-base-directory-environment-variables

            %rosenthal-default-channels
            %rosenthal-default-kernel-arguments
            %rosenthal-default-keyboard-layout
            %rosenthal-base-initrd-modules
            %rosenthal-base-file-systems
            %rosenthal-base-packages
            %rosenthal-base-services))

;; Common procedures and variables shared across my home environment and
;; operating system definitions.

;; Channels
(define %channel-guix
  (first %default-channels))

(define %channel-nonguix
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define %channel-rosenthal
  (channel
   (name 'rosenthal)
   (url "https://github.com/rakino/rosenthal")
   (branch "trunk")
   (introduction
    (make-channel-introduction
     "7677db76330121a901604dfbad19077893865f35"
     (openpgp-fingerprint
      "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7")))))

(define %rosenthal-default-channels
  (list %channel-guix
        %channel-nonguix
        %channel-rosenthal))

;; Keys
;; local
(define %guix-authorized-key-dorphine
  (plain-file "dorphine.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #BBE816F9D051E8B715F17DA26B674462DF1967AC77A4130CA3306878314B84AC#)))"))

;; https://substitutes.nonguix.org/signing-key.pub
(define %guix-authorized-key-nonguix
  (plain-file "nonguix.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

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
    "net.ifnames=0"))

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
  (let ((to-add (list nss-certs))
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
  (cons* (modify-services %base-services
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
                                '("https://substitutes.nonguix.org")))
                       (authorized-keys
                        (cons* %guix-authorized-key-nonguix
                               %default-authorized-guix-keys)))))))

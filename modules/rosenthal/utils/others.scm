;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils others)
  #:use-module (guix gexp)
  #:use-module (rosenthal utils file)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:export (%rosenthal-guix-key-dorphine
            %rosenthal-guix-key-nuporta
            %rosenthal-guix-key-bocis
            %rosenthal-guix-key-ignamma

            %rosenthal-ssh-key-hako
            %rosenthal-ssh-key-hako-deploy
            %rosenthal-ssh-key-jonathan
            %rosenthal-ssh-key-podiki

            %rosenthal-network-manager-ipv6-privacy
            %rosenthal-network-manager-random-mac-address

            %rosenthal-cli-packages))


;;;
;;; Guix public keys
;;;

(define %rosenthal-guix-key-dorphine
  (plain-file "dorphine.pub"
    "(public-key (ecc (curve Ed25519)
(q #A279175682D0DAE3E11268E67E1F3FA47C38D7E509F7725567CF891E248E719F#)))"))

;; Nonguix head node.
(define %rosenthal-guix-key-nuporta
  (plain-file "nuporta.pub"
    "(public-key (ecc (curve Ed25519)
(q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

;; aarch64-linux build worker.
(define %rosenthal-guix-key-bocis
  (plain-file "bocis.pub"
    "(public-key (ecc (curve Ed25519)
(q #7927EA1162184C1FAA62D20C111121A4604F00956E69F0FEB89EEE1721647897#)))"))

;; x86_64-linux + aarch64-linux build worker.
(define %rosenthal-guix-key-ignamma
  (plain-file "ignamma.pub"
    "(public-key (ecc (curve Ed25519)
(q #6FEEB15C4363F9975EB15C908EC911A4362E486DA642431FA2438C0B1C3D55F5#)))"))


;;;
;;; SSH public keys
;;;

(define %rosenthal-ssh-key-hako
  (plain-file "hako.pub"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFcTj1N3cL/bh2Uvwh5/YubhZplPFnvGk/iVHQs3FWV2 openpgp:0x77671915\n"))
(define %rosenthal-ssh-key-hako-deploy
  (plain-file "hako-deploy.pub"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMLWIp8y5/JGBaw+yFA5MFB5nlFpEx/tjc0q0Ij9KjTu\n"))

(define %rosenthal-ssh-key-jonathan
  (plain-file "jonathan.pub"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHgzHvP3BRTIZ960LVglrK8w/C0+6Z5VM8/Q5Uwa0o+Z jonathan@3700X\n"))

(define %rosenthal-ssh-key-podiki
  (plain-file "podiki.pub"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDaSmW/3uq5L6ZP6gWmRw5RiTTg0es1PrbAo/x4vkPzwIKTrMFOCBCmcuH3vOCkEZJtNy3OpXbt/a3tDW+cc6dkeq2H4WpogQvyMTXreFS2phMgDTEXW2gGZIP6fA33CHERmhd9A/m0A+NH5KGAmLDQNK8QgPgIjZuseJYtYHNCnN2TCsWQYnbZtVQF5CS6iBUILpVp6p7QlSUokiCGaPjZfrjSFCm1hUPjJYSkv0NTq8TzyDfU2quqP7TBCj4WBi9HoW9+a8tN2TQ/+GYbGqlFljeNdz3vzItcHjidHOQL/42mpvzgZx7o7dtrqX9stp+mI3oBREYSD0bMyvND/dEBRWIbpFvbyYx/leMKq9yUcFNyI2lztk17ObaQkDLxlq4ClytgEtdbP6X0gua29FYK/YlAi13NptK6uy2xB2gsEIt5P4N3u+gZCNA0U3IVd7iMRSpg6PWiL1JguvhYSD5vGOnOjiXVlBCKn+ErTO9Ey/BZqwVBZMeDwynFnU1mYnkxtA+G54VI77gj24FrHw/ClOdJOdBUGAso9P3sFjdykkAJyKd4jiFzpDTOOJNs8qKhmFFzJBnJjn7nzwjElwOCZXdDKTrKqF/51WEqpNr8Za2QjRirV4m7n6FnyyD38b24InAVa+yze3qDI9yk2vjPdtFGCeLODSEjfV3U1z1hiw== cardno:11 465 639\n"))


;;;
;;; NetworkManager
;;;

(define %rosenthal-network-manager-ipv6-privacy
  `("ip6-privacy.conf"
    ,(ini-file "ip6-privacy.conf"
       '(("connection"
          . (("ipv6.ip6-privacy" . 2)))))))

;; NOTE: When using on cloud machines, refer to the terms of the provider
;; first.
(define %rosenthal-network-manager-random-mac-address
  `("random-mac-address.conf"
    ,(ini-file "random-mac-address.conf"
       '(("connection-mac-randomization"
          . (("ethernet.cloned-mac-address" . "stable")
             ("wifi.cloned-mac-address" . "stable")))))))


;;;
;;; Packages
;;;

(define %rosenthal-cli-packages
  (list curl
        file
        git
        `(,git "send-email")
        glibc
        gnupg
        htop
        mosh
        ncurses
        rclone
        rsync
        unzip
        zip))

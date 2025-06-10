;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services cuirass)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (rosenthal utils packages)
  #:use-module (gnu system)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)
  #:export (cuirass-worker-container-configuration
            cuirass-worker-container-configuration?
            this-cuirass-worker-container-configuration
            cuirass-worker-container-host-name
            cuirass-worker-container-server
            cuirass-worker-container-supported-systems
            cuirass-worker-container-substitute-urls
            cuirass-worker-container-script
            cuirass-worker-container-activation
            cuirass-worker-container-shepherd
            cuirass-worker-container-service-type))

;;;
;;; Run Cuirass remote worker in container.
;;;

(define-record-type* <cuirass-worker-container-configuration>
  cuirass-worker-container-configuration
  make-cuirass-worker-container-configuration
  cuirass-worker-container-configuration?
  this-cuirass-worker-container-configuration
  (host-name         cuirass-worker-container-host-name)
  (server            cuirass-worker-container-server)
  (supported-systems cuirass-worker-container-supported-systems
                     (default (list (%current-system)))
                     (thunked))
  (substitute-urls   cuirass-worker-container-substitute-urls
                     (default %default-substitute-urls))
  ;; Internal.
  (container-script  cuirass-worker-container-script
                     (default (%cuirass-worker-container-script
                               this-cuirass-worker-container-configuration))
                     (thunked))
  ;; Extensions.
  (activation        cuirass-worker-container-activation
                     (default (%cuirass-worker-container-activation
                               this-cuirass-worker-container-configuration))
                     (thunked))
  (shepherd          cuirass-worker-container-shepherd
                     (default (%cuirass-worker-container-shepherd
                               this-cuirass-worker-container-configuration))
                     (thunked)))

(define %cuirass-worker-container-script
  (match-record-lambda <cuirass-worker-container-configuration>
      (host-name server supported-systems substitute-urls)
    (define cuirass-remote-worker-for-container
      (service-type
       (name 'cuirass-remote-worker)
       (extensions
        (list (service-extension shepherd-root-service-type
                                 (compose
                                  (lambda (services)
                                    (map
                                     (lambda (s)
                                       (if (member 'cuirass-remote-worker
                                                   (shepherd-service-provision s))
                                           (shepherd-service
                                             (inherit s)
                                             (requirement
                                              (lset-difference
                                               eqv?
                                               (shepherd-service-requirement s)
                                               '(avahi-daemon guix-daemon))))
                                           s))
                                     services))
                                  (@@ (gnu services cuirass)
                                      cuirass-remote-worker-shepherd-service)))
              (service-extension account-service-type
                                 (const
                                  (@@ (gnu services cuirass)
                                      %cuirass-remote-worker-accounts)))))
       (description
        "Run the Cuirass remote build worker service.")))
    (define os
      (operating-system
        (bootloader
         (bootloader-configuration
           (bootloader grub-bootloader)
           (targets '("/dev/sda"))))
        (file-systems
         (cons (file-system
                 (mount-point "/")
                 (device "nothing")
                 (type "dummy"))
               %base-file-systems))
        (kernel linux-libre-lts)

        (host-name host-name)
        (services
         (cons (service cuirass-remote-worker-for-container
                 (cuirass-remote-worker-configuration
                   (cuirass (pkg "cuirass-hako"))
                   (server server)
                   (systems supported-systems)
                   (publish-port 5558)
                   (substitute-urls substitute-urls)))
               %base-services))))
    (with-store store
      (run-with-store store
        (container-script os #:shared-network? #t)))))

(define (%cuirass-worker-container-activation _)
  (with-imported-modules (source-module-closure '((guix build utils)))
    #~(begin
        (use-modules (guix build utils))
        (let ((log-file "/var/log/cuirass-worker-container.log"))
          (mkdir-p (dirname log-file))
          ;; Clear log on start.
          (call-with-output-file log-file (const #t))))))

(define %cuirass-worker-container-shepherd
  (match-record-lambda <cuirass-worker-container-configuration>
      (container-script)
    (list (shepherd-service
            (provision '(cuirass-worker-container))
            (requirement '(guix-daemon user-processes))
            (start
             #~(make-forkexec-constructor
                (list #$container-script
                      "--expose=/etc/guix"
                      "--share=/var/guix/daemon-socket/socket"
                      (string-append
                       "--share=" (string-join
                                   '("/var/log/cuirass-worker-container.log"
                                     "/var/log/cuirass-remote-worker.log")
                                   "=")))))
            (stop
             #~(make-kill-destructor))))))

(define cuirass-worker-container-service-type
  (service-type
   (name 'cuirass-worker-container)
   (extensions
    (list (service-extension activation-service-type
                             cuirass-worker-container-activation)
          (service-extension shepherd-root-service-type
                             cuirass-worker-container-shepherd)))
   (description "")))

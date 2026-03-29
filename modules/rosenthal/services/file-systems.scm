;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services file-systems)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  ;; Guix System
  #:use-module (gnu system pam)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  ;; Guix packages
  #:use-module (gnu packages backup)
  #:use-module (gnu packages file-systems)
  #:use-module (rosenthal packages admin)
  #:export (btrbk-service-type
            btrbk-configuration

            dumb-runtime-dir-service-type

            zfs-service-type
            zfs-configuration))


;;;
;;; Btrbk
;;;


(define-configuration btrbk-configuration
  (btrbk
   (file-like btrbk)
   "@code{btrbk} package to use.")
  (config-file
   (file-like (plain-file "empty" ""))
   "File-like object for btrbk configuration, see also @code{btrbk.conf(5)}.")
  (no-serialization))

(define (btrbk-etc-service config)
  `(("btrbk/btrbk.conf" ,(btrbk-configuration-config-file config))))

(define (btrbk-mcron-jobs config)
  (list #~(job next-hour-from
               #$(file-append (btrbk-configuration-btrbk config)
                              "/bin/btrbk run --quiet"))))

(define btrbk-service-type
  (service-type
   (name 'btrbk)
   (extensions
    (list (service-extension etc-service-type
                             btrbk-etc-service)
          (service-extension mcron-service-type
                             btrbk-mcron-jobs)))
   (default-value (btrbk-configuration))
   (description "Configure and run btrbk hourly.")))


;;;
;;; pam-dumb-runtime-dir
;;;


(define dumb-runtime-dir-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/run/user")
      (chmod "/run/user" #o0755)))

(define dumb-runtime-dir-pam-service
  (let ((optional-pam-entry
         (pam-entry
          (control "optional")
          (module
           (file-append
            pam-dumb-runtime-dir "/lib/security/pam_dumb_runtime_dir.so")))))
    (list (pam-extension
           (transformer
            (lambda (pam)
              (if (string=? (pam-service-name pam) "login")
                  (pam-service
                   (inherit pam)
                   (session
                    (cons optional-pam-entry
                          (pam-service-session pam))))
                  pam)))))))

(define dumb-runtime-dir-service-type
  (service-type
   (name 'dumb-runtime-dir)
   (extensions
    (list (service-extension activation-service-type
                             (const dumb-runtime-dir-activation))
          (service-extension pam-root-service-type
                             (const dumb-runtime-dir-pam-service))))
   (default-value #f)                   ;No default value required.
   (description "Create @code{XDG_RUNTIME_DIR} on login and never remove it.")))


;;;
;;; ZFS
;;;

(define-configuration/no-serialization zfs-configuration
  (zfs
   (file-like zfs)
   "ZFS package to use.")
  (kernel-has-zfs-module?
   (boolean #f)
   "Whether ZFS module has already been built into the kernel.")
  (volumes?
   (boolean #f)
   "Wait for ZFS volumes to show up.")
  (auto-mount?
   (boolean #t)
   "Auto-mount ZFS datasets."))

(define zfs-linux-loadable-module-service
  (match-record-lambda <zfs-configuration>
      (zfs kernel-has-zfs-module?)
    (if kernel-has-zfs-module?
        '()
        (list `(,zfs "module")))))

(define add-zfs-package
  (match-record-lambda <zfs-configuration>
      (zfs)
    (list zfs)))

(define zfs-shepherd-service
  (match-record-lambda <zfs-configuration>
      (zfs volumes? auto-mount?)
    `(,(shepherd-service
         (provision '(zfs-import))
         (requirement '(udev))
         (documentation "Import ZFS storage pools.")
         (start
          #~(make-system-constructor
             (string-join
              (list #$(file-append zfs "/sbin/zpool") "import" "-a" "-N"))))
         (stop #~(const #f)))
      ,@(if volumes?
            (list
             (shepherd-service
               (provision '(zfs-volumes))
               (requirement '(zfs-import))
               (documentation "Wait for ZFS volume links to appear in /dev.")
               (start
                #~(make-system-constructor
                   (string-join
                    ;; TODO: Patch references within zfs package instead.
                    (list "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
                          #$(file-append zfs "/bin/zvol_wait")))))
               (stop #~(const #f))))
            '())
      ,@(if auto-mount?
            (list
             (shepherd-service
               (provision '(zfs-mount))
               (requirement '(zfs-import))
               (documentation "Mount all available ZFS file systems.")
               (start
                #~(make-system-constructor
                   (string-join
                    (list #$(file-append zfs "/sbin/zfs") "mount" "-a" "-l"))))
               (stop
                #~(make-system-destructor
                   (string-join
                    (list #$(file-append zfs "/sbin/zfs") "unmount" "-a"))))))
            '())
      ,(shepherd-service
         (provision '(file-system-zfs))
         (requirement
          `(zfs-import
            ,@(if volumes? '(zfs-volumes) '())
            ,@(if auto-mount? '(zfs-mount) '())))
         (documentation "Take care of ZFS file systems.")
         (start #~(const #t))
         (stop #~(const #f))))))

(define zfs-service-type
  (service-type
    (name 'zfs)
    (extensions
     (list (service-extension linux-loadable-module-service-type
                              zfs-linux-loadable-module-service)
           (service-extension udev-service-type
                              add-zfs-package)
           (service-extension profile-service-type
                              add-zfs-package)
           (service-extension shepherd-root-service-type
                              zfs-shepherd-service)
           (service-extension user-processes-service-type
                              (const '(file-system-zfs)))))
    (default-value (zfs-configuration))
    (description "")))

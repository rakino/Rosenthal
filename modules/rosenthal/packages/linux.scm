;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages linux)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; Guix packages
  #:use-module (gnu packages linux)
  #:export (default-initrd-configs
            cachyos-configs))

(define* (default-initrd-configs
           #:optional
           (system (or (%current-target-system)
                       (%current-system))))
  "Kernel configurations required by 'default-linux-libre-initrd-modules'."
  `("CONFIG_NLS_ISO8859_1=m"

    "CONFIG_CRYPTO_SERPENT=m"
    "CONFIG_CRYPTO_WP512=m"
    "CONFIG_DM_CRYPT=m"

    "CONFIG_BLK_DEV_NVME=m"
    "CONFIG_MMC_BLOCK=m"
    "CONFIG_SATA_AHCI=m"
    "CONFIG_USB_STORAGE=m"
    "CONFIG_USB_UAS=m"
    ,@(if (string-match "^(x86_64|i[3-6]86)-" system)
          '("CONFIG_PATA_ACPI=m"
            "CONFIG_PATA_ATIIXP=m"
            "CONFIG_SCSI_ISCI=m")
          '())

    "CONFIG_HW_RANDOM_VIRTIO=m"
    "CONFIG_SCSI_VIRTIO=m"
    "CONFIG_VIRTIO_BALLOON=m"
    "CONFIG_VIRTIO_BLK=m"
    "CONFIG_VIRTIO_CONSOLE=m"
    "CONFIG_VIRTIO_MMIO=m"
    "CONFIG_VIRTIO_NET=m"
    "CONFIG_VIRTIO_PCI=m"

    "CONFIG_HID_GENERIC=m"
    "CONFIG_USB_HID=m"
    ,@(if (target-riscv64? system)
          '()
          '("CONFIG_HID_APPLE=m"))))


;;;
;;; linux-cachyos
;;; https://github.com/CachyOS/linux-cachyos
;;;

(define* (cachyos-configs #:key (major-version "6")
                          cachy-config?
                          cpusched
                          cc-harder?
                          per-gov?
                          tcp-bbr3?
                          HZ-ticks
                          tickrate
                          preempt
                          hugepage
                          processor-opt)
  `(,@(match processor-opt
        ('generic
         '("CONFIG_GENERIC_CPU=y"
           "CONFIG_MZEN4"
           "CONFIG_X86_NATIVE_CPU"))
        ('zen4
         '("CONFIG_GENERIC_CPU"
           "CONFIG_MZEN4=y"
           "CONFIG_X86_NATIVE_CPU"))
        ('native
         '("CONFIG_GENERIC_CPU"
           "CONFIG_MZEN4"
           "CONFIG_X86_NATIVE_CPU=y"))
        (_ '()))
    ,@(if cachy-config?
          '("CONFIG_CACHY=y")
          '())
    ,@(match cpusched
        ((or 'cachyos 'bore 'hardened)
         '("CONFIG_SCHED_BORE=y"))
        ('bmq
         '("CONFIG_SCHED_ALT=y"
           "CONFIG_SCHED_BMQ=y"))
        ('eevdf
         '())
        ('rt
         '("CONFIG_PREEMPT_RT=y"))
        ('rt-bore
         '("CONFIG_SCHED_BORE=y"
           "CONFIG_PREEMPT_RT=y"))
        (_ '()))
    ,@(match HZ-ticks
        ((or 100 250 500 600 750 1000)
         `("CONFIG_HZ_300"
           ,(format #f "CONFIG_HZ_~a=y" HZ-ticks)
           ,(format #f "CONFIG_HZ=~a" HZ-ticks)))
        (300
         '("CONFIG_HZ_300=y"
           "CONFIG_HZ=300"))
        (_ '()))
    ,@(if per-gov?
          '("CONFIG_CPU_FREQ_DEFAULT_GOV_SCHEDUTIL"
            "CONFIG_CPU_FREQ_DEFAULT_GOV_PERFORMANCE=y")
          '())
    ,@(match tickrate
        ('periodic
         '("CONFIG_NO_HZ_IDLE"
           "CONFIG_NO_HZ_FULL"
           "CONFIG_NO_HZ"
           "CONFIG_NO_HZ_COMMON"
           "CONFIG_HZ_PERIODIC=y"))
        ('idle
         '("CONFIG_HZ_PERIODIC"
           "CONFIG_NO_HZ_FULL"
           "CONFIG_NO_HZ_IDLE=y"
           "CONFIG_NO_HZ"
           "CONFIG_NO_HZ_COMMON"))
        ('full
         '("CONFIG_HZ_PERIODIC"
           "CONFIG_NO_HZ_IDLE"
           "CONFIG_CONTEXT_TRACKING_FORCE"
           "CONFIG_NO_HZ_FULL=y"
           "CONFIG_NO_HZ=y"
           "CONFIG_NO_HZ_COMMON=y"
           "CONFIG_CONTEXT_TRACKING=y")))
    ,@(if (not (member cpusched '(rt rt-bore)))
          (if (version>=? major-version "7.0")
              (match preempt
                ('full
                 '("CONFIG_PREEMPT=y"
                   "CONFIG_PREEMPT_LAZY"))
                ('lazy
                 '("CONFIG_PREEMPT"
                   "CONFIG_PREEMPT_LAZY=y"))
                (_ '()))
              (match preempt
                ('full
                 '("CONFIG_PREEMPT_DYNAMIC=y"
                   "CONFIG_PREEMPT=y"
                   "CONFIG_PREEMPT_VOLUNTARY"
                   "CONFIG_PREEMPT_LAZY"
                   "CONFIG_PREEMPT_NONE"))
                ('lazy
                 '("CONFIG_PREEMPT_DYNAMIC=y"
                   "CONFIG_PREEMPT"
                   "CONFIG_PREEMPT_VOLUNTARY"
                   "CONFIG_PREEMPT_LAZY=y"
                   "CONFIG_PREEMPT_NONE"))
                ('voluntary
                 '("CONFIG_PREEMPT_DYNAMIC"
                   "CONFIG_PREEMPT=y"
                   "CONFIG_PREEMPT_VOLUNTARY=y"
                   "CONFIG_PREEMPT_LAZY"
                   "CONFIG_PREEMPT_NONE"))
                ('none
                 '("CONFIG_PREEMPT_DYNAMIC"
                   "CONFIG_PREEMPT"
                   "CONFIG_PREEMPT_VOLUNTARY"
                   "CONFIG_PREEMPT_LAZY"
                   "CONFIG_PREEMPT_NONE=y"))
                (_ '())))
          '())
    ,@(if cc-harder?
          '("CONFIG_CC_OPTIMIZE_FOR_PERFORMANCE"
            "CONFIG_CC_OPTIMIZE_FOR_PERFORMANCE_O3=y")
          '())
    ,@(if tcp-bbr3?
          '("CONFIG_TCP_CONG_CUBIC=m"
            "CONFIG_DEFAULT_CUBIC"
            "CONFIG_TCP_CONG_BBR=y"
            "CONFIG_DEFAULT_BBR=y"
            "CONFIG_DEFAULT_TCP_CONG=\"bbr\""
            "CONFIG_NET_SCH_FQ_CODEL=m"
            "CONFIG_NET_SCH_FQ=y"
            "CONFIG_DEFAULT_FQ_CODEL"
            "CONFIG_DEFAULT_FQ=y")
          '())
    ,@(match hugepage
        ('always
         '("CONFIG_TRANSPARENT_HUGEPAGE_MADVISE"
           "CONFIG_TRANSPARENT_HUGEPAGE_ALWAYS=y"))
        ('madvise
         '("CONFIG_TRANSPARENT_HUGEPAGE_ALWAYS"
           "CONFIG_TRANSPARENT_HUGEPAGE_MADVISE=y"))
        (_ '()))
    ,@(if (version>=? major-version "7.0")
          '()
          '("CONFIG_USER_NS=y"))))

(define (%kernel-config path)
  (let* ((commit "343874d509a9ff74729281fd297d8961dcedd342")
         (source
          (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://codeberg.org/hako/kernel-config.git")
                   (commit commit)))
            (file-name (string-append "kernel-config." (string-take commit 7)))
            (sha256
             (base32 "150nms11b9cdp051i7xn4x8vfj4izhhnmj0h5bj9gqk4fzxqz9n7")))))
    (file-append source path)))

(define-public linux-cachyos-lts-server
  (let* ((version "6.18.52-1")
         (kernel
          (customize-linux
           #:name "linux-cachyos-lts-server"
           #:source
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/CachyOS/linux/releases/download/cachyos-"
                   version "/cachyos-" version ".tar.gz"))
             (sha256
              (base32 "0m3rp34gfddjki662jv0mirig5rfa1chy5vfrvyzcsvvs26qpyrn")))
           #:defconfig (%kernel-config "/defconfig_server")
           #:configs
           (string-join
            (append (cachyos-configs
                     #:major-version (version-major version)
                     #:cachy-config? #f
                     #:cpusched 'eevdf
                     #:cc-harder? #t
                     #:per-gov? #f
                     #:tcp-bbr3? #t
                     #:HZ-ticks 300
                     #:tickrate 'full
                     #:preempt 'none
                     #:hugepage 'always
                     #:processor-opt 'generic)
                    (default-initrd-configs))
            "\n"))))
    (hidden-package
     (package
       (inherit kernel)
       (version version)
       (supported-systems '("x86_64-linux"))))))

(define-public linux-cachyos-bore-zen4
  (let* ((version "7.2.8-1")
         (kernel
          (customize-linux
           #:name "linux-cachyos-bore-zen4"
           #:source
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/CachyOS/linux/releases/download/cachyos-"
                   version "/cachyos-" version ".tar.gz"))
             (sha256
              (base32 "0a7kxhivrp13j50scqqvb7bnsg0dcqing30q314imhsapm1dyj7q"))
             (patches (map %kernel-config '("/patches/bore-cachy-7.2.patch"))))
           #:defconfig (%kernel-config "/defconfig_desktop")
           #:configs
           (string-join
            (append (cachyos-configs
                     #:major-version (version-major version)
                     #:cachy-config? #t
                     #:cpusched 'bore
                     #:cc-harder? #t
                     #:per-gov? #f
                     #:tcp-bbr3? #t
                     #:HZ-ticks 1000
                     #:tickrate 'full
                     #:preempt 'full
                     #:hugepage 'always
                     #:processor-opt 'zen4)
                    (default-initrd-configs))
            "\n"))))
    (hidden-package
     (package
       (inherit kernel)
       (version version)
       (supported-systems '("x86_64-linux"))))))

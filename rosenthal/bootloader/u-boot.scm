;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal bootloader u-boot)
  #:use-module (ice-9 format)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (rosenthal packages bootloaders)
  #:export (u-boot-rpi4-arm64-bootloader))

(define %rpi4-arm64-boot-config
  (plain-file "config.txt" (format #f "~
enable_uart=1
avoid_warnings=1
arm_64bit=1
kernel=u-boot.bin
")))

;; https://www.raspberrypi.com/documentation/computers/configuration.html#the-boot-folder
(define install-rpi4-arm64-bootloader
  #~(lambda (bootloader device mount-point)
      (let ((u-boot (string-append bootloader "/libexec/u-boot.bin"))
            (install-dir (string-append mount-point "/boot")))
        ;; Install raspberrypi-firmware
        ;; (for-each (lambda (file)
        ;;             (install-file
        ;;              (string-append #$raspberrypi-firmware "/" file) install-dir))
        ;;           '(;; VideoCore firmwares
        ;;             "start4.elf"        ;basic
        ;;             "start4x.elf"       ;camera drivers and codec
        ;;             "start4cd.elf"      ;cut-down
        ;;             "start4db.elf"      ;debug
        ;;             ;; Linker files
        ;;             "fixup4.dat"
        ;;             "fixup4x.dat"
        ;;             "fixup4cd.dat"
        ;;             "fixup4db.dat"
        ;;             ;; Device Tree Overlays (for Raspberry Pi's bootloader)
        ;;             "bcm2711-rpi-4-b.dtb"))
        ;; Install u-boot
        (install-file u-boot install-dir)
        ;; Raspberry Pi's bootloader configuration
        (copy-file #$%rpi4-arm64-boot-config
                   (string-append install-dir "/config.txt")))))

(define u-boot-rpi4-arm64-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-rpi-arm64)
   (installer install-rpi4-arm64-bootloader)))

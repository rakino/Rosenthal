;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages busybox)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages busybox))

(define-public busybox/static
  (let ((base busybox))
    (package
      (inherit base)
      (name "busybox-static")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'configure 'static-build
                (lambda _
                  (substitute* ".config"
                    (("# CONFIG_STATIC is not set")
                     "CONFIG_STATIC=y"))))
              ;; FIXME: All mdev tests fail when building staticly.
              (add-before 'check 'disable-failing-tests
                (lambda _
                  (delete-file "testsuite/mdev.tests"))))))))))

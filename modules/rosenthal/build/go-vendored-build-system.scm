;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal build go-vendored-build-system)
  #:use-module (guix build utils)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build go-build-system) #:prefix go:)
  #:export (%standard-phases
            go-vendored-build))

(define* (unpack-vendored-dependencies #:key native-inputs inputs #:allow-other-keys)
  (let ((vendored-dependencies
         (assoc-ref (or native-inputs inputs) "vendored-go-dependencies")))
    (unsetenv "GO111MODULE")
    (when vendored-dependencies
      (copy-recursively vendored-dependencies "vendor"))))

(define %standard-phases
  (modify-phases go:%standard-phases
    (add-after 'unpack 'unpack-vendored-dependencies unpack-vendored-dependencies)
    (replace 'unpack
      (assoc-ref gnu:%standard-phases 'unpack))
    (replace 'install-license-files
      (assoc-ref gnu:%standard-phases 'install-license-files))))

(define* (go-vendored-build #:key inputs (phases %standard-phases)
                            #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

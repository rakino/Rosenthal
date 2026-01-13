;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal))

;; Re-export commonly-used modules.

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((gnu)
        (guix utils)
        (rosenthal utils file)
        (rosenthal utils packages)
        (rosenthal utils transformations)

        (gnu system privilege)
        (rosenthal bootloader grub)
        (rosenthal bootloader uki)

        (gnu services desktop)
        (gnu services guix)
        (gnu services shepherd)
        (rosenthal services base)
        (rosenthal services desktop)

        (gnu home)
        (gnu home services)
        (gnu home services desktop)
        (gnu home services shepherd)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))

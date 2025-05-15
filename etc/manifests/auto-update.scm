;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: CC0-1.0

(use-modules (guix packages)
             (guix profiles)
             (rosenthal packages))

(define (disable-updater? p)
  (let ((properties (package-properties p)))
    (and (assq 'rosenthal-update? properties)
         (not (assq-ref properties 'rosenthal-update?)))))

(manifest (map package->manifest-entry
               (filter (negate disable-updater?)
                       (all-rosenthal-packages))))

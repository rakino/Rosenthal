;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: CC0-1.0

(use-modules (guix packages)
             (guix profiles)
             (rosenthal utils packages))

(manifest (map package->manifest-entry
               (filter (lambda (p)
                         (and (rosenthal-disable-updater? p)
                              (not (hidden-package? p))))
                       (all-rosenthal-packages))))

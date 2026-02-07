;; SPDX-License-Identifier: CC0-1.0
;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(use-modules (guix profiles)
             (rosenthal utils packages))

(manifest (map package->manifest-entry
               (filter (negate rosenthal-disable-updater?)
                       (all-rosenthal-packages))))

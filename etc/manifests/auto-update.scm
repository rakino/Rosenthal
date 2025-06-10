;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: CC0-1.0

(use-modules (guix profiles)
             (rosenthal utils packages))

(manifest (map package->manifest-entry
               (filter (negate rosenthal-disable-updater?)
                       (all-rosenthal-packages))))

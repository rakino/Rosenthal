;;; SPDX-FileCopyrightText: 2024-2025 Ludovic Courtès <ludo@gnu.org>
;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (guix memoization)
             (guix packages)
             (guix profiles)
             (guix store)
             (guix transformations)
             (rosenthal utils packages)
             ((guix import github) #:select (%github-api))
             ((guix scripts build) #:select (dependents)))

;; Bypass the GitHub updater: we'd need an API token or we would hit the rate
;; limit.
(%github-api "http://example.org")

(define latest-version
  (mlambdaq (package)
    (package-with-upstream-version package
                                   ;; Preserve patches and snippets to get
                                   ;; exactly the same as what we'd have with
                                   ;; 'guix refresh -u PACKAGE'.
                                   #:preserve-patches? #t

                                   ;; XXX: Disable source code authentication:
                                   ;; this requires a local keyring, populated
                                   ;; from key servers, but key servers may be
                                   ;; unreliable or may lack the upstream
                                   ;; keys.  Leave it up to packagers to
                                   ;; actually authenticate code and make sure
                                   ;; it matches what this manifest computed.
                                   #:authenticate? #f)))

(define updatable-packages
  (filter (negate rosenthal-disable-updater?)
          (all-rosenthal-packages)))

(manifest
 (with-store store
   (let ((update-all (package-input-rewriting
                      (map (lambda (package)
                             `(,package . ,(latest-version package)))
                           updatable-packages)
                      #:recursive? #t)))
     (map (lambda (package)
            (manifest-entry
              (inherit (package->manifest-entry
                        (update-all package)))
              (name (string-append (package-name package) "-full-upgrade"))))
          (dependents store updatable-packages 2)))))

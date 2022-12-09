;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages web)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public buku-run-dev
  (let ((revision "23")
        (commit "54fcdd77fc1e8e657b785b7d4ca8dc915e5f336b"))
    (package
      (inherit buku-run)
      (name "buku-run-dev")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/carnager/buku_run")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "079ygn39px71bypa54jn4z55iq24lxxcy7jv3ijy08iinqbfvldc")))))))

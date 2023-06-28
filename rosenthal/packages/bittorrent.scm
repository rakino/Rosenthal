;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages bittorrent)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages qt))

(define-public qbittorrent-enhanced
  (let ((base qbittorrent))
    (package
      (inherit base)
      (name "qbittorrent-enhanced")
      (version "4.5.0.10")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/c0re100/qBittorrent-Enhanced-Edition")
                      (commit (string-append "release-" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dzlf85jmzdxa4rqjzddicl02053d7bzqy5ls1cqm95jaj94qbdh"))))
      (home-page "https://github.com/c0re100/qBittorrent-Enhanced-Edition")
      (description
       "qBittorrent Enhanced is a bittorrent client based on qBittorrent with
the following features:

@itemize
@item Auto Ban Xunlei, QQ, Baidu, Xfplay, DLBT and Offline downloader
@item Auto Ban Unknown Peer from China Option (Default: OFF)
@item Auto Update Public Trackers List (Default: OFF)
@item Auto Ban BitTorrent Media Player Peer Option (Default: OFF)
@item Peer whitelist/blacklist
@end itemize\n"))))

(define-public qbittorrent-enhanced-nox
  (let ((base qbittorrent-enhanced))
    (package
      (inherit base)
      (name "qbittorrent-enhanced-nox")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags configure-flags)
          #~(append #$configure-flags
                    (list "--disable-gui")))
         ((#:phases phases)
          #~(modify-phases #$phases
              (delete 'wrap-qt)))))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "qtsvg-5"))))))

(define-public qbittorrent-enhanced-edition
  (deprecated-package "qbittorrent-enhanced-edition" qbittorrent-enhanced))

(define-public qbittorrent-enhanced-edition-nox
  (deprecated-package "qbittorrent-enhanced-edition-nox" qbittorrent-enhanced-nox))

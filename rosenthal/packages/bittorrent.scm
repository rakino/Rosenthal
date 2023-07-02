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
      (version "4.5.4.10")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/c0re100/qBittorrent-Enhanced-Edition")
                      (commit (string-append "release-" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qwk69mgcyh7fij4nsi4ndd17aa61p2c6cxn9l402w4cf1dy6hfs"))))
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

(define-public qbittorrent-enhanced-edition
  (deprecated-package "qbittorrent-enhanced-edition" qbittorrent-enhanced))

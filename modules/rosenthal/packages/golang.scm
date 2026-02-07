;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages golang)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils cargo)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  ;; Guix build systems
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  ;; Guix packages
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages version-control))

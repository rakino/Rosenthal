;;; -*- lexical-binding: t -*-
;;; SPDX-FileCopyrightText: DogLooksGood <doglooksgood@hotmail.com>
;;; SPDX-FileCopyrightText: 2022 Nyk Ma <i@nyk.ma>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;; https://github.com/DogLooksGood/meomacs/blob/master/laf.org
;; https://github.com/nykma/nema/blob/develop/my-sample/font.el

(defvar nema--font-size 12 "Font size")
(defvar nema-fonts '((mono     . "monospace")
                     (sans     . "sans")
                     (serif    . "serif")
                     (cjk      . "Noto Serif CJK SC")
                     (symbol   . "Noto Color Emoji")
                     (modeline . "Source Serif 4"))
  "Fonts to use.")

(defun nema//get-font-family (key)
  (alist-get key nema-fonts))

(defun nema//generate-font-spec (key)
  (format "%s-%d"
          (nema//get-font-family key)
          nema--font-size))

(defun nema//load-base-font ()
  "Load the default font for ascii characters."
  (let* ((font-spec (nema//generate-font-spec 'mono)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist (cons 'font font-spec))))

(defun nema//load-face-font ()
  "Load fonts used in faces.

This function must be called after frame creation."
  (let ((mono (nema//generate-font-spec 'mono))
        (sans (nema//generate-font-spec 'sans))
        (serif (nema//generate-font-spec 'sans-serif))
        (modeline (nema//generate-font-spec 'modeline)))
    (set-face-attribute 'variable-pitch nil :font sans)
    (set-face-attribute 'variable-pitch-text nil :font serif)
    (set-face-attribute 'fixed-pitch nil :font mono)
    (set-face-attribute 'fixed-pitch-serif nil :font mono)
    (set-face-attribute 'mode-line nil :font modeline)
    (set-face-attribute 'mode-line-inactive nil :font modeline)))

(defun nema//load-ext-font ()
  "Load fonts used for non-ascii characters.

This function must be called after frame creation."
  (let ((font (frame-parameter nil 'font))
        (font-spec-cjk (font-spec :family (nema//get-font-family 'cjk)))
        (font-spec-symbol (font-spec :family (nema//get-font-family 'symbol))))
    (dolist (charset '(kana han hangul cjk-misc bopomofo))
      (set-fontset-font font charset font-spec-cjk))
    (set-fontset-font font 'symbol font-spec-symbol)))

(defun nema/load-font ()
  "Load all font configuration."
  (interactive)
  (when (display-graphic-p)
    (nema//load-base-font)
    (nema//load-ext-font)
    (nema//load-face-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'nema/load-font)
  ;; Else: not in daemon
  (add-hook 'after-init-hook #'nema/load-font))

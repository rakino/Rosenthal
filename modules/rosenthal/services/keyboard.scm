;;; SPDX-FileCopyrightText: 2021 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services keyboard)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home services)
  #:export (set-xkb-variables
            home-keyboard-service-type))

(define (set-xkb-variables layout)
  (if layout
      `(("XKB_DEFAULT_LAYOUT" . ,(keyboard-layout-name layout))
	("XKB_DEFAULT_VARIANT" . ,(keyboard-layout-variant layout))
	("XKB_DEFAULT_OPTIONS" . ,(string-join
				   (keyboard-layout-options layout) ","))
	("XKB_DEFAULT_MODEL" . ,(keyboard-layout-model layout)))
      '()))

(define home-keyboard-service-type
  (service-type (name 'home-keyboard)
                (extensions
                 (list (service-extension
			home-environment-variables-service-type
			set-xkb-variables)))
                (default-value #f)
                (description "Set layouts by configuring XKB_*
environment variables.  Service accepts an instance of
@code{keyboard-layout} from @code{(gnu system keyboard)}.")))

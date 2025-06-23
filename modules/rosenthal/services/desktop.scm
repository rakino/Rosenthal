;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services desktop)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (rosenthal utils file)
  #:use-module (rosenthal utils packages)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (rosenthal services base)

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)

  #:use-module (rosenthal packages xorg)

  #:export (%rosenthal-example-emacs-fonts
            %rosenthal-example-emacs-init

            home-blueman-applet-configuration
            home-blueman-applet-service-type

            home-fcitx5-configuration
            home-fcitx5-service-type

            home-mako-configuration
            home-mako-service-type

            home-network-manager-applet-configuration
            home-network-manager-applet-service-type

            home-niri-configuration
            home-niri-service-type

            home-rofi-configuration
            home-rofi-service-type

            home-swaybg-configuration
            home-swaybg-service-type

            home-theme-configuration
            home-theme-service-type

            home-waybar-configuration
            home-waybar-service-type

            %rosenthal-desktop-services
            %rosenthal-desktop-home-services))


;;;
;;; Configuration file presets.
;;;

(define %rosenthal-example-emacs-fonts
  (local-file "../examples/emacs/fonts.el"))

(define %rosenthal-example-emacs-init
  (local-file "../examples/emacs/init.el"))


;;;
;;; Blueman
;;;

(define-record-type* <home-blueman-applet-configuration>
  home-blueman-applet-configuration
  make-home-blueman-applet-configuration
  home-blueman-applet-configuration?
  this-home-blueman-applet-configuration
  (blueman home-blueman-applet-configuration-blueman
           (default (spec->pkg+out "blueman"))))

(define home-blueman-applet-shepherd-service
  (match-record-lambda <home-blueman-applet-configuration>
      (blueman)
    (list (shepherd-service
            (documentation "Start blueman applet.")
            (provision '(blueman-applet))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append blueman "/bin/blueman-applet"))))
            (stop #~(make-kill-destructor))))))

(define home-blueman-applet-service-type
  (service-type
    (name 'blueman-applet)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-blueman-applet-configuration-blueman))
           (service-extension home-shepherd-service-type
                              home-blueman-applet-shepherd-service)))
    (default-value (home-blueman-applet-configuration))
    (description "Run blueman applet, a tray applet for managing bluetooth.")))


;;;
;;; Fcitx5
;;;

(define-record-type* <home-fcitx5-configuration>
  home-fcitx5-configuration
  make-home-fcitx5-configuration
  home-fcitx5-configuration?
  this-home-fcitx5-configuration
  (fcitx5                home-fcitx5-configuration-fcitx5
                         (default (spec->pkg   "fcitx5")))
  (utilities             home-fcitx5-configuration-utilities
                         (default (specs->pkgs "fcitx5-configtool")))
  (themes                home-fcitx5-configuration-themes
                         (default '()))
  (input-method-editors  home-fcitx5-configuration-input-method-editors
                         (default '()))
  (gtk-im-module?        home-fcitx5-configuration-gtk-im-module?
                         (default #f))
  (qt-im-module?         home-fcitx5-configuration-qt-im-module?
                         (default #f))
  (xim?                  home-fcitx5-configuration-xim?
                         (default #t))
  ;; Extensions
  (environment-variables home-fcitx5-configuration-environment-variables
                         (default (%home-fcitx5-environment-variables
                                   this-home-fcitx5-configuration))
                         (thunked))
  (profile               home-fcitx5-configuration-profile
                         (default (%home-fcitx5-profile
                                   this-home-fcitx5-configuration))
                         (thunked))
  (shepherd              home-fcitx5-configuration-shepherd
                         (default (%home-fcitx5-shepherd
                                   this-home-fcitx5-configuration))
                         (thunked)))

(define %home-fcitx5-environment-variables
  (match-record-lambda <home-fcitx5-configuration>
      (gtk-im-module? qt-im-module? xim?)
    `(,@(if gtk-im-module?
            '(("GTK_IM_MODULE" . "fcitx"))
            '())
      ,@(if qt-im-module?
            '(("QT_IM_MODULE" . "fcitx"))
            '())
      ,@(if xim?
            '(("XMODIFIERS" . "@im=fcitx"))
            '()))))

(define %home-fcitx5-profile
  (match-record-lambda <home-fcitx5-configuration>
      (fcitx5 utilities themes input-method-editors gtk-im-module? qt-im-module?)
    (append (list fcitx5)
            utilities
            themes
            input-method-editors
            (if gtk-im-module?
                (list (spec->pkg "fcitx5-gtk"))
                '())
            (if qt-im-module?
                (list (spec->pkg "fcitx5-qt"))
                '()))))

(define %home-fcitx5-shepherd
  (match-record-lambda <home-fcitx5-configuration>
      (fcitx5)
    (list (shepherd-service
            (documentation "Start fcitx5.")
            (provision '(fcitx5))
            (requirement '(dbus))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append fcitx5 "/bin/fcitx5"))))
            (stop #~(make-kill-destructor))))))

(define home-fcitx5-service-type
  (service-type
    (name 'fcitx5)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              home-fcitx5-configuration-environment-variables)
           (service-extension home-profile-service-type
                              home-fcitx5-configuration-profile)
           (service-extension home-shepherd-service-type
                              home-fcitx5-configuration-shepherd)))
    (default-value (home-fcitx5-configuration))
    (description "Run fcitx5, an input method framework.")))


;;;
;;; mako
;;;

(define %rosenthal-example-mako-config
  (local-file "../examples/mako.conf"))

(define-record-type* <home-mako-configuration>
  home-mako-configuration
  make-home-mako-configuration
  home-mako-configuration?
  this-home-mako-configuration
  (mako       home-mako-configuration-mako
              (default (spec->pkg "mako")))
  (config     home-mako-configuration-config
              (default %rosenthal-example-mako-config))
  ;; Extensions.
  (xdg-config home-mako-configuration-xdg-config
              (default (%home-mako-xdg-config
                        this-home-mako-configuration))
              (thunked))
  (shepherd   home-mako-configuration-shepherd
              (default (%home-mako-shepherd
                        this-home-mako-configuration))
              (thunked)))

(define %home-mako-xdg-config
  (match-record-lambda <home-mako-configuration>
      (config)
    `(("mako/config" ,config))))

(define %home-mako-shepherd
  (match-record-lambda <home-mako-configuration>
      (mako)
    (list (shepherd-service
            (documentation "Start mako.")
            (provision '(mako))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append mako "/bin/mako"))))
            (stop #~(make-kill-destructor))))))

(define home-mako-service-type
  (service-type
    (name 'mako)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              home-mako-configuration-xdg-config)
           (service-extension home-shepherd-service-type
                              home-mako-configuration-shepherd)))
    (default-value (home-mako-configuration))
    (description "Run mako, a notification daemon.")))


;;;
;;; network-manager-applet
;;;

(define-record-type* <home-network-manager-applet-configuration>
  home-network-manager-applet-configuration
  make-home-network-manager-applet-configuration
  home-network-manager-applet-configuration?
  this-home-network-manager-applet-configuration
  (network-manager-applet home-network-manager-applet-configuration-network-manager-applet
                          (default (spec->pkg "network-manager-applet"))))

(define home-network-manager-applet-shepherd-service
  (match-record-lambda <home-network-manager-applet-configuration>
      (network-manager-applet)
    (list (shepherd-service
            (documentation "Start network manager applet.")
            (provision '(network-manager-applet))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append network-manager-applet "/bin/nm-applet"))))
            (stop #~(make-kill-destructor))))))

(define home-network-manager-applet-service-type
  (service-type
    (name 'network-manager-applet)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-network-manager-applet-configuration-network-manager-applet))
           (service-extension home-shepherd-service-type
                              home-network-manager-applet-shepherd-service)))
    (default-value (home-network-manager-applet-configuration))
    (description "Run nm-applet, a tray applet for managing networks.")))


;;;
;;; niri
;;;

(define %rosenthal-example-niri-config
  (computed-substitution-with-inputs "niri.kdl"
    (local-file "../examples/niri.kdl")
    (cons xwayland-satellite
          (specs->pkgs "alacritty"
                       "guix-backgrounds"
                       "light"
                       "rofi-wayland"
                       "wireplumber"))))

(define-record-type* <home-niri-configuration>
  home-niri-configuration
  make-home-niri-configuration
  home-niri-configuration?
  this-home-niri-configuration
  (config     home-niri-configuration-config
              (default %rosenthal-example-niri-config))
  ;; Extension.
  (xdg-config home-niri-configuration-xdg-config
              (default (%home-niri-xdg-config
                        this-home-niri-configuration))
              (thunked)))

(define %home-niri-xdg-config
  (match-record-lambda <home-niri-configuration>
      (config)
    `(("niri/config.kdl" ,config))))

(define home-niri-service-type
  (service-type
    (name 'niri)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              home-niri-configuration-xdg-config)))
    (default-value (home-niri-configuration))
    (description
     "Set up configuration file for niri, a scrollable-tiling Wayland
compositor.")))


;;;
;;; rofi
;;;

(define %rosenthal-example-rofi-config
  (mixed-text-file "rofi.rasi" "\
configuration {
    icon-theme: \"Qogir\";
}
@theme \"" (spec->pkg "rofi-wayland") "/share/rofi/themes/fullscreen-preview.rasi\"\n"))


(define-record-type* <home-rofi-configuration>
  home-rofi-configuration
  make-home-rofi-configuration
  home-rofi-configuration?
  this-home-rofi-configuration
  (config     home-rofi-configuration-config
              (default %rosenthal-example-rofi-config))
  ;; Extension.
  (xdg-config home-rofi-configuration-xdg-config
              (default (%home-rofi-xdg-config
                        this-home-rofi-configuration))
              (thunked)))

(define %home-rofi-xdg-config
  (match-record-lambda <home-rofi-configuration>
      (config)
    `(("rofi/config.rasi" ,config))))

(define home-rofi-service-type
  (service-type
    (name 'rofi)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              home-rofi-configuration-xdg-config)))
    (default-value (home-rofi-configuration))
    (description
     "Set up configuration file for rofi, an application launcher.")))


;;;
;;; swaybg
;;;

(define-record-type* <home-swaybg-configuration>
  home-swaybg-configuration
  make-home-swaybg-configuration
  home-swaybg-configuration?
  this-home-swaybg-configuration
  (swaybg     home-swaybg-configuration-swaybg
              (default (spec->pkg "swaybg")))
  (background home-swaybg-configuration-background
              (default (local-file "../examples/wallpaper.jpg")))
  ;; Extensions.
  (shepherd   home-swaybg-configuration-shepherd
              (default (%home-swaybg-shepherd
                        this-home-swaybg-configuration))
              (thunked)))

(define %home-swaybg-shepherd
  (match-record-lambda <home-swaybg-configuration>
      (swaybg background)
    (list (shepherd-service
            (documentation "Start swaybg.")
            (provision '(swaybg))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append swaybg "/bin/swaybg") "--mode" "fill"
                      "--image" #$background)))
            (stop #~(make-kill-destructor))))))

(define home-swaybg-service-type
  (service-type
    (name 'swaybg)
    (extensions
     (list (service-extension home-shepherd-service-type
                              home-swaybg-configuration-shepherd)))
    (default-value (home-swaybg-configuration))
    (description
     "Run swaybg, a screen wallpaper utility for Wayland compositors.")))


;;;
;;; theme
;;;

(define-record-type* <home-theme-configuration>
  home-theme-configuration
  make-home-theme-configuration
  home-theme-configuration?
  this-home-theme-configuration
  (packages     home-theme-configuration-packages
                (default '()))
  (icon-theme   home-theme-configuration-icon-theme
                (default "Adwaita"))
  (cursor-theme home-theme-configuration-cursor-theme
                (default "Adwaita"))
  (cursor-size  home-theme-configuration-cursor-size
                (default 24))
  (key-theme    home-theme-configuration-key-theme
                (default "Emacs"))
  ;; Extensions.
  (environment-variables home-theme-configuration-environment-variables
                         (default (%home-theme-environment-variables
                                   this-home-theme-configuration))
                         (thunked))
  (profile               home-theme-configuration-profile
                         (default (%home-theme-profile
                                   this-home-theme-configuration))
                         (thunked))
  (files                 home-theme-configuration-files
                         (default (%home-theme-files
                                   this-home-theme-configuration))
                         (thunked))
  (xdg-config            home-theme-configuration-xdg-config
                         (default (%home-theme-xdg-config
                                   this-home-theme-configuration))
                         (thunked)))

(define (%home-theme-environment-variables _)
  '(("QT_QPA_PLATFORMTHEME" . "gtk3")
    ("QT_WAYLAND_DECORATION" . "adwaita")))

(define %home-theme-profile
  (match-record-lambda <home-theme-configuration>
      (packages)
    (append (specs->pkgs "adwaita-icon-theme"
                         "hicolor-icon-theme"
                         "qtwayland")
            packages)))

(define %home-theme-files
  (match-record-lambda <home-theme-configuration>
      (icon-theme)
    `((".icons/default/index.theme"
       ,(plain-file "index.theme"
          (format #f "~
[icon theme]
Inherits = ~a~%"
                  icon-theme))))))

(define %home-theme-xdg-config
  (match-record-lambda <home-theme-configuration>
      (icon-theme cursor-theme cursor-size key-theme)
    `(("gtk-3.0/settings.ini"
       ,(plain-file "settings.ini"
          (format #f "~
[Settings]
gtk-theme-name = Adwaita
gtk-icon-theme-name = ~a
gtk-font-name = Sans 11
gtk-cursor-theme-name = ~a
gtk-cursor-theme-size = ~a
gtk-key-theme-name = ~a~%"
                  icon-theme cursor-theme cursor-size key-theme))))))

(define home-theme-service-type
  (service-type
    (name 'theme)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              home-theme-configuration-environment-variables)
           (service-extension home-profile-service-type
                              home-theme-configuration-profile)
           (service-extension home-files-service-type
                              home-theme-configuration-files)
           (service-extension home-xdg-configuration-files-service-type
                              home-theme-configuration-xdg-config)))
    (default-value (home-theme-configuration))
    (description "Set up desktop themes.")))


;;;
;;; waybar
;;;

(define %rosenthal-example-waybar-config
  (computed-substitution-with-inputs "config.jsonc"
    (local-file "../examples/waybar/config.jsonc")
    (specs->pkgs "light" "pavucontrol" "wireplumber")))

(define %rosenthal-example-waybar-style
  (local-file "../examples/waybar/style.css"))

(define-record-type* <home-waybar-configuration>
  home-waybar-configuration
  make-home-waybar-configuration
  home-waybar-configuration?
  this-home-waybar-configuration
  (waybar     home-waybar-configuration-waybar
              (default (spec->pkg "waybar")))
  (config     home-waybar-configuration-config
              (default %rosenthal-example-waybar-config))
  (style      home-waybar-configuration-style
              (default %rosenthal-example-waybar-style))
  ;; Extensions.
  (xdg-config home-waybar-configuration-xdg-config
              (default (%home-waybar-xdg-config
                        this-home-waybar-configuration))
              (thunked))
  (shepherd   home-waybar-configuration-shepherd
              (default (%home-waybar-shepherd
                        this-home-waybar-configuration))
              (thunked)))

(define %home-waybar-xdg-config
  (match-record-lambda <home-waybar-configuration>
      (config style)
    `(("waybar/config.jsonc" ,config)
      ("waybar/style.css" ,style))))

(define %home-waybar-shepherd
  (match-record-lambda <home-waybar-configuration>
      (waybar)
    (list (shepherd-service
            (documentation "Start waybar.")
            (provision '(waybar))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append waybar "/bin/waybar"))))
            (stop #~(make-kill-destructor))))))

(define home-waybar-service-type
  (service-type
    (name 'waybar)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              home-waybar-configuration-xdg-config)
           (service-extension home-shepherd-service-type
                              home-waybar-configuration-shepherd)))
    (default-value (home-waybar-configuration))
    (description "Run waybar, a status bar for Wayland compositors.")))


;;;
;;; Service presets.
;;;

(define* (rosenthal-desktop-services-for-system
          #:optional (system (or (%current-target-system)
                                 (%current-system))))
  (define %display-manager-service-type
    (if (string-prefix? "x86_64" system)
        gdm-service-type
        sddm-service-type))

  (define %term-font
    (file-append (spec->pkg "font-terminus") "/share/consolefonts/ter-132n"))

  (cons* (service greetd-service-type
           (greetd-configuration
             (greeter-supplementary-groups '("video" "input"))
             (terminals
              (map (lambda (x)
                     (greetd-terminal-configuration
                       (terminal-vt (number->string x))
                       (terminal-switch (eqv? 1 x))
                       (default-session-command
                         (cond
                          ((eqv? 1 x)
                           (greetd-tuigreet-session))
                          (else
                           (greetd-agreety-session
                             (command
                              (greetd-user-session
                                (command #~(getenv "SHELL"))))))))))
                   (iota 6 1)))))

         (service bluetooth-service-type
           (bluetooth-configuration
             (auto-enable? #t)))

         ;; Screen lockers for Wayland environment.  No dependencies are pulled
         ;; in since we're using empty files.
         (service screen-locker-service-type
           (screen-locker-configuration
             (name "swaylock")
             (program (plain-file "empty" "")) ;Not used.
             (using-setuid? #f)))
         (service screen-locker-service-type
           (screen-locker-configuration
             (name "waylock")
             (program (plain-file "empty" "")) ;Not used.
             (using-setuid? #f)))

         ;; Add udev rules for backlight control.
         (simple-service 'backlight udev-service-type (specs->pkgs "light"))

         (modify-services %desktop-services
           (delete mingetty-service-type)
           (delete %display-manager-service-type)
           (delete screen-locker-service-type)

           ;; Use a font suitable for HiDPI monitors.
           (console-font-service-type
            _ => (map (lambda (num)
                        (cons (string-append "tty" (number->string num))
                              %term-font))
                      (iota 6 1))))))

(define-syntax %rosenthal-desktop-services
  (identifier-syntax (rosenthal-desktop-services-for-system)))

(define %rosenthal-desktop-home-services
  (cons* (service home-files-service-type
           `((".guile" ,%default-dotguile)))

         (service home-xdg-configuration-files-service-type
           `(("gdb/gdbinit" ,%default-gdbinit)
             ("nano/nanorc" ,%default-nanorc)))

         (service home-shepherd-service-type
           (home-shepherd-configuration
             ;; Start by WM to inherit environment variables for graphical session.
             (auto-start? #f)
             (daemonize? #f)))

         (service home-dbus-service-type)

         (service home-pipewire-service-type)

         (service home-blueman-applet-service-type)
         (service home-network-manager-applet-service-type)

         %base-home-services))

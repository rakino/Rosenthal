;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services desktop)
  ;; Utilities
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (rosenthal utils file)
  #:use-module (rosenthal utils packages)
  ;; Guix build systems
  #:use-module (guix build-system copy)
  ;; Guix System
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (rosenthal services base)
  ;; Guix Home - services
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  ;; Guix packages
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (rosenthal packages wm)
  #:export (home-blueman-applet-configuration
            home-blueman-applet-service-type

            home-fcitx5-configuration
            home-fcitx5-service-type

            home-mako-configuration
            home-mako-service-type

            home-network-manager-applet-configuration
            home-network-manager-applet-service-type

            home-niri-configuration
            home-niri-service-type

            home-noctalia-shell-configuration
            home-noctalia-shell-service-type

            home-polkit-gnome-service-type

            home-rofi-configuration
            home-rofi-service-type

            home-swaybg-configuration
            home-swaybg-service-type

            home-theme-configuration
            home-theme-service-type

            home-waybar-configuration
            home-waybar-service-type

            %rosenthal-skeletons
            %rosenthal-desktop-services/gdm
            %rosenthal-desktop-services/tuigreet

            %rosenthal-desktop-services-gdm ;deprecated
            %rosenthal-desktop-services     ;deprecated

            %rosenthal-desktop-home-services))


;;;
;;; Blueman
;;;

(define-configuration/no-serialization home-blueman-applet-configuration
  (blueman
   (file-like blueman)
   ""))

(define %home-blueman-applet-shepherd
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
    (name 'home-blueman-applet)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-blueman-applet-configuration-blueman))
           (service-extension home-shepherd-service-type
                              %home-blueman-applet-shepherd)))
    (default-value (home-blueman-applet-configuration))
    (description "Run blueman applet, a tray applet for managing bluetooth.")))


;;;
;;; Fcitx5
;;;

(define list-of-file-likes?
  (list-of file-like?))
(define-maybe file-like)

(define-configuration/no-serialization home-fcitx5-configuration
  (fcitx5
   (file-like fcitx5)
   "")
  (utilities
   (list-of-file-likes (list fcitx5-configtool))
   "")
  (themes
   (list-of-file-likes '())
   "")
  (input-method-editors
   (list-of-file-likes '())
   "")
  (gtk-im-module?
   (boolean #f)
   "")
  (qt-im-module?
   (boolean #f)
   "")
  (xim?
   (boolean #t)
   ""))

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
                (list fcitx5-gtk)
                '())
            (if qt-im-module?
                (list fcitx5-qt)
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
    (name 'home-fcitx5)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              %home-fcitx5-environment-variables)
           (service-extension home-profile-service-type
                              %home-fcitx5-profile)
           (service-extension home-shepherd-service-type
                              %home-fcitx5-shepherd)))
    (default-value (home-fcitx5-configuration))
    (description "Run fcitx5, an input method framework.")))


;;;
;;; mako
;;;

(define-configuration/no-serialization home-mako-configuration
  (mako
   (file-like mako)
   "")
  (config
   maybe-file-like
   "")
  )

(define %home-mako-xdg-config
  (match-record-lambda <home-mako-configuration>
      (config)
    (if (maybe-value-set? config)
        `(("mako/config" ,config))
        '())))

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
    (name 'home-mako)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-mako-xdg-config)
           (service-extension home-shepherd-service-type
                              %home-mako-shepherd)))
    (default-value (home-mako-configuration))
    (description "Run mako, a notification daemon.")))


;;;
;;; network-manager-applet
;;;

(define-configuration/no-serialization home-network-manager-applet-configuration
  (network-manager-applet
   (file-like network-manager-applet)
   ""))

(define %home-network-manager-applet-shepherd
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
    (name 'home-network-manager-applet)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-network-manager-applet-configuration-network-manager-applet))
           (service-extension home-shepherd-service-type
                              %home-network-manager-applet-shepherd)))
    (default-value (home-network-manager-applet-configuration))
    (description "Run nm-applet, a tray applet for managing networks.")))


;;;
;;; niri
;;;

(define-configuration/no-serialization home-niri-configuration
  (config
   maybe-file-like
   ""))

(define %home-niri-xdg-config
  (match-record-lambda <home-niri-configuration>
      (config)
    (if (maybe-value-set? config)
        `(("niri/config.kdl" ,config))
        '())))

(define home-niri-service-type
  (service-type
    (name 'home-niri)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-niri-xdg-config)))
    (default-value (home-niri-configuration))
    (description
     "Set up configuration file for niri, a scrollable-tiling Wayland
compositor.")))


;;;
;;; Noctalia
;;;

(define-configuration/no-serialization home-noctalia-shell-configuration
  (noctalia-shell
   (file-like noctalia-shell)
   "File-like object to provide @command{/bin/noctalia-shell}."))

(define %home-noctalia-shell-shepherd
  (match-record-lambda <home-noctalia-shell-configuration>
      (noctalia-shell)
    (list (shepherd-service
            (documentation "Start noctalia-shell.")
            (provision '(noctalia-shell))
            (modules '((shepherd support)))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append noctalia-shell "/bin/noctalia-shell"))
                #:log-file (in-vicinity %user-log-dir "noctalia-shell.log")))
            (stop #~(make-kill-destructor))))))

(define home-noctalia-shell-service-type
  (service-type
    (name 'home-noctalia-shell)
    (extensions
     (list (service-extension home-shepherd-service-type
                              %home-noctalia-shell-shepherd)
           (service-extension home-profile-service-type
                              (compose list home-noctalia-shell-configuration-noctalia-shell))))
    (default-value (home-noctalia-shell-configuration))
    (description "")))


;;;
;;; polkit-gnome
;;;

(define (%home-polkit-gnome-shepherd _)
  (list (shepherd-service
          (provision '(polkit-gnome))
          (start
           #~(make-forkexec-constructor
              (list #$(file-append polkit-gnome "/libexec/polkit-gnome-authentication-agent-1"))))
          (stop #~(make-kill-destructor)))))

(define home-polkit-gnome-service-type
  (service-type
    (name 'home-polkit-gnome)
    (extensions
     (list (service-extension home-shepherd-service-type
                              %home-polkit-gnome-shepherd)))
    (default-value #f)
    (description "")))


;;;
;;; rofi
;;;

(define-configuration/no-serialization home-rofi-configuration
  (config
   maybe-file-like
   ""))

(define %home-rofi-xdg-config
  (match-record-lambda <home-rofi-configuration>
      (config)
    (if (maybe-value-set? config)
        `(("rofi/config.rasi" ,config))
        '())))

(define home-rofi-service-type
  (service-type
    (name 'home-rofi)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-rofi-xdg-config)))
    (default-value (home-rofi-configuration))
    (description
     "Set up configuration file for rofi, an application launcher.")))


;;;
;;; swaybg
;;;

(define-configuration/no-serialization home-swaybg-configuration
  (swaybg
   (file-like swaybg)
   "")
  (background
   (file-like (local-file "../examples/wallpaper.jpg"))
   ""))

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
    (name 'home-swaybg)
    (extensions
     (list (service-extension home-shepherd-service-type
                              %home-swaybg-shepherd)))
    (default-value (home-swaybg-configuration))
    (description
     "Run swaybg, a screen wallpaper utility for Wayland compositors.")))


;;;
;;; theme
;;;

(define-configuration/no-serialization home-theme-configuration
  (packages
   (list-of-file-likes '())
   "")
  (icon-theme
   (string "Adwaita")
   "")
  (font
   (string "Sans")
   "")
  (cursor-theme
   (string "Adwaita")
   "")
  (cursor-size
   (number 24)
   "")
  (key-theme
   (string "Default")
   ""))

(define (%home-theme-environment-variables _)
  '(("QT_QPA_PLATFORMTHEME" . "gtk3")
    ("QT_WAYLAND_DECORATION" . "adwaita")))

(define %home-theme-profile
  (match-record-lambda <home-theme-configuration>
      (packages)
    (cons* adwaita-icon-theme
           hicolor-icon-theme
           qtwayland
           packages)))

(define %home-theme-files
  (match-record-lambda <home-theme-configuration>
      (icon-theme)
    `((".icons/default/index.theme"
       ,(plain-file "index.theme"
          (format #f "\
[icon theme]
Inherits = ~a~%"
                  icon-theme))))))

(define %home-theme-xdg-config
  (match-record-lambda <home-theme-configuration>
      (icon-theme font cursor-theme cursor-size key-theme)
    `(("gtk-3.0/settings.ini"
       ,(plain-file "settings.ini"
          (format #f "\
[Settings]
gtk-theme-name = Adwaita
gtk-icon-theme-name = ~a
gtk-font-name = ~a
gtk-cursor-theme-name = ~a
gtk-cursor-theme-size = ~a
gtk-key-theme-name = ~a~%"
                  icon-theme font cursor-theme cursor-size key-theme))))))

(define home-theme-service-type
  (service-type
    (name 'home-theme)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              %home-theme-environment-variables)
           (service-extension home-profile-service-type
                              %home-theme-profile)
           (service-extension home-files-service-type
                              %home-theme-files)
           (service-extension home-xdg-configuration-files-service-type
                              %home-theme-xdg-config)))
    (default-value (home-theme-configuration))
    (description "Set up desktop themes.")))


;;;
;;; waybar
;;;

(define-configuration/no-serialization home-waybar-configuration
  (waybar
   (file-like waybar)
   "")
  (config
   maybe-file-like
   "")
  (style
   maybe-file-like
   ""))

(define %home-waybar-xdg-config
  (match-record-lambda <home-waybar-configuration>
      (config style)
    `(,@(if (maybe-value-set? config)
            `(("waybar/config.jsonc" ,config))
            '())
      ,@(if (maybe-value-set? style)
            `(("waybar/style.css" ,style))
            '()))))

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
    (name 'home-waybar)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-waybar-xdg-config)
           (service-extension home-shepherd-service-type
                              %home-waybar-shepherd)))
    (default-value (home-waybar-configuration))
    (description "Run waybar, a status bar for Wayland compositors.")))


;;;
;;; Configuration file presets.
;;;

(define %rosenthal-skeletons
  `((".config/emacs/init.el"
     ,(local-file "../examples/emacs/init.el"))
    (".config/emacs/init-fonts.el"
     ,(local-file "../examples/emacs/init-fonts.el"))
    (".config/emacs/init-interface.el"
     ,(local-file "../examples/emacs/init-interface.el"))
    (".config/emacs/init-editing.el"
     ,(local-file "../examples/emacs/init-editing.el"))
    (".config/emacs/init-miscellaneous.el"
     ,(local-file "../examples/emacs/init-miscellaneous.el"))

    (".config/autostart/shepherd.desktop"
     ,(local-file "../examples/dot-config/autostart/shepherd.desktop"))
    (".config/niri/config.kdl"
     ,(local-file "../examples/dot-config/niri/config.kdl"))
    (".config/noctalia/settings.json"
     ,(local-file "../examples/dot-config/noctalia/settings.json"))
    (".config/wezterm/wezterm.lua"
     ,(local-file "../examples/dot-config/wezterm/wezterm.lua"))
    (".config/xfce4/helpers.rc"
     ,(local-file "../examples/dot-config/xfce4/helpers.rc"))
    ;; Prevent Noctalia shell initial screen.
    (".cache/noctalia/shell-state.json"
     ,(local-file "../examples/dot-cache/noctalia/shell-state.json"))

    ;; Selected from the default skeletons.
    (".config/gdb/gdbinit" ,%default-gdbinit)
    (".config/nano/nanorc" ,%default-nanorc)
    (".guile" ,%default-dotguile)))


;;;
;;; Service presets.
;;;

(define* (base-rosenthal-desktop-services
          #:optional (system (or (%current-target-system)
                                 (%current-system))))

  (define %display-manager-service-type
    (if (target-64bit? system)
        gdm-service-type
        sddm-service-type))

  (cons* (service bluetooth-service-type
           (bluetooth-configuration
             (auto-enable? #t)))

         (service gvfs-service-type)

         (service power-profiles-daemon-service-type)

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
         (simple-service 'backlight udev-service-type (list light))

         (modify-services %desktop-services
           (delete %display-manager-service-type)
           ;; Use a font suitable for HiDPI monitors.
           (console-font-service-type
            _ => (map (lambda (num)
                        (cons (string-append "tty" (number->string num))
                              (file-append font-terminus
                                           "/share/consolefonts/ter-132n")))
                      (iota 6 1))))))

(define-syntax %rosenthal-desktop-services/base
  (identifier-syntax (base-rosenthal-desktop-services)))

(define %rosenthal-desktop-services/gdm
  (cons* (service gdm-service-type)
         %rosenthal-desktop-services/base))

(define %rosenthal-desktop-services/tuigreet
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
         (modify-services %rosenthal-desktop-services/base
           (delete mingetty-service-type))))

(define-deprecated %rosenthal-desktop-services-gdm %rosenthal-desktop-services/gdm)
(define-deprecated %rosenthal-desktop-services %rosenthal-desktop-services/tuigreet)

(define %rosenthal-desktop-home-services
  (cons* (service home-shepherd-service-type
           (home-shepherd-configuration
             ;; Start by WM to inherit environment variables for graphical session.
             (auto-start? #f)
             (daemonize? #f)))

         ;; NOTE: The environment variable set by ‘home-dbus-service-type’ will
         ;; prevent GNOME from starting when using above Shepherd configuration.
         ;; Replace ‘home-dbus-service-type’, expecting the session bus will be
         ;; started elsewhere.  See also:
         ;; https://codeberg.org/guix/guix/issues/5899#issuecomment-10208485
         (simple-service 'dbus home-shepherd-service-type
           (list (shepherd-service
                   (provision '(dbus))
                   (start #~(const #t))
                   (stop #~(const #f)))))

         (service home-pipewire-service-type)

         %base-home-services))
